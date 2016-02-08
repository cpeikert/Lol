{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, RoleAnnotations, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | An implementation of modular arithmetic, i.e., the ring Zq.

module Crypto.Lol.Types.ZqBasic
( ZqBasic -- export the type, but not the constructor (for safety)
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude    as LP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZPP

import Math.NumberTheory.Primes.Factorisation
import Math.NumberTheory.Primes.Testing

import Control.Applicative
import Control.Arrow
import Control.DeepSeq        (NFData)
import Data.Coerce
import Data.Maybe
import NumericPrelude.Numeric as NP (round)
import System.Random
import Test.QuickCheck

-- for the Unbox instances
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

-- for the Elt instance
import qualified Data.Array.Repa.Eval as E

import qualified Algebra.Additive       as Additive (C)
import qualified Algebra.Field          as Field (C)
import qualified Algebra.IntegralDomain as IntegralDomain (C)
import qualified Algebra.Ring           as Ring (C)
import qualified Algebra.ZeroTestable   as ZeroTestable (C)

-- | The ring @Z_q@ of integers modulo 'q', using underlying integer
-- type 'z'.
newtype ZqBasic q z = ZqB z
                    deriving (Eq, Ord, ZeroTestable.C, E.Elt, Show, NFData, Storable)

-- the q argument, though phantom, matters for safety
type role ZqBasic nominal representational

--deriving instance (U.Unbox i) => G.Vector U.Vector (ZqBasic q i)
--deriving instance (U.Unbox i) => M.MVector U.MVector (ZqBasic q i)
--deriving instance (U.Unbox i) => U.Unbox (ZqBasic q i)

-- convenience synonym for many instances
type ReflectsTI q z = (Reflects q z, ToInteger z)

{-# INLINABLE reduce' #-}
reduce' :: forall q z . (ReflectsTI q z) => z -> ZqBasic q z
reduce' = ZqB . (`mod` proxy value (Proxy::Proxy q))

-- puts value in range [-q/2, q/2)
decode' :: forall q z . (ReflectsTI q z) => ZqBasic q z -> z
decode' = let qval = proxy value (Proxy::Proxy q)
          in \(ZqB x) -> if 2 * x < qval
                         then x
                         else x - qval

instance (ReflectsTI q z, Enum z) => Enumerable (ZqBasic q z) where
  values = let qval :: z = proxy value (Proxy::Proxy q)
           in coerce [0..(qval-1)]

instance (ReflectsTI q z) => Mod (ZqBasic q z) where
  type ModRep (ZqBasic q z) = z

  modulus = retag (value :: Tagged q z)

type instance CharOf (ZqBasic p z) = p

instance (PPow pp, zq ~ ZqBasic pp z,
          PrimeField (ZpOf zq), Ring zq, Ring (ZpOf zq))
         => ZPP (ZqBasic (pp :: PrimePower) z) where

  type ZpOf (ZqBasic pp z) = ZqBasic (PrimePP pp) z

  modulusZPP = retag (ppPPow :: Tagged pp PP)
  liftZp = coerce

instance (ReflectsTI q z) => Reduce z (ZqBasic q z) where
  reduce = reduce'

instance (Reflects q z, Ring (ZqBasic q z)) => Reduce Integer (ZqBasic q z) where
  reduce = fromInteger

type instance LiftOf (ZqBasic q z) = z

instance (ReflectsTI q z) => Lift' (ZqBasic q z) where
  lift = decode'

instance (ReflectsTI q z, ReflectsTI q' z, Ring z)
         => Rescale (ZqBasic q z) (ZqBasic q' z) where

    rescale = rescaleMod

instance (Reflects p z, ReflectsTI q z,
          Field (ZqBasic p z), Field (ZqBasic q z))
         => Encode (ZqBasic p z) (ZqBasic q z) where

    lsdToMSD = let pval :: z = proxy value (Proxy::Proxy p)
                   negqval :: z = negate $ proxy value (Proxy::Proxy q)
               in (reduce' negqval, recip $ reduce' pval)

-- | Yield a /principal/ @m@th root of unity @omega_m \in @Z_q^*@.
-- The implementation requires @q@ to be prime.  It works by finding a
-- generator of @Z_q^*@ and raising it to the @(q-1)/m@ power.
-- Therefore, outputs for different values of @m@ are consistent,
-- i.e., @omega_{m'}^(m'/m) = omega_m@.

principalRootUnity :: forall q z . (ReflectsTI q z, Enumerable (ZqBasic q z))
                      => Int -> Maybe (Int -> ZqBasic q z)
principalRootUnity =        -- use Integers for all intermediate calcs
    let qval = fromIntegral $ (proxy value (Proxy::Proxy q) :: z)
        -- order of Zq^* (assuming q prime)
        order = qval-1
        -- the primes dividing the order of Zq^*
        pfactors = fst <$> factorise order
        -- the powers we need to check
        exps = div order <$> pfactors
        -- whether an element is a generator of Zq^*
        isGen x = (x^order == one) && all (\e -> x^e /= one) exps
        -- for simplicity, require q to be prime
    in if isPrime qval
       then \m -> let (mq,mr) = order `divMod` fromIntegral m
                  in if mr == 0
                     then let omega = head (filter isGen values) ^ mq
                              omegaPows = V.iterateN m (*omega) one
                          in Just $ (omegaPows V.!) . (`mod` m)
                     else Nothing
       else const Nothing       -- fail if q composite


-- instance of CRTrans
instance (ReflectsTI q z, PID z, Enumerable (ZqBasic q z))
         => CRTrans (ZqBasic q z) where

  crtInfo =
    --DT.trace ("ZqBasic.crtInfo: q = " ++
    --          show (proxy value (Proxy::Proxy q) :: z)) $
    let qval :: z = proxy value (Proxy::Proxy q)
    in \m -> (,) <$> principalRootUnity m <*>
                     (reduce' <$> fromIntegral (valueHat m) `modinv` qval)

-- instance of CRTEmbed
instance (ReflectsTI q z, Ring (ZqBasic q z)) => CRTEmbed (ZqBasic q z) where
  type CRTExt (ZqBasic q z) = Complex Double

  toExt (ZqB x) = fromReal $ fromIntegral x
  fromExt x = reduce' $ NP.round $ real x

-- instance of Additive
instance (ReflectsTI q z, Additive z) => Additive.C (ZqBasic q z) where

  {-# INLINABLE zero #-}
  zero = ZqB zero

  {-# INLINABLE (+) #-}
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (ZqB x) (ZqB y) ->
        let z = x + y
        in ZqB (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (ZqB x) = reduce' $ negate x

-- instance of Ring
instance (ReflectsTI q z, Ring z) => Ring.C (ZqBasic q z) where
  {-# INLINABLE (*) #-}
  (ZqB x) * (ZqB y) = reduce' $ x * y
    
  {-# INLINABLE fromInteger #-}
  fromInteger =
    let qval = toInteger (proxy value (Proxy::Proxy q) :: z)
    -- this is safe as long as type z can hold the value of q
    in \x -> ZqB $ fromInteger $ x `mod` qval

-- instance of Field
instance (ReflectsTI q z, PID z, Show z) => Field.C (ZqBasic q z) where

  {-# INLINABLE recip #-}
  recip = let qval = proxy value (Proxy::Proxy q)
              -- safe because modinv returns in range 0..qval-1
          in \(ZqB x) -> ZqB $
               fromMaybe (error $ "ZqB.recip fail: " ++
                         show x ++ "\t" ++ show qval) $ modinv x qval

-- (canonical) instance of IntegralDomain, needed for Cyclotomics
instance (Field (ZqBasic q z)) => IntegralDomain.C (ZqBasic q z) where
  divMod a b = (a/b, zero)

-- Gadget-related instances
instance (ReflectsTI q z, Additive z) => Gadget TrivGad (ZqBasic q z) where
  gadget = tag [one]

instance (ReflectsTI q z, Ring z) => Decompose TrivGad (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose x = tag [lift x]

instance (ReflectsTI q z, Ring z) => Correct TrivGad (ZqBasic q z) where
  correct a = case untag a of
    [b] -> (b, [zero])
    _ -> error "Correct TrivGad: wrong length"

-- BaseBGad instances

gadlen :: (RealIntegral z) => z -> z -> Int
gadlen _ q | isZero q = 0
gadlen b q = 1 + (gadlen b $ q `div` b)

-- | The base-@b@ gadget for modulus @q@, over integers (not mod
-- anything).
gadgetZ :: (RealIntegral z) => z -> z -> [z]
gadgetZ b q = take (gadlen b q) $ iterate (*b) one

instance (ReflectsTI q z, RealIntegral z, Reflects b z)
         => Gadget (BaseBGad b) (ZqBasic q z) where

  gadget = let qval = proxy value (Proxy :: Proxy q)
               bval = proxy value (Proxy :: Proxy b)
           in tag $ reduce' <$> gadgetZ bval qval

instance (ReflectsTI q z, Ring z, ZeroTestable z, Reflects b z)
    => Decompose (BaseBGad b) (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose = let qval = proxy value (Proxy :: Proxy q)
                  bval = proxy value (Proxy :: Proxy b)
                  k = gadlen bval qval
                  radices = replicate (k-1) bval
              in tag . decomp radices . lift

-- | Yield the error vector for a noisy multiple of the gadget (all
-- over the integers). 
correctZ :: forall z . (RealIntegral z)
            => z                   -- ^ modulus @q@
            -> z                   -- ^ base @b@
            -> [z]                 -- ^ input vector @v = s \cdot g^t + e@
            -> [z]                 -- ^ error @e@
correctZ q b =
  let gadZ = gadgetZ b q
      k = length gadZ
      gadlast = last gadZ
  in \v -> 
    if length v /= k
    then error $ "correctZ: wrong length: was " ++ show (length v) ++", expected " ++ show k
    else let (w, x) = barBtRnd (q `div` b) v
             (v', v'l) = subLast v $ qbarD w x
             s = fst $ v'l `divModCent` gadlast
         in zipWith (-) v' $ (s*) <$> gadZ

    where
      -- | Yield @w = round(\bar{B}^t \cdot v / q)@, along with the inner
      -- product of @w@ with the top row of @q \bar{D}@.
      barBtRnd :: z -> [z] -> ([z], z)
      barBtRnd _ (_:[]) = ([], zero)
      barBtRnd q' (v1:vs@(v2:_)) = let quo = fst $ divModCent (b*v1-v2) q
                                   in (quo:) *** (quo*q' +) $
                                      barBtRnd (q' `div` b) vs

      -- | Yield @(q \bar{D}) \cdot w@, given precomputed first entry
      qbarD :: [z] -> z -> [z]
      qbarD [] x = [x]
      qbarD (w0:ws) x = x : qbarD ws (b*x - q*w0)

      -- | Yield the difference between the input vectors, along with
      -- their final entry.
      subLast :: [z] -> [z] -> ([z], z)
      subLast [v0] [v'0] = let y = v0-v'0 in ([y], y)
      subLast (v0:vs) (v'0:v's) = first ((v0-v'0):) $ subLast vs v's

instance (ReflectsTI q z, Ring z, Reflects b z)
    => Correct (BaseBGad b) (ZqBasic q z) where

  correct =
    let qval = proxy value (Proxy :: Proxy q)
        bval = proxy value (Proxy :: Proxy b)
        correct' = correctZ qval bval
    in \tv -> let v = untag tv
                  es = correct' $ lift <$> v
              in (head v - reduce (head es), es)

-- instance of Random
instance (ReflectsTI q z, Random z) => Random (ZqBasic q z) where
  random = let high = proxy value (Proxy::Proxy q) - 1
           in \g -> let (x,g') = randomR (0,high) g
                    in (ZqB x, g')

  randomR _ = error "randomR non-sensical for Zq types"

-- instance of Arbitrary
instance (ReflectsTI q z, Random z) => Arbitrary (ZqBasic q z) where
  arbitrary =
    let qval :: z = proxy value (Proxy::Proxy q)
    in fromIntegral <$> choose (0, qval-1)

  shrink = shrinkNothing

-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (ZqBasic q z) = MV_ZqBasic (U.MVector s z)
newtype instance U.Vector (ZqBasic q z) = V_ZqBasic (U.Vector z)

-- Unbox, when underlying representation is
instance U.Unbox z => U.Unbox (ZqBasic q z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox z => M.MVector U.MVector (ZqBasic q z) where
  basicLength (MV_ZqBasic v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqBasic v) = MV_ZqBasic $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqBasic v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqBasic <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqB x) = MV_ZqBasic <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqBasic v) z = ZqB <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqBasic v) z (ZqB x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqBasic v) = M.basicClear v
  basicSet (MV_ZqBasic v) (ZqB x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqBasic v) n = MV_ZqBasic <$> M.basicUnsafeGrow v n

instance U.Unbox z => G.Vector U.Vector (ZqBasic q z) where
  basicUnsafeFreeze (MV_ZqBasic v) = V_ZqBasic <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqBasic v) = MV_ZqBasic <$> G.basicUnsafeThaw v
  basicLength (V_ZqBasic v) = G.basicLength v
  basicUnsafeSlice z n (V_ZqBasic v) = V_ZqBasic $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqBasic v) z = ZqB <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqBasic mv) (V_ZqBasic v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
