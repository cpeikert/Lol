{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, 
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | An implementation of modular arithmetic, i.e., the ring Zq.

module Crypto.Lol.Types.ZqBasic
( ZqBasic -- export the type, but not the constructor (for safety)
) where

import Crypto.Lol.LatticePrelude as LP
import Crypto.Lol.Reflects
import Crypto.Lol.CRTrans
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Gadget

import Control.Applicative
import Control.DeepSeq        (NFData)
import Control.Monad          (liftM)
import Data.Coerce
import Data.Maybe
import Data.Typeable
import NumericPrelude.Numeric as NP (round)
import System.Random
import Test.QuickCheck

-- for the Unbox instances
import qualified Data.Vector.Generic         as V
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

--deriving instance (U.Unbox i) => V.Vector U.Vector (ZqBasic q i)
--deriving instance (U.Unbox i) => M.MVector U.MVector (ZqBasic q i)
--deriving instance (U.Unbox i) => U.Unbox (ZqBasic q i)

-- convenience synonym for many instances
type ReflectsTI q z = (Reflects q z, ToInteger z)

reduce' :: forall q z . (ReflectsTI q z) => z -> ZqBasic q z
reduce' = coerce . (`mod` proxy value (Proxy::Proxy q))

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
          PrimeField (ZPOf zq), Ring zq, Ring (ZPOf zq)) 
         => ZPP (ZqBasic (pp :: PrimePower) z) where

  type ZPOf (ZqBasic pp z) = ZqBasic (PrimePP pp) z

  modulusZPP = retag (ppPPow :: Tagged pp PP)

  liftZp = coerce

instance (ReflectsTI q z) => Reduce z (ZqBasic q z) where
  reduce = reduce'

instance (Reflects q z, Ring (ZqBasic q z)) => Reduce Integer (ZqBasic q z) where
  reduce = fromInteger

instance (ReflectsTI q z) => Lift' (ZqBasic q z) where
  type LiftOf (ZqBasic q z) = z
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

-- instance of CRTrans
instance (Reflects q z, PID z, r ~ (ZqBasic q z), Mod r, Enumerable r,
          Show z) -- for DT.trace
         => CRTrans (ZqBasic q z) where

  crtInfo =
    --DT.trace ("ZqBasic.crtInfo: q = " ++ 
    --          show (proxy value (Proxy::Proxy q) :: z)) $
    let qval :: z = proxy value (Proxy::Proxy q)
    in \m -> (,) <$> omegaPowMod m <*>
  -- CJP: using coerce depends on modinv returning in [0..q-1]
                     (coerce $ fromIntegral (valueHat m) `modinv` qval)

-- instance of CRTEmbed
instance (ReflectsTI q z, Ring (ZqBasic q z)) => CRTEmbed (ZqBasic q z) where
  type CRTExt (ZqBasic q z) = Complex Double

  toExt (ZqB x) = fromReal $ fromIntegral x
  fromExt x = reduce' $ NP.round $ real x

-- instance of Additive
instance (ReflectsTI q z, Additive z) => Additive.C (ZqBasic q z) where
  -- CJP: "LHS too complicated to desugar"; might be fixed in 7.10:
  -- https://ghc.haskell.org/trac/ghc/ticket/8848
  {-# SPECIALIZE instance ReflectsTI q Int => Additive.C (ZqBasic q Int) #-}
  {-# SPECIALIZE instance ReflectsTI q Int64 => Additive.C (ZqBasic q Int64) #-}
  
  zero = ZqB zero
  
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (ZqB x) (ZqB y) ->
        let z = x + y
        in ZqB (if z >= qval then z - qval else z)

  negate (ZqB x) = reduce' $ negate x

-- instance of Ring
instance (ReflectsTI q z, Ring z) => Ring.C (ZqBasic q z) where
    (ZqB x) * (ZqB y) = reduce' $ x * y

    fromInteger x =
      let qval = toInteger (proxy value (Proxy::Proxy q) :: z)
    -- this is safe as long as type z can hold the value of q
      in ZqB $ fromInteger $ x `mod` qval

-- instance of Field
instance (ReflectsTI q z, PID z, Show z) => Field.C (ZqBasic q z) where

  recip = let qval = proxy value (Proxy::Proxy q)
              -- safe because modinv returns in range 0..qval-1
          in \(ZqB x) -> ZqB $ 
               fromMaybe (error $ "ZqB.recip fail: " ++ 
                         show x ++ "\t" ++ show qval) $ modinv x qval

-- (canonical) instance of IntegralDomain, needed for FastCyc
instance (Field (ZqBasic q z)) => IntegralDomain.C (ZqBasic q z) where
    divMod a b = (a/b, zero)

-- Gadget-related instances
instance (ReflectsTI q z, Additive z)
         => Gadget TrivGad (ZqBasic q z) where
  
  gadget = tag [one]

instance (ReflectsTI q z, Ring z) => Decompose TrivGad (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose x = tag [lift x]

instance (ReflectsTI q z, Ring z) => Correct TrivGad (ZqBasic q z) where
  correct a = case untag a of
    [b] -> b
    _ -> error "Correct TrivGad: wrong length"

instance (ReflectsTI q z, Additive z, Reflects b z)
         => Gadget (BaseBGad b) (ZqBasic q z) where
  
  gadget = let qval = proxy value (Proxy :: Proxy q)
               bval = proxy value (Proxy :: Proxy b)
               k = logCeil bval qval
           in tag $ map reduce' (take k (iterate (*bval) one))

instance (ReflectsTI q z, Ring z, Reflects b z) => Decompose (BaseBGad b) (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose = let qval = proxy value (Proxy :: Proxy q)
                  bval = proxy value (Proxy :: Proxy b)
                  k = logCeil bval qval
                  radices = replicate (k-1) bval
              in tag . decomp radices . lift

-- TODO: implement Correct for BaseBGad b

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
instance (U.Unbox z) => U.Unbox (ZqBasic q z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance (U.Unbox z) => M.MVector U.MVector (ZqBasic q z) where
  basicLength (MV_ZqBasic v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqBasic v) = MV_ZqBasic $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqBasic v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqBasic `liftM` M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqB x) = MV_ZqBasic `liftM` M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqBasic v) z = ZqB `liftM` M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqBasic v) z (ZqB x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqBasic v) = M.basicClear v
  basicSet (MV_ZqBasic v) (ZqB x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqBasic v) n = MV_ZqBasic `liftM` M.basicUnsafeGrow v n

instance (U.Unbox z) => V.Vector U.Vector (ZqBasic q z) where
  basicUnsafeFreeze (MV_ZqBasic v) = V_ZqBasic `liftM` V.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqBasic v) = MV_ZqBasic `liftM` V.basicUnsafeThaw v
  basicLength (V_ZqBasic v) = V.basicLength v
  basicUnsafeSlice z n (V_ZqBasic v) = V_ZqBasic $ V.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqBasic v) z = ZqB `liftM` V.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqBasic mv) (V_ZqBasic v) = V.basicUnsafeCopy mv v
  elemseq _ = seq
