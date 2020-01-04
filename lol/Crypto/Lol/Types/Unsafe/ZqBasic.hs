{-|
Module      : Crypto.Lol.Types.Unsafe.ZqBasic
Description : An implementation of modular arithmetic over the integers.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\C{\mathbb{C}} \)

An implementation of the quotient ring \(\Z_q = \Z/(q\Z)\).
This module is "unsafe" because it exports the 'ZqBasic' constructor.
This module should only be used to make tensor-specific instances for 'ZqBasic'.
The safe way to use this type is to import "Crypto.Lol.Types".

EAC: It may help GHC do specialization at higher levels of the library
if we "simplify" constraints in this module. For example, replace the
(Additive (ZqBasic q z)) constraint on the Reduce instance with
(Additive z)
-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Types.Unsafe.ZqBasic
( ZqBasic(..) -- export constructor from this (unsafe) module
, goodQs
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Gadget
import Crypto.Lol.Prelude           as LP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZPP

import Math.NumberTheory.Primes

import Control.Applicative
import Control.Arrow
import Control.DeepSeq        (NFData)
import Data.Maybe
import NumericPrelude.Numeric as NP (round)
import System.Random

import qualified Data.Vector as V

import qualified Algebra.Additive       as Additive (C)
import qualified Algebra.Field          as Field (C)
import qualified Algebra.IntegralDomain as IntegralDomain (C)
import qualified Algebra.Ring           as Ring (C)
import qualified Algebra.ZeroTestable   as ZeroTestable (C)

-- | An infinite list of primes greater than @lower@ and congruent to
-- 1 mod @m@.
goodQs :: (ToInteger a) => a -> a -> [a]
goodQs m lower = filter (isJust . isPrime . toInteger) $
  iterate (+m) $ lower + ((m-lower) `mod` m) + 1

-- | The ring \(\Z_q\) of integers modulo 'q', using underlying integer
-- type 'z'.
newtype ZqBasic q z = ZqB { unZqB :: z }
    deriving (Eq, Ord, ZeroTestable.C, Show, NFData)

-- the q argument, though phantom, matters for safety
type role ZqBasic nominal representational

--deriving instance (U.Unbox i) => G.Vector U.Vector (ZqBasic q i)
--deriving instance (U.Unbox i) => M.MVector U.MVector (ZqBasic q i)
--deriving instance (U.Unbox i) => U.Unbox (ZqBasic q i)

{-# INLINABLE reduce' #-}
reduce' :: forall q z . (Reflects q z, ToInteger z) => z -> ZqBasic q z
reduce' = ZqB . (`mod` value @q)

-- puts value in range [-q/2, q/2)
decode' :: forall q z . (Reflects q z, ToInteger z) => ZqBasic q z -> z
decode' = let qval = value @q
          in \(ZqB x) -> if 2 * x < qval then x else x - qval

instance (Reflects q z, ToInteger z, Enum z) => Enumerable (ZqBasic q z) where
  values = ZqB <$> [0..(value @q - 1)]

instance (Reflects q z, ToInteger z) => Mod (ZqBasic q z) where
  type ModRep (ZqBasic q z) = z

  modulus = value @q

type instance CharOf (ZqBasic p z) = p

instance (PPow pp, zq ~ ZqBasic pp z,
          PrimeField (ZpOf zq), Ring zq)
         => ZPP (ZqBasic (pp :: PrimePower) z) where

  type ZpOf (ZqBasic pp z) = ZqBasic (PrimePP pp) z

  modulusZPP = ppPPow @pp
  liftZp = ZqB . unZqB

instance (Reflects q z, ToInteger z) => Reduce z (ZqBasic q z) where
  reduce = reduce'

instance (Reflects q z, ToInteger z, Ring z) => Reduce Integer (ZqBasic q z) where
  reduce = fromInteger

type instance LiftOf (ZqBasic q z) = z

instance (Reflects q z, ToInteger z) => Lift' (ZqBasic q z) where
  lift = decode'

instance (Reflects q z, ToInteger z, Reflects q' z, Ring z)
         => Rescale (ZqBasic q z) (ZqBasic q' z) where

  rescale = rescaleMod

instance (Reflects p z, Reflects q z, ToInteger z, Field (ZqBasic q z), Field (ZqBasic p z))
         => Encode (ZqBasic p z) (ZqBasic q z) where

  lsdToMSD = let pval = value @p
                 negqval = negate $ value @q
             in (reduce' negqval, recip $ reduce' pval)

-- | Yield a /principal/ \(m\)th root of unity \(\omega_m \in \Z_q^*\).
-- The implementation requires \(q\) to be prime.  It works by finding a
-- generator of \(\Z_q^*\) and raising it to the \( (q-1)/m\) power.
-- Therefore, outputs for different values of \(m\) are consistent,
-- i.e., \(\omega_{m'}^(m'/m) = \omega_m\).
principalRootUnity ::
    forall m q z . (Reflects m Int, Reflects q z, ToInteger z, Enumerable (ZqBasic q z))
               => TaggedT m Maybe (Int -> ZqBasic q z)
principalRootUnity =        -- use Integers for all intermediate calcs
  let qval = fromIntegral (value @q :: z)
      mval = value @m
      -- order of Zq^* (assuming q prime)
      order = qval-1
      -- the primes dividing the order of Zq^*
      pfactors = unPrime . fst <$> (factorise @Integer) order
      -- the powers we need to check
      exps = div order <$> pfactors
      -- whether an element is a generator of Zq^*
      isGen x = (x^order == one) && all (\e -> x^e /= one) exps
  in tagT $ if isJust (isPrime qval) -- for simplicity, require q to be prime
            then let (mq,mr) = order `divMod` fromIntegral mval
                 in if mr == 0
                    then let omega = head (filter isGen values) ^ mq
                             omegaPows = V.iterateN mval (*omega) one
                         in Just $ (omegaPows V.!) . (`mod` mval)
                    else Nothing
            else Nothing       -- fail if q composite

mhatInv :: forall m q z . (Reflects m Int, Reflects q z, ToInteger z, PID z)
           => TaggedT m Maybe (ZqBasic q z)
mhatInv = tagT $ reduce' <$>
          ((`modinv` (value @q)) $ fromIntegral $ valueHat $ (value @m :: Int))

-- instance of CRTrans
instance (Reflects q z, ToInteger z, PID z, Enumerable (ZqBasic q z))
         => CRTrans Maybe (ZqBasic q z) where
  crtInfo = (,) <$> principalRootUnity <*> mhatInv

-- | Embeds into the complex numbers \( \C \).
instance (Reflects q z, ToInteger z, Ring (ZqBasic q z)) => CRTEmbed (ZqBasic q z) where
  type CRTExt (ZqBasic q z) = Complex Double

  toExt (ZqB x) = fromReal $ fromIntegral x
  fromExt = reduce' . NP.round . real

-- instance of Additive
instance (Reflects q z, ToInteger z, Additive z) => Additive.C (ZqBasic q z) where

  {-# INLINABLE zero #-}
  zero = ZqB zero

  {-# INLINABLE (+) #-}
  (ZqB x) + (ZqB y) = reduce' $ x + y

  {-# INLINABLE negate #-}
  negate (ZqB x) = reduce' $ negate x

-- instance of Ring
instance (Reflects q z, ToInteger z, Ring z) => Ring.C (ZqBasic q z) where
  {-# INLINABLE (*) #-}
  (ZqB x) * (ZqB y) = reduce' $ x * y

  {-# INLINABLE fromInteger #-}
  fromInteger =
    let qval = toInteger (value @q :: z)
    -- this is safe as long as type z can hold the value of q
    in \x -> ZqB $ fromInteger $ x `mod` qval

-- instance of Field
instance (Reflects q z, ToInteger z, PID z, Show z) => Field.C (ZqBasic q z) where

  {-# INLINABLE recip #-}
  recip = let qval = value @q
              -- safe because modinv returns in range 0..qval-1
          in \(ZqB x) -> ZqB $
               fromMaybe (error $ "ZqB.recip fail: " ++
                         show x ++ "\t" ++ show qval) $ modinv x qval

-- (canonical) instance of IntegralDomain, needed for Cyclotomics
instance (Reflects q z, ToInteger z, Field (ZqBasic q z)) => IntegralDomain.C (ZqBasic q z) where
  divMod a b = (a/b, zero)

-- Gadget-related instances
instance (Reflects q z, ToInteger z) => Gadget TrivGad (ZqBasic q z) where
  gadget = [one]

instance (Reflects q z, ToInteger z) => Decompose TrivGad (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose x = [lift x]

instance (Reflects q z, ToInteger z, Ring z) => Correct TrivGad (ZqBasic q z) where
  correct [b] = (b, [zero])
  correct _   = error "Correct TrivGad: wrong length"

-- BaseBGad instances

gadlen :: (RealIntegral z) => z -> z -> Int
gadlen _ q | isZero q = 0
gadlen b q = 1 + gadlen b (q `div` b)

-- | The base-\(b\) gadget for modulus \(q\), over integers (not mod
-- anything).
gadgetZ :: (RealIntegral z) => z -> z -> [z]
gadgetZ b q = take (gadlen b q) $ iterate (*b) one

instance (Reflects q z, ToInteger z, RealIntegral z, Reflects b z)
         => Gadget (BaseBGad b) (ZqBasic q z) where

  gadget = let qval = value @q
               bval = value @b
           in reduce' <$> gadgetZ bval qval

instance (Reflects q z, ToInteger z, Reflects b z)
    => Decompose (BaseBGad b) (ZqBasic q z) where
  type DecompOf (ZqBasic q z) = z
  decompose = let qval = value @q
                  bval = value @b
                  k = gadlen bval qval
                  radices = replicate (k-1) bval
              in decomp radices . lift

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
      barBtRnd _ [_] = ([], zero)
      barBtRnd q' (v1:vs@(v2:_)) = let quo = fst $ divModCent (b*v1-v2) q
                                   in (quo:) *** (quo*q' +) $
                                      barBtRnd (q' `div` b) vs

      -- | Yield @(q \bar{D}) \cdot w@, given precomputed first entry
      qbarD :: [z] -> z -> [z]
      qbarD [] x      = [x]
      qbarD (w0:ws) x = x : qbarD ws (b*x - q*w0)

      -- | Yield the difference between the input vectors, along with
      -- their final entry.
      subLast :: [z] -> [z] -> ([z], z)
      subLast [v0] [v'0]        = let y = v0-v'0 in ([y], y)
      subLast (v0:vs) (v'0:v's) = first ((v0-v'0):) $ subLast vs v's

instance (Reflects q z, ToInteger z, Reflects b z)
    => Correct (BaseBGad b) (ZqBasic q z) where

  correct =
    let qval = value @q
        bval = value @b
        correct' = correctZ qval bval
    in \tv -> let es = correct' $ lift <$> tv
              in (head tv - reduce (head es), es)

-- instance of Random
instance (Reflects q z, ToInteger z, Random z) => Random (ZqBasic q z) where
  random = let high = value @q - 1
           in \g -> let (x,g') = randomR (0,high) g
                    in (ZqB x, g')

  randomR _ = error "randomR non-sensical for Zq types"
  {-# INLINABLE random #-}
