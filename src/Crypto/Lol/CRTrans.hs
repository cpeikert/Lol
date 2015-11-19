{-# LANGUAGE FlexibleContexts, FlexibleInstances, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies #-}

-- | Classes and helper methods for the Chinese remainder transform
-- and ring extensions.

module Crypto.Lol.CRTrans
( CRTrans(..), CRTEmbed(..)
, CRTInfo
, crtInfoFact, crtInfoPPow, crtInfoNatC
, gEmbPPow, gEmbNatC
) where

import Crypto.Lol.LatticePrelude

import           Control.Arrow
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Type.Natural       (Sing (SS))

-- | Information that characterizes the (invertible) Chinese remainder
-- transformation over a ring @r@, namely:
--
--     (1) a function that returns the @i@th power of some /principal/
--     @m@th root of unity (for any integer @i@)
--
--     (2) the multiplicative inverse of @\\hat{m}@ in @r@.

type CRTInfo r = (Int -> r, r)

-- | A ring that (possibly) supports invertible Chinese remainder
-- transformations of various indices.

-- | The values of 'crtInfo' for different indices @m@ should be
-- consistent, in the sense that if @omega@, @omega'@ are respectively
-- the values returned for @m@, @m'@ where @m'@ divides @m@, then it
-- should be the case that @omega^(m/m')=omega'@.

class Ring r => CRTrans r where

  -- | 'CRTInfo' for a given index @m@. The method itself may be
  -- slow, but the function it returns should be fast, e.g., via
  -- internal memoization.  The default implementation returns
  -- 'Nothing'.
  crtInfo :: Int -> Maybe (CRTInfo r)
  crtInfo = const Nothing

-- | A ring with a ring embedding into some ring @CRTExt r@ that has
-- an invertible CRT transformation for /every/ positive index @m@.
class (Ring r, Ring (CRTExt r)) => CRTEmbed r where
  type CRTExt r

  -- | Embeds from @r@ to @CRTExt r@
  toExt :: r -> CRTExt r
  -- | Projects from @CRTExt r@ to @r@
  fromExt :: CRTExt r -> r

-- CRTrans instance for product rings
instance (CRTrans a, CRTrans b) => CRTrans (a,b) where
  crtInfo i = do
    (apow, aiInv) <- crtInfo i
    (bpow, biInv) <- crtInfo i
    return (apow &&& bpow, (aiInv, biInv))

-- CRTEmbed instance for product rings
instance (CRTEmbed a, CRTEmbed b) => CRTEmbed (a,b) where
  type CRTExt (a,b) = (CRTExt a, CRTExt b)
  toExt = toExt *** toExt
  fromExt = fromExt *** fromExt

omegaPowC :: (Transcendental a) => Int -> Int -> Complex a
omegaPowC m i = cis (2*pi*fromIntegral i / fromIntegral m)

-- | 'crtInfo' wrapper for 'Fact' types.
crtInfoFact :: (Fact m, CRTrans r) => TaggedT m Maybe (CRTInfo r)
crtInfoFact = (tagT . crtInfo) =<< pureT valueFact

-- | 'crtInfo' wrapper for 'PPow' types.
crtInfoPPow :: (PPow pp, CRTrans r) => TaggedT pp Maybe (CRTInfo r)
crtInfoPPow = (tagT . crtInfo) =<< pureT valuePPow

-- | 'crtInfo' wrapper for 'NatC' types.
crtInfoNatC :: (NatC p, CRTrans r) => TaggedT p Maybe (CRTInfo r)
crtInfoNatC = (tagT . crtInfo) =<< pureT valueNatC

-- | A function that returns the 'i'th embedding of @g_{p^e} = g_p@ for
-- @i@ in @Z*_{p^e}@.
gEmbPPow :: forall pp r . (PPow pp, CRTrans r) => TaggedT pp Maybe (Int -> r)
gEmbPPow = tagT $ case (sing :: SPrimePower pp) of
  -- intentionally no match for zero exponents
  (SPP (STuple2 sp (SS _))) -> withWitnessT gEmbNatC sp

-- | A function that returns the @i@th embedding of @g_p@ for @i@ in @Z*_p@,
-- i.e., @1-omega_p^i@.
gEmbNatC :: (NatC p, CRTrans r) => TaggedT p Maybe (Int -> r)
gEmbNatC = do
  (f, _) <- crtInfoNatC
  return $ \i -> one - f i      -- not checking that i /= 0 (mod p)

-- the complex numbers have roots of unity of any order
instance (Transcendental a) => CRTrans (Complex a) where
  crtInfo m = Just (omegaPowC m, recip $ fromIntegral $ valueHat m)

-- trivial CRTEmbed instance for complex numbers
instance (Transcendental a) => CRTEmbed (Complex a) where
  type CRTExt (Complex a) = Complex a
  toExt = id
  fromExt = id

-- Default CRTrans instances for real and integer types, which do
-- not have roots of unity (except in trivial cases). These are needed
-- to use FastCyc with these integer types.
instance CRTrans Double
instance CRTrans Int
instance CRTrans Int64
instance CRTrans Integer
-- can also do for Int8, Int16, Int32 etc.

-- CRTEmbed instances for real and integer types, embedding into
-- Complex.  These are needed to use FastCyc with these integer types.
instance CRTEmbed Double where
  type CRTExt Double = Complex Double
  toExt = fromReal . realToField
  fromExt = realToField . real

instance CRTEmbed Int where
  type CRTExt Int = Complex Double
  toExt = fromIntegral
  fromExt = fst . roundComplex

instance CRTEmbed Int64 where
  type CRTExt Int64 = Complex Double
  toExt = fromIntegral
  fromExt = fst . roundComplex

instance CRTEmbed Integer where
  -- CJP: sufficient precision?  Not in general.
  type CRTExt Integer = Complex Double
  toExt = fromIntegral
  fromExt = fst . roundComplex
