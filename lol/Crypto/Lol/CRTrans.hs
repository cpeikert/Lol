{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             PolyKinds, RebindableSyntax, ScopedTypeVariables,
             TypeFamilies #-}

-- | Classes and helper methods for the Chinese remainder transform
-- and ring extensions.

module Crypto.Lol.CRTrans
( CRTrans(..), CRTEmbed(..)
, CRTInfo
) where

import Crypto.Lol.Prelude
import Crypto.Lol.Reflects

import Control.Arrow

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
-- the roots of unity used for @m@, @m'@ where @m'@ divides @m@, then
-- it should be the case that @omega^(m/m')=omega'@.

class (Monad mon, Ring r) => CRTrans mon r where

  -- | 'CRTInfo' for a given index @m@. The method itself may be
  -- slow, but the function it returns should be fast, e.g., via
  -- internal memoization.
  crtInfo :: Reflects m Int => TaggedT m mon (CRTInfo r)

-- | A ring with a ring embedding into some ring @CRTExt r@ that has
-- an invertible CRT transformation for /every/ positive index @m@.
class (Ring r, Ring (CRTExt r)) => CRTEmbed r where
  type CRTExt r

  -- | Embeds from @r@ to @CRTExt r@
  toExt :: r -> CRTExt r
  -- | Projects from @CRTExt r@ to @r@
  fromExt :: CRTExt r -> r

-- CRTrans instance for product rings
instance (CRTrans mon a, CRTrans mon b) => CRTrans mon (a,b) where
  crtInfo = do
    (fa, inva) <- crtInfo
    (fb, invb) <- crtInfo
    return (fa &&& fb, (inva, invb))

-- CRTEmbed instance for product rings
instance (CRTEmbed a, CRTEmbed b) => CRTEmbed (a,b) where
  type CRTExt (a,b) = (CRTExt a, CRTExt b)
  toExt = toExt *** toExt
  fromExt = fromExt *** fromExt

-- the complex numbers have roots of unity of any order
instance (Monad mon, Transcendental a) => CRTrans mon (Complex a) where
  crtInfo = crtInfoC

crtInfoC :: forall mon m a . (Monad mon, Reflects m Int, Transcendental a)
            => TaggedT m mon (CRTInfo (Complex a))
crtInfoC = let mval = proxy value (Proxy::Proxy m)
               mhat = valueHat mval
           in return (omegaPowC mval, recip $ fromIntegral mhat)

omegaPowC :: (Transcendental a) => Int -> Int -> Complex a
omegaPowC m i = cis (2*pi*fromIntegral i / fromIntegral m)

-- trivial CRTEmbed instance for complex numbers
instance (Transcendental a) => CRTEmbed (Complex a) where
  type CRTExt (Complex a) = Complex a
  toExt = id
  fromExt = id

-- CRTrans instances for real and integer types, which do
-- not have roots of unity (except in trivial cases). These are needed
-- to use Cyc with these integral types.
instance CRTrans Maybe Double where crtInfo = tagT Nothing
instance CRTrans Maybe Int where crtInfo = tagT Nothing
instance CRTrans Maybe Int64 where crtInfo = tagT Nothing
instance CRTrans Maybe Integer where crtInfo = tagT Nothing
-- can also do for Int8, Int16, Int32 etc.

-- CRTEmbed instances for real and integer types, embedding into
-- Complex.  These are needed to use Cyc with these integer types.
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
