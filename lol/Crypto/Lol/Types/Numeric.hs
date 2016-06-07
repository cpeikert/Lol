{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleInstances, GADTs,
             MultiParamTypeClasses, NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeOperators #-}

-- we have some orphan instances here for instances of
-- package classes with Prelude data types
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module imports NumericPrelude and defines constraint
-- synonyms for NumericPrelude classes to help with code readability,
-- and defines saner versions of some NumericPrelude functions

module Crypto.Lol.Types.Numeric
( module Crypto.Lol.Types.Numeric -- everything we define here
, module NumericPrelude         -- re-export
, Int64                         -- commonly used
) where

import Control.DeepSeq
import Control.Monad.Random

-- NumericPrelude has silly types for these functions
import           NumericPrelude         hiding (abs, max, min, (^))
import qualified NumericPrelude.Numeric (abs)
import qualified Prelude                (max, min)

import qualified Algebra.Absolute             (C)
import qualified Algebra.Additive             (C)
import qualified Algebra.Algebraic            (C)
import qualified Algebra.Field                (C)
import qualified Algebra.IntegralDomain       (C)
import qualified Algebra.Module               (C)
import qualified Algebra.PrincipalIdealDomain (C)
import qualified Algebra.RealField            (C)
import qualified Algebra.RealIntegral         (C)
import qualified Algebra.RealRing             (C)
import qualified Algebra.RealTranscendental   (C)
import qualified Algebra.Ring                 (C)
import qualified Algebra.ToInteger            (C)
import qualified Algebra.ToRational           (C, realToField)
import qualified Algebra.Transcendental       (C)
import qualified Algebra.ZeroTestable         (C)
import           MathObj.Polynomial

import Data.Int (Int64)

-- | The Prelude definition of 'max'.
max :: Ord a => a -> a -> a
max = Prelude.max

-- | The Prelude definition of 'min'.
min :: Ord a => a -> a -> a
min = Prelude.min

-- | The sane definition of 'abs' from
-- 'NumericPrelude.Numeric'
-- rather than the default from 'NumericPrelude'.
abs :: Absolute a => a -> a
abs = NumericPrelude.Numeric.abs

-- | The hidden NP function from 'Algebra.ToRational'.
realToField :: (Field b, ToRational a) => a -> b
realToField = Algebra.ToRational.realToField

-- use this if you need:
{- isZero -}
-- | Sane synonym for 'Algebra.ZeroTestable.C'.
type ZeroTestable a = (Algebra.ZeroTestable.C a)

{- - + negate -}
-- | Sane synonym for 'Algebra.Additive.C'.
type Additive a = (Algebra.Additive.C a)

{- Additive, plus: * fromIntegral -}
-- | Sane synonym for 'Algebra.Ring.C'.
type Ring a = (Algebra.Ring.C a)

{- Ring and Additive, plus: *> -}
-- | Sane synonym for 'Algebra.Module.C'.
type Module a v = (Algebra.Module.C a v)

{- Ring, plus: div, mod, divmod -}
-- | Sane synonym for 'Algebra.IntegralDomain.C'.
type IntegralDomain a = (Algebra.IntegralDomain.C a)

{- Ring, plus: abs signum toRational' -}
-- | Sane synonym for 'Algebra.ToRational.C'.
type ToRational a = (Algebra.ToRational.C a)

{- Ring, plus: / recip fromRational -}
-- | Sane synonym for 'Algebra.Field.C'.
type Field a = (Algebra.Field.C a)

{- Ring, plus: abs and rounding functions -}
-- | Sane synonym for 'Algebra.RealRing.C'.
type RealRing a = (Algebra.RealRing.C a)

{- Field, plus: abs signum round floor ceiling -}
-- | Sane synonym for 'Algebra.RealField.C'.
type RealField a = (Algebra.RealField.C a)

{- Field, plus: sqrt root ^/ -}
-- | Sane synonym for 'Algebra.Algebraic.C'.
type Algebraic a = (Algebra.Algebraic.C a)

{- Algebraic, plus: pi exp log sin atan -}
-- | Sane synonym for 'Algebra.Transcendental.C'.
type Transcendental a = (Algebra.Transcendental.C a)

{- Transcendental and RealField, plus atan2 -}
-- | Sane synonym for 'Algebra.RealTranscendental.C'.
type RealTranscendental a = (Algebra.RealTranscendental.C a)

{- Transcendental, plus: == <= >= < > -}
-- | Convenient synonym for @(Ord a, Transcendental a)@
type OrdFloat a = (Ord a, Transcendental a)

{- ToRational and Ring, plus: toInteger div mod divmod quot rem quotrem -}
-- | Sane synonym for 'Algebra.ToInteger.C'.
type ToInteger a = (Algebra.ToInteger.C a)

-- | Sane synonym for 'Algebra.Absolute.C'.
type Absolute a = (Algebra.Absolute.C a)

-- | Sane synonym for 'Algebra.RealIntegral.C'.
type RealIntegral a = (Algebra.RealIntegral.C a)

-- | Sane synonym for 'Algebra.PrincipalIdealDomain.C'.
type PID a = (Algebra.PrincipalIdealDomain.C a)

-- | Sane synonym for 'MathObj.Polynomial.T'.
type Polynomial a = MathObj.Polynomial.T a

-- IntegralDomain instance for Double
instance Algebra.IntegralDomain.C Double where
    _ `div` 0 = error "cannot divide Double by 0\n"
    a `div` b = a / b
    _ `mod` _ = 0

-- NFData instance for Polynomial, missing from NP
instance (NFData r) => NFData (Polynomial r) where
  rnf = rnf . coeffs

-- | Our custom exponentiation, overriding NP's version that
-- requires 'Integer' exponent.
-- Copied from http://hackage.haskell.org/package/base-4.7.0.0/docs/src/GHC-Real.html#%5E
{-# SPECIALISE [1] (^) ::
        Integer -> Integer -> Integer,
        Integer -> Int -> Integer,
        Int -> Int -> Int,
        Int64 -> Int64 -> Int64
  #-}
(^) :: forall a i . (Ring a, ToInteger i) => a -> i -> a
x0 ^ y0 | y0 < 0    = error "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where -- f : x0 ^ y0 = x ^ y
          f :: a -> i -> a -- a polymorphic local binding needs a sig
          f x y | even y    = f (x * x) (y `quot` 2)
                | y == 1    = x
                | otherwise = g (x * x) ((y - 1) `quot` 2) x
          -- g : x0 ^ y0 = (x ^ y) * z
          g :: a -> i -> a -> a
          g x y z | even y = g (x * x) (y `quot` 2) z
                  | y == 1 = x * z
                  | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

-- | Inverse of @a@ modulo @q@, in range @0..q-1@.  (Argument order is
-- infix-friendly.)
modinv :: (PID i, Eq i) => i -> i -> Maybe i
modinv a q = let (d, (_, inv)) = extendedGCD q a
             in if d == one
                then Just $ inv `mod` q
                else Nothing

-- | Decompose an element into a list of "centered" digits with respect
-- to relative radices.
decomp :: (IntegralDomain z, Ord z) => [z] -> z -> [z]
decomp [] v = [v]
decomp (b:bs) v = let (q,r) = v `divModCent` b
                  in r : decomp bs q

-- | Deterministically round to the nearest multiple of @i@.
roundMult :: (RealField r, ToInteger i) => i -> r -> i
roundMult 1 r  = round r
roundMult i r = let r' = r / fromIntegral i in i * round r'

-- | Randomly round to the nearest larger or smaller multiple of @i@,
-- where the round-off term has expectation zero.
roundScalarCentered :: (RealField r, Random r, ToInteger i,
                        MonadRandom mon)
                      => i -> r -> mon i
roundScalarCentered p x =
  let x' = x / fromIntegral p
      mod1 = x' - floor x'
  in do prob <- getRandomR (zero, one)
        return $ p * if prob < mod1
                     then ceiling x'
                     else floor x'

-- | Variant of 'Algebra.IntegralDomain.divMod' in which the remainder
-- is in the range @[-b\/2,b\/2)@.
divModCent :: (IntegralDomain i, Ord i)
              => i              -- ^ dividend @a@
              -> i              -- ^ divisor @b@
              -> (i,i)          -- ^ (quotient, remainder)
divModCent = flip (\b ->
             let shift = b `div` 2
             in \a -> let (q,r) = (a+shift) `divMod` b
                      in (q,r-shift))
