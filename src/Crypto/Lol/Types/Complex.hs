{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables,
             StandaloneDeriving, TemplateHaskell, TypeFamilies,
             UndecidableInstances #-}

-- | Data type, functions, and instances for complex numbers.

module Crypto.Lol.Types.Complex (
  Complex
, roundComplex
, cis, real, imag, fromReal
) where

import           Algebra.Additive       as Additive (C)
import           Algebra.Field          as Field (C)
import           Algebra.IntegralDomain as IntegralDomain
import           Algebra.Ring           as Ring (C)
import           Algebra.ZeroTestable   as ZeroTestable (C)
import qualified Number.Complex         as C hiding (exp, signum)

import Crypto.Lol.Types.Numeric as LP

import Control.DeepSeq
import Data.Array.Repa.Eval         as R
import Data.Vector.Storable         (Storable)
import Data.Vector.Unboxed          (Unbox)
import Data.Vector.Unboxed.Deriving
import System.Random
import Test.QuickCheck

-- | Newtype wrapper (with slightly different instances) for
-- <https://hackage.haskell.org/package/numeric-prelude-0.4.2/docs/Number-Complex.html numeric-prelude Complex>.
newtype Complex a = Complex (C.T a) deriving (Additive.C, Ring.C, ZeroTestable.C, Field.C, Storable, Eq, Show, Arbitrary)

derivingUnbox "Complex"
  [t| forall a . (Unbox a) => Complex a -> (a, a) |]
  [| \ (Complex x) -> (C.real x, C.imag x) |]
  [| \ (r, i) -> Complex $ r C.+: i |]

-- a custom IntegralDomain instance, replacing the one provided by NP.
-- it always returns 0 as the remainder of a division.  If we were to
-- use the NP instance, sometimes precision issues yield nonzero
-- remainders, which makes, e.g., 'divGPow' think that division has
-- failed, when it has not.  This in turn causes 'divGCRT' to yield
-- Nothing, among other problems.
instance (Field a) => IntegralDomain.C (Complex a) where
  (Complex a) `divMod` (Complex b) = (Complex $ a / b, LP.zero)

-- we can't use Generics for NFData because NP doesn't export the
-- (deep) constructor for Complex.T
instance (NFData a) => NFData (Complex a) where
  rnf (Complex x) = let r = C.real x
                        i = C.imag x
                    in rnf r `seq` rnf i `seq` ()

instance (Random a) => Random (Complex a) where
    random g = let (a,g') = random g
                   (b,g'') = random g'
               in (Complex $ a C.+: b, g'')

    randomR = error "randomR not defined for (Complex t)"

instance (R.Elt a) => R.Elt (Complex a) where
    touch (Complex c) = do
        touch $ C.real c
        touch $ C.imag c
    zero = Complex $ R.zero C.+: R.zero
    one = Complex $ R.one C.+: R.zero

-- | Rounds the real and imaginary components to the nearest integer.
roundComplex :: (RealRing a, ToInteger b) => Complex a -> (b,b)
roundComplex (Complex x) = (round $ C.real x, round $ C.imag x)

-- | 'cis' @t@ is a complex value with magnitude 1 and phase t (modulo @2*Pi@).
cis :: Transcendental a => a -> Complex a
cis = Complex . C.cis

-- | Real component of a complex number.
real :: Complex a -> a
real (Complex a) = C.real a

-- | Imaginary component of a complex number.
imag :: Complex a -> a
imag (Complex a) = C.imag a

-- | Embeds a scalar as the real component of a complex number.
fromReal :: Additive a => a -> Complex a
fromReal = Complex . C.fromReal
