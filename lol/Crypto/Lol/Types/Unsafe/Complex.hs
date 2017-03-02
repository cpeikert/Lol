{-|
Module      : Crypto.Lol.Types.Unsafe.Complex
Description : Data type, functions, and instances for complex numbers.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Data type, functions, and instances for complex numbers.
This module is "unsafe" because it exports the 'Complex' constructor.
This module should only be used to make tensor-specific instances for 'Complex'.
The safe way to use this type is to import "Crypto.Lol.Types".
-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Crypto.Lol.Types.Unsafe.Complex (
  Complex(..)
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

import System.Random

-- | Newtype wrapper (with slightly different instances) for
-- @Number.Complex@.
newtype Complex a = Complex' (C.T a)
    deriving (Additive.C, Ring.C, ZeroTestable.C, Field.C, Eq, Show)

-- | Custom instance replacing the one provided by numeric prelude: it
-- always returns 0 as the remainder of a division.  (The NP instance
-- sometimes has precision issues, because it yields nonzero
-- remainders, which is a problem for 'divG' methods.)
instance (Field a) => IntegralDomain.C (Complex a) where
  (Complex' a) `divMod` (Complex' b) = (Complex' $ a / b, LP.zero)

-- we can't use Generics for NFData because NP doesn't export the
-- (deep) constructor for Complex.T
instance (NFData a) => NFData (Complex a) where
  rnf (Complex' x) = let r = C.real x
                         i = C.imag x
                     in rnf r `seq` rnf i `seq` ()

instance (Random a) => Random (Complex a) where
    random g = let (a,g') = random g
                   (b,g'') = random g'
               in (Complex' $ a C.+: b, g'')

    randomR = error "randomR not defined for (Complex t)"

-- | Rounds the real and imaginary components to the nearest integer.
roundComplex :: (RealRing a, ToInteger b) => Complex a -> (b,b)
roundComplex (Complex' x) = (round $ C.real x, round $ C.imag x)

-- | 'cis' \(t\) is a complex value with magnitude 1 and phase \(t \bmod 2\cdot\pi\)).
cis :: Transcendental a => a -> Complex a
cis = Complex' . C.cis

-- | Real component of a complex number.
real :: Complex a -> a
real (Complex' a) = C.real a

-- | Imaginary component of a complex number.
imag :: Complex a -> a
imag (Complex' a) = C.imag a

-- | Embeds a scalar as the real component of a complex number.
fromReal :: Additive a => a -> Complex a
fromReal = Complex' . C.fromReal
