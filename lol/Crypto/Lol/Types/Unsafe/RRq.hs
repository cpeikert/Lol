{-|
Module      : Crypto.Lol.Types.Unsafe.RRq
Description : An implementation of modular arithmetic over the reals.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\R{\mathbb{R}} \)

An implementation of the additive quotient group \(\R/(q\Z)\).
This module is "unsafe" because it exports the 'RRq' constructor.
This module should only be used to make tensor-specific instances for 'RRq'.
The safe way to use this type is to import "Crypto.Lol.Types".
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Types.Unsafe.RRq
( RRq(..)
) where

import Algebra.Additive     as Additive (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.DeepSeq
import System.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic hiding (ZqB)

-- invariant: 0 <= x < 1, scaled
-- | The additive group \( \R/(q\Z) \) of reals modulo 'q', using
-- underlying floating-point type 'r'.
newtype RRq q r = RRq r
    deriving (Eq, ZeroTestable.C, Show, NFData)

instance RealField r => Additive.C (RRq q r) where
  {-# INLINABLE zero #-}
  zero = RRq zero

  {-# INLINABLE (+) #-}
  (RRq x) + (RRq y) = RRq $ fraction $ x + y

  {-# INLINABLE negate #-}
  negate (RRq x) = RRq $ fraction $ negate x

reduce' :: forall q r . (Reflects q r, RealField r) => r -> RRq q r
reduce' r = RRq $ fraction $ r / value @q -- scale down

instance (Reflects q r, RealField r, Additive (RRq q r))
  => Reduce r (RRq q r) where
  reduce = reduce'

type instance LiftOf (RRq q r) = r

instance (Reflects q r, Field r, Reduce r (RRq q r))
  => Lift' (RRq q r) where
  lift (RRq r) = (r-0.5) * value @q

instance (Additive (RRq q r), Additive (RRq p r))
  => Rescale (RRq q r) (RRq p r) where
  rescale (RRq r) = RRq r

instance Random (RRq q Double) where
  random g = let (r,g') = random g in (RRq r, g')
  randomR = error "randomR is nonsensical for RRq"

instance (ToInteger z, RealField r, Reflects q z, Reflects q r)
  => Subgroup (ZqBasic q z) (RRq q r) where
  fromSubgroup = reduce' . fromIntegral . lift
