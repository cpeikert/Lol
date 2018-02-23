{-|
Module      : Crypto.Lol.Cyclotomic.Language
Description : Interfaces for operations on cyclotomic rings.
Copyright   : (c) Chris Peikert, 2018-
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\F{\mathbb{F}} \)
  \( \def\Q{\mathbb{Q}} \)
  \( \def\Tw{\text{Tw}} \)
  \( \def\Tr{\text{Tr}} \)
  \( \def\O{\mathcal{O}} \)
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Lol.Cyclotomic.Language
where

import Crypto.Lol.Factored

import Control.Monad.Random      (MonadRandom)
import Data.Functor.Trans.Tagged

-- | Used to specify a basis for cyclotomic operations
data Basis = Pow | Dec

-- | Operations on cyclotomics.
class Cyclotomic c r where
  -- | Embed a scalar.
  scalarCyc :: Fact m => r -> c m r

  -- | Multiply by the special element \( g \).
  mulG :: Fact m => c m r -> c m r

  -- | Divide by the special element \( g \), returning 'Nothing' if
  -- the input is not evenly divisible.
  divG :: Fact m => c m r -> Maybe (c m r)

  -- CJP: does the following make sense for arbitrary r? Or should be
  -- it be put in its own class for specific choices, like Z and Q?
  -- It's special and rarely used.

  -- | Yield the scaled squared norm of \( g_m \cdot e \) under the
  -- canonical embedding, namely, \(\hat{m}^{-1} \cdot \| \sigma(g_m
  -- \cdot e) \|^2\).
  gSqNorm :: Fact m => c m r -> r

  -- | Yield an equivalent element that /may/ be in a CRT
  -- representation.  This can serve as an optimization hint. E.g.,
  -- call 'adviseCRT' prior to multiplying the same value by many
  -- other values.
  adviseCRT :: Fact m => c m r -> c m r
  -- | Same as 'adviseCRT', but for the powerful basis.
  advisePow :: Fact m => c m r -> c m r
  -- | Same as 'adviseCRT', but for the decoding basis.
  adviseDec :: Fact m => c m r -> c m r

class GaussianCyc c q where
  tweakedGaussian :: (Fact m, ToRational v, MonadRandom rnd)
    => v -> rnd (Cyc t m q)

-- | Cyclotomic extensions \( \O_{m'}/\O_m \).
class ExtensionCyc c r where
  -- | Embed into a cyclotomic extension.
  embed :: (m `Divides` m') => c m r -> c m' r

  -- | The "tweaked trace" (twace) \( \Tw(x) = (\hat{m} / \hat{m}')
  -- \cdot \Tr((g' / g) \cdot x) \), which is the left-inverse of
  -- 'embed' (i.e., @twace . embed == id@).
  twace :: (m `Divides` m') => c m' r -> c m r

  -- | The relative powerful basis of the extension.
  powBasis :: (m `Divides` m') => Tagged m [c m' r]

  -- | Yield the coefficient vector with respect to the given
  -- (relative) basis of the extension.
  coeffsCyc :: (m `Divides` m') => Basis -> c m' r -> [c m r]

coeffsPow, coeffsDec :: (ExtensionCyc c r, m `Divides` m') => c m' r -> [c m r]
-- | 'coeffsCyc' specialized to the powerful basis.
coeffsPow = coeffsCyc Pow
-- | 'coeffsCyc' specialized to the decoding basis.
coeffsDec = coeffsCyc Dec

class LiftCyc c a b where
  -- | Lift using the specified basis.
  liftCyc :: Fact m => Basis -> c m a -> c m b

liftPow, liftDec :: (LiftCyc c a b, Fact m) => c m a -> c m b
-- | 'liftCyc' specialized to the powerful basis.
liftPow = liftCyc Pow
-- | 'liftCyc' specialized to the decoding basis.
liftDec = liftCyc Dec

-- | Rescaling on cyclotomics from one base ring to another.
class RescaleCyc c a b where
  -- | Rescale in the given basis.
  rescaleCyc :: Fact m => Basis -> c m a -> c m b

rescalePow, rescaleDec :: (RescaleCyc c a b, Fact m) => c m a -> c m b
-- | 'rescaleCyc' specialized to the powerful basis.
rescalePow = rescaleCyc Pow
-- | 'rescaleCyc' specialized to the decoding basis.
rescaleDec = rescaleCyc Dec

