{-|
Module      : Crypto.Lol.Cyclotomic.Language
Description : Abstract interfaces for operations on cyclotomic rings.
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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Lol.Cyclotomic.Language
where

import Crypto.Lol.Prelude

import Control.Monad.Random (MonadRandom)

-- | Used to specify a basis for cyclotomic operations
data Basis = Pow | Dec

-- | Operations on cyclotomics.
class Cyclotomic cmr where
  -- | Multiply by the special element \( g \).
  mulG :: cmr -> cmr

  -- | Divide by the special element \( g \), returning 'Nothing' if
  -- the input is not evenly divisible.
  divG :: cmr -> Maybe cmr

  -- | Yield an equivalent element that /may/ be in
  -- powerful\/decoding\/CRT representation.  This can serve as an
  -- optimization hint. E.g., call 'adviseCRT' prior to multiplying a
  -- value by many other values.
  advisePow, adviseDec, adviseCRT :: cmr -> cmr

class GSqNormCyc cm r where
  -- | Yield the scaled squared norm of \( g_m \cdot e \) under the
  -- canonical embedding, namely, \( \hat{m}^{-1} \cdot \| \sigma(g_m
  -- \cdot e) \|^2 \).
  gSqNorm :: cm r -> r

-- | Sampling from tweaked Gaussian distributions over cyclotomic
-- number fields.
class GaussianCyc cmq where
  -- | Sample from the "tweaked" Gaussian distribution \( t \cdot D
  -- \), where \( D \) has scaled variance \( v \).
  tweakedGaussian :: (ToRational v, MonadRandom rnd) => v -> rnd cmq

-- | Convenient type synonym that looks like a class constraint.
type RoundedGaussianCyc cm z =
  (ToInteger z, GaussianCyc (cm Double), FunctorCyc cm Double z)

-- | Sample from the tweaked Gaussian with given scaled variance,
-- deterministically rounded using the decoding basis.
roundedGaussian :: forall cm z v rnd .
  (RoundedGaussianCyc cm z, ToRational v, MonadRandom rnd)
  => v -> rnd (cm z)
roundedGaussian svar = fmapCyc (Just Dec) (roundMult one) <$>
                       (tweakedGaussian svar :: rnd (cm Double))

{-

-- | Sampling from /discretized/ tweaked Gaussian distributions over
-- cyclotomic number rings.
class RoundedGaussianCyc cmz where
  -- | Sample from the tweaked Gaussian with given scaled variance,
  -- deterministically rounded using the decoding basis.
  roundedGaussian :: (ToRational v, MonadRandom rnd) => v -> rnd cmz

-}

-- CJP TODO: see if we can implement cosetGaussian generically using
-- ZipWithCyc class

-- | Sampling from tweaked Gaussian distributions, discretized to
-- mod-p cosets of cyclotomic number rings.
class CosetGaussianCyc rp where
  -- | Sample from the tweaked Gaussian with scaled variance \( v
  -- \cdot p^2 \), deterministically rounded to the given coset of
  -- \( R_p \) using the decoding basis.
  cosetGaussian :: (ToRational v, MonadRandom rnd)
                => v -> rp -> rnd (LiftOf rp)

-- | Cyclotomic extensions \( \O_{m'}/\O_m \).
class ExtensionCyc c r where
  -- | Embed into a cyclotomic extension.
  embed :: (m `Divides` m') => c m r -> c m' r

  -- | The "tweaked trace" (twace) \( \Tw(x) = (\hat{m} / \hat{m}')
  -- \cdot \Tr((g' / g) \cdot x) \), which is the left-inverse of
  -- 'embed' (i.e., @twace . embed == id@).
  twace :: (m `Divides` m') => c m' r -> c m r

  -- | The relative powerful/decoding bases of the extension.
  powBasis :: (m `Divides` m') => Tagged m [c m' r]

  -- | Yield the coefficient vector with respect to the given
  -- (relative) basis of the extension.
  coeffsCyc :: (m `Divides` m') => Basis -> c m' r -> [c m r]

coeffsPow, coeffsDec :: (ExtensionCyc c r, m `Divides` m') => c m' r -> [c m r]
-- | 'coeffsCyc' specialized to the powerful basis.
coeffsPow = coeffsCyc Pow
-- | 'coeffsCyc' specialized to the decoding basis.
coeffsDec = coeffsCyc Dec

-- | Relative CRT sets of cyclotomic extensions.
class ExtensionCyc c r => CRTSetCyc c r where
  -- | The relative mod-@r@ CRT set of the extension.
  crtSet :: (m `Divides` m') => Tagged m [c m' r]

-- | Map over coefficients in a specified basis.
class FunctorCyc cm a b where
  -- | Map in the specified basis (where 'Nothing' indicates that
  -- any 'Basis' may be used).
  fmapCyc :: Maybe Basis -> (a -> b) -> cm a -> cm b

-- | Convenient specializations of 'fmapCyc'.
fmapAny, fmapPow, fmapDec :: FunctorCyc cm a b => (a -> b) -> cm a -> cm b
fmapAny = fmapCyc   Nothing
fmapPow = fmapCyc $ Just Pow
fmapDec = fmapCyc $ Just Dec

-- | Reduce on a cyclotomic (in an arbitrary basis).
reduceCyc :: (FunctorCyc cm a b, Reduce a b) => cm a -> cm b
reduceCyc = fmapAny reduce

-- | Convenient type synonym that looks like a class constraint.
type LiftCyc cm a = (Lift' a, FunctorCyc cm a (LiftOf a))

-- | Lift a cyclotomic in the specified basis (or any basis).
liftCyc :: (LiftCyc cm a) => Maybe Basis -> cm a -> cm (LiftOf a)
liftCyc = flip fmapCyc lift

liftAny, liftPow, liftDec :: (LiftCyc cm a) => cm a -> cm (LiftOf a)
liftAny = liftCyc   Nothing
liftPow = liftCyc $ Just Pow
liftDec = liftCyc $ Just Dec

-- | Rescaling on cyclotomics from one base ring to another. (This is
-- a separate class because there are optimized rescaling algorithms
-- that can't be implemented using 'FunctorCyc'.)
class RescaleCyc cm a b where
  -- | Rescale in the given basis.
  rescaleCyc :: Basis -> cm a -> cm b

rescalePow, rescaleDec :: (RescaleCyc cm a b) => cm a -> cm b
-- | 'rescaleCyc' specialized to the powerful basis.
rescalePow = rescaleCyc Pow
-- | 'rescaleCyc' specialized to the decoding basis.
rescaleDec = rescaleCyc Dec

