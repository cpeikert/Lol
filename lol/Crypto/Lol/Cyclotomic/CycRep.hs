{-|
Module      : Crypto.Lol.Cyclotomic.CycRep
Description : A low-level implementation of cyclotomic rings.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\F{\mathbb{F}} \)
  \( \def\Q{\mathbb{Q}} \)
  \( \def\O{\mathcal{O}} \)

A low-level implementation of cyclotomic rings that allows (and
requires) the programmer to control the underlying representation
of ring elements, i.e., powerful, decoding, or CRT basis.

__WARNING:__ as with all fixed-point arithmetic, the functions
associated with 'CycRep' may result in overflow (and thereby
incorrect answers and potential security flaws) if the input
arguments are too close to the bounds imposed by the base type.
The acceptable range of inputs for each function is determined by
the internal linear transforms and other operations it performs.
-}

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Cyclotomic.CycRep
(
-- * Data types and constraints
  CycRep, P, D, C, E, CycRepEC, CycRepPC, CRTElt
-- * Changing representation
, toPow, toDec, toCRT
-- * Scalars
, scalarPow, scalarCRT
-- * Basic operations
, mulGPow, mulGDec, mulGCRTC, divGPow, divGDec, divGCRTC, gSqNormDec
-- * Error sampling
, tweakedGaussian, roundedGaussian, cosetGaussian
-- * Inter-ring operations and values
, embedPow, embedCRTC, embedCRTE
, twacePow, twaceDec, twaceCRTC, twaceCRTE
, coeffsPow, coeffsDec, powBasis, crtSet
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.CRTSentinel
import Crypto.Lol.Cyclotomic.Tensor      hiding (divGDec, divGPow,
                                          embedCRT, embedPow, gSqNormDec,
                                          mulGDec, mulGPow, scalarCRT,
                                          scalarPow, twaceCRT)
import Crypto.Lol.Prelude                as LP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.IFunctor
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Unsafe.ZqBasic

import qualified Crypto.Lol.Cyclotomic.Tensor as T

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Module       as Module (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative    as A
import Control.DeepSeq
import Control.Monad.Identity (Identity (..))
import Control.Monad.Random   hiding (ap, lift)
import Data.Foldable          as F
import Data.Traversable

--import qualified Debug.Trace as DT

-- | Represents a cyclotomic ring such as \(\Z[\zeta_m]\),
-- \(\Z_q[\zeta_m]\), and \(\Q(\zeta_m)\) in an explicit
-- representation: @t@ is the 'TensorPowDec' type for storing coefficient
-- tensors; @m@ is the cyclotomic index; @rep@ is the representation
-- (e.g., 'P', 'D', 'C', 'E'); @r@ is the base ring of the
-- coefficients (e.g., \(\Z\), \(\Z_q\)).

-- | Nullary index type representing the powerful basis.
data P
-- | Nullary index type representing the decoding basis.
data D
-- | Nullary index type representing the CRT basis over base ring.
data C
-- | Nullary index type representing the CRT basis over extension of
-- base ring.
data E

data family CycRep (t :: Factored -> * -> *) rep (m :: Factored) r

newtype instance CycRep t P m r = Pow  (t m r) deriving (Eq, ZeroTestable.C)
newtype instance CycRep t D m r = Dec  (t m r) deriving (Eq, ZeroTestable.C)

-- C/ESentinel enforces invariant that exactly one of the following
-- two can be created for a given (t,m,r).
data    instance CycRep t C m r = CRTC !(CSentinel t m r) !(t m r) deriving (Eq)
-- can't derive ZT due to sentinel

data    instance CycRep t E m r = CRTE !(ESentinel t m r) !(t m (CRTExt r))
-- no Eq due to precision

{- CJP: old GADT definition of CycRep, which seems less
   runtime-efficient than the data family definition.

data CycRep t rep (m :: Factored) r where
  Pow  :: !(t m r) -> CycRep t P m r
  Dec  :: !(t m r) -> CycRep t D m r
  CRTC :: !(CSentinel t m r) -> !(t m r) -> CycRep t C m r
  CRTE :: !(ESentinel t m r) -> !(t m (CRTExt r)) -> CycRep t E m r
-}

-- | Convenient synonym for either CRT representation.
type CycRepEC t m r = Either (CycRep t E m r) (CycRep t C m r)

-- | Convenient synonym for random sampling.
type CycRepPC t m r = Either (CycRep t P m r) (CycRep t C m r)

-- | Constraints needed for CRT-related operations on 'CycRep' data.
type CRTElt t r = (TensorG t r, CRTEmbed r,
                   TensorCRT t Maybe r, TensorCRT t Identity (CRTExt r))

-- | Embed a scalar from the base ring.
scalarPow :: (TensorPowDec t r, Fact m) => r -> CycRep t P m r
scalarPow = Pow . T.scalarPow
{-# INLINABLE scalarPow #-}

-- | Embed a scalar from the base ring.
scalarCRT :: (Fact m, CRTElt t r) => r -> CycRepEC t m r
scalarCRT r = case crtSentinel of
  Right s -> Right $ CRTC s $ scalarCRTCS s r
  Left  s -> Left  $ CRTE s $ runIdentity T.scalarCRT $ toExt r
{-# INLINABLE scalarCRT #-}


---------- Numeric Prelude instances ----------

-- ZeroTestable instances

{- CJP: only need these instances for GADT version of CycRep; for data
   family they are auto-derived.

instance ZeroTestable (t m r) => ZeroTestable.C (CycRep t P m r) where
  isZero (Pow v) = isZero v
  {-# INLINABLE isZero #-}

instance ZeroTestable (t m r) => ZeroTestable.C (CycRep t D m r) where
  isZero (Dec v) = isZero v
  {-# INLINABLE isZero #-}

-}

-- ZT for P,D auto-derived

instance ZeroTestable (t m r) => ZeroTestable.C (CycRep t C m r) where
  -- can't derive this because of sentinel
  isZero (CRTC _ v) = isZero v
  {-# INLINABLE isZero #-}

-- no ZT instance for E due to precision

-- Additive instances

-- TODO: replace these implementations to use Additive instance of
-- underlying tensor? Would this require (forall m . Fact m => Additive (t m))?

instance (TensorPowDec t r, Fact m) => Additive.C (CycRep t P m r) where
  {-# SPECIALIZE instance (TensorPowDec t Int64, Fact m) => Additive.C (CycRep t P m Int64) #-}
  {-# SPECIALIZE instance (TensorPowDec t Double, Fact m) => Additive.C (CycRep t P m Double) #-}
  {-# SPECIALIZE instance (TensorPowDec t (ZqBasic q Int64), Fact m) => Additive.C (CycRep t P m (ZqBasic q Int64)) #-}

  zero = Pow $ T.scalarPow zero
  (Pow v1) + (Pow v2) = Pow $ zipWithI (+) v1 v2
  (Pow v1) - (Pow v2) = Pow $ zipWithI (-) v1 v2
  negate (Pow v) = Pow $ fmapI negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (TensorPowDec t r, Fact m) => Additive.C (CycRep t D m r) where
  {-# SPECIALIZE instance (TensorPowDec t Int64, Fact m) => Additive.C (CycRep t D m Int64) #-}
  {-# SPECIALIZE instance (TensorPowDec t Double, Fact m) => Additive.C (CycRep t D m Double) #-}
  {-# SPECIALIZE instance (TensorPowDec t (ZqBasic q Int64), Fact m) => Additive.C (CycRep t D m (ZqBasic q Int64)) #-}

  zero = Dec $ T.scalarPow zero -- scalarPow works because it's zero
  (Dec v1) + (Dec v2) = Dec $ zipWithI (+) v1 v2
  (Dec v1) - (Dec v2) = Dec $ zipWithI (-) v1 v2
  negate (Dec v) = Dec $ fmapI negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- | only for appropriate CRT representation (otherwise 'zero' would
-- violate internal invariant)
instance (Fact m, CRTElt t r) => Additive.C (CycRepEC t m r) where
  {-# SPECIALIZE instance (Fact m, CRTElt t Int64) => Additive.C (CycRepEC t m Int64) #-}
  {-# SPECIALIZE instance (Fact m, CRTElt t Double) => Additive.C (CycRepEC t m Double) #-}
  {-# SPECIALIZE instance (Fact m, CRTElt t (ZqBasic q Int64)) => Additive.C (CycRepEC t m (ZqBasic q Int64)) #-}

  zero = scalarCRT zero

  -- CJP: precision OK?  Alternatively, switch to pow and back after
  -- adding/subtracting.  Expensive, but necessary given output type.
  (Right (CRTC s v1)) + (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (+) v1 v2
  (Left (CRTE s v1)) + (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (+) v1 v2
  _ + _ = error "CycRep (+) internal error: mixed CRTC/CRTE"

  (Right (CRTC s v1)) - (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (-) v1 v2
  (Left (CRTE s v1)) - (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (-) v1 v2
  _ - _ = error "CycRep (-) internal error: mixed CRTC/CRTE"

  negate (Right (CRTC s v)) = Right $ CRTC s $ fmapI negate v
  negate (Left (CRTE s v))  = Left $ CRTE s $ fmapI negate v

  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- | only for appropriate CRT representation
instance (Fact m, CRTElt t r) => Ring.C (CycRepEC t m r) where
  {-# SPECIALIZE instance (Fact m, CRTElt t Int64) => Ring.C (CycRepEC t m Int64) #-}
  {-# SPECIALIZE instance (Fact m, CRTElt t Double) => Ring.C (CycRepEC t m Double) #-}
  {-# SPECIALIZE instance (Fact m, CRTElt t (ZqBasic q Int64)) => Ring.C (CycRepEC t m (ZqBasic q Int64)) #-}

  one = scalarCRT one
  fromInteger c = scalarCRT $ fromInteger c

  (Right (CRTC s v1)) * (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (*) v1 v2
  (Left (CRTE s v1)) * (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (*) v1 v2
  _ * _ = error "CycRep internal error: mixed CRTC/CRTE"

  {-# INLINABLE one #-}
  {-# INLINABLE fromInteger #-}
  {-# INLINABLE (*) #-}


instance (Ring r, TensorPowDec t r, Fact m) => Module.C r (CycRep t P m r) where
  {-# SPECIALIZE instance (Fact m, TensorPowDec t Int64) => Module.C Int64 (CycRep t P m Int64) #-}
  {-# SPECIALIZE instance (Fact m, TensorPowDec t Double) => Module.C Double (CycRep t P m Double) #-}
  {-# SPECIALIZE instance (Fact m, TensorPowDec t (ZqBasic q Int64), Reflects q Int64) => Module.C (ZqBasic q Int64) (CycRep t P m (ZqBasic q Int64)) #-}

  r *> (Pow v) = Pow $ fmapI (r *) v
  {-# INLINABLE (*>) #-}

instance (Ring r, TensorPowDec t r, Fact m) => Module.C r (CycRep t D m r) where
  {-# SPECIALIZE instance (Fact m, TensorPowDec t Int64) => Module.C Int64 (CycRep t D m Int64) #-}
  {-# SPECIALIZE instance (Fact m, TensorPowDec t Double) => Module.C Double (CycRep t D m Double) #-}
  {-# SPECIALIZE instance (Fact m, TensorPowDec t (ZqBasic q Int64), Reflects q Int64) => Module.C (ZqBasic q Int64) (CycRep t D m (ZqBasic q Int64)) #-}

  r *> (Dec v) = Dec $ fmapI (r *) v
  {-# INLINABLE (*>) #-}

instance (CRTElt t r, Fact m) => Module.C r (CycRepEC t m r) where
  {-# SPECIALIZE instance (CRTElt t Int64, Fact m) => Module.C Int64 (CycRepEC t m Int64) #-}
  {-# SPECIALIZE instance (CRTElt t Double, Fact m) => Module.C Double (CycRepEC t m Double) #-}
  {-# SPECIALIZE instance (CRTElt t (ZqBasic q Int64), Fact m) => Module.C (ZqBasic q Int64) (CycRepEC t m (ZqBasic q Int64)) #-}

  r *> (Right (CRTC s v)) = Right $ CRTC s $ fmapI (r *) v
  r *> (Left (CRTE s v)) = Left $ CRTE s $ fmapI (toExt r *) v
  {-# INLINABLE (*>) #-}

-- | \(R_p\) is an \(\F_{p^d}\)-module when \(d\) divides \(\varphi(m)\), by
-- applying \(d\)-dimensional \(\F_p\)-linear transform on \(d\)-dim chunks of
-- powerful basis coeffs.
instance (GFCtx fp d, Fact m, TensorPowDec t fp, Module (GF fp d) (t m fp))
         => Module.C (GF fp d) (CycRep t P m fp) where
  -- can use any r-basis to define module mult, but must be
  -- consistent.
  r *> (Pow v) = Pow $ r LP.*> v


instance (Fact m, Reduce a b, IFunctor t, IFElt t a, IFElt t b)
         => Reduce (CycRep t P m a) (CycRep t P m b) where
  {-# SPECIALIZE instance (Fact m, Reflects q Int64, IFunctor t, IFElt t Int64, IFElt t (ZqBasic q Int64)) => Reduce (CycRep t P m Int64) (CycRep t P m (ZqBasic q Int64)) #-}

  reduce (Pow v) = Pow $ fmapI reduce v
  {-# INLINABLE reduce #-}

instance (Fact m, Reduce a b, IFunctor t, IFElt t a, IFElt t b)
    => Reduce (CycRep t D m a) (CycRep t D m b) where
  {-# SPECIALIZE instance (Fact m, Reflects q Int64, IFunctor t, IFElt t Int64, IFElt t (ZqBasic q Int64)) => Reduce (CycRep t D m Int64) (CycRep t D m (ZqBasic q Int64)) #-}

  reduce (Dec v) = Dec $ fmapI reduce v
  {-# INLINABLE reduce #-}

-- CJP: no Reduce for C because CRT basis may not exist for target
-- type

type instance LiftOf (CycRep t P m r) = CycRep t P m (LiftOf r)
type instance LiftOf (CycRep t D m r) = CycRep t D m (LiftOf r)

instance (Fact m, Lift' r, IFunctor t, IFElt t r, IFElt t (LiftOf r))
         => Lift' (CycRep t P m r) where
  {-# SPECIALIZE instance (Fact m, Reflects q Int64, IFunctor t, IFElt t Int64, IFElt t (ZqBasic q Int64)) => Lift' (CycRep t P m (ZqBasic q Int64)) #-}

  lift (Pow v) = Pow $ fmapI lift v
  {-# INLINABLE lift #-}

instance (Lift' r, IFunctor t, IFElt t r, IFElt t (LiftOf r), Fact m)
         => Lift' (CycRep t D m r) where
  {-# SPECIALIZE instance (Fact m, Reflects q Int64, IFunctor t, IFElt t Int64, IFElt t (ZqBasic q Int64)) => Lift' (CycRep t D m (ZqBasic q Int64)) #-}

  lift (Dec v) = Dec $ fmapI lift v
  {-# INLINABLE lift #-}

-- CJP: no Lift' for C because CRT basis may not exist for target type

instance (Rescale a b, TensorPowDec t a, TensorPowDec t b, Fact m)
         => Rescale (CycRep t P m a) (CycRep t P m b) where
  rescale (Pow v) = Pow $ fmapI rescale v
  {-# INLINABLE rescale #-}

instance (Rescale a b, TensorPowDec t a, TensorPowDec t b, Fact m)
         => Rescale (CycRep t D m a) (CycRep t D m b) where
  rescale (Dec v) = Dec $ fmapI rescale v
  {-# INLINABLE rescale #-}

-- CJP: no Rescale for C because CRT basis may not exist for target
-- type

-- CJP: we don't instantiate RescaleCyc because it requires changing bases

-- CJP: we don't instantiate Gadget etc., because (1) their methods
-- wouldn't be efficient, and (2) their superclass constraints are not
-- satisfied anyway (e.g., Ring for P rep).

-- | multiply by the special element @g@
mulGPow :: (Fact m, TensorG t r) => CycRep t P m r -> CycRep t P m r
{-# INLINABLE mulGPow #-}
mulGPow (Pow v) = Pow $ T.mulGPow v

-- | multiply by the special element @g@
mulGDec :: (Fact m, TensorG t r) => CycRep t D m r -> CycRep t D m r
{-# INLINABLE mulGDec #-}
mulGDec (Dec v) = Dec $ T.mulGDec v

-- | multiply by the special element @g@
mulGCRTC :: (Fact m, TensorCRT t Maybe r)
         => CycRep t C m r -> CycRep t C m r
{-# INLINABLE mulGCRTC #-}
mulGCRTC (CRTC s v) = CRTC s $ mulGCRTCS s v

-- Note: We do not implement divGCRTE because we can't tell whether
-- the element is actually divisible by g when using the CRT extension
-- basis.

-- | Divide by the special element \(g_m\).
-- WARNING: this implementation is not a constant-time algorithm, so
-- information about the argument may be leaked through a timing
-- channel.
divGPow :: (Fact m, TensorG t r) => CycRep t P m r -> Maybe (CycRep t P m r)
{-# INLINABLE divGPow #-}
divGPow (Pow v) = Pow <$> T.divGPow v

-- | Similar to 'divGPow'.
divGDec :: (Fact m, TensorG t r) => CycRep t D m r -> Maybe (CycRep t D m r)
{-# INLINABLE divGDec #-}
divGDec (Dec v) = Dec <$> T.divGDec v

-- | Similar to 'divGPow'.
divGCRTC :: (Fact m, CRTElt t r) => CycRep t C m r -> CycRep t C m r
{-# INLINE divGCRTC #-}
divGCRTC (CRTC s v) = CRTC s $ divGCRTCS s v

-- | Yield the scaled squared norm of \(g_m \cdot e\) under
-- the canonical embedding, namely,
-- \(\hat{m}^{-1} \cdot \| \sigma(g_m \cdot e) \|^2\) .
gSqNormDec :: (TensorGSqNorm t r, Fact m) => CycRep t D m r -> r
{-# INLINABLE gSqNormDec #-}
gSqNormDec (Dec v) = T.gSqNormDec v

-- | Sample from the "tweaked" Gaussian error distribution \(t\cdot D\) in
-- the decoding basis, where \(D\) has scaled variance \(v\).
tweakedGaussian :: (TensorGaussian t q, MonadRandom rnd, Fact m, ToRational v)
                   => v -> rnd (CycRep t D m q)
{-# INLINABLE tweakedGaussian #-}
tweakedGaussian = fmap Dec . tweakedGaussianDec

-- | Sample from the tweaked Gaussian with given scaled variance,
-- deterministically rounded using the decoding basis. (This
-- implementation uses 'Double' precision to generate the Gaussian
-- sample, which might not be sufficient for rigorous proof-based
-- security.)
roundedGaussian :: forall v rnd t m z .
  (TensorGaussian t Double, IFElt t Double, IFunctor t, ToInteger z,
   IFElt t z, Fact m, ToRational v, MonadRandom rnd)
  => v -> rnd (CycRep t D m z)
{-# INLINABLE roundedGaussian #-}
roundedGaussian svar =
  Dec . fmapI (roundMult one) <$> (tweakedGaussianDec svar :: rnd (t m Double))

-- | Sample from the tweaked Gaussian with scaled variance \(v \cdot
-- p^2\), deterministically rounded to the given coset of \(R_p\)
-- using the decoding basis. (This implementation uses 'Double'
-- precision to generate the Gaussian sample, which may not be
-- sufficient for rigorous proof-based security.)
cosetGaussian :: forall t m zp z v rnd .
  (TensorGaussian t Double, IFElt t Double, IFunctor t, Lift zp z, Mod zp,
   z ~ ModRep zp, IFElt t zp, IFElt t z, Fact m, ToRational v, MonadRandom rnd)
  => v -> CycRep t D m zp -> rnd (CycRep t D m z)
{-# INLINABLE cosetGaussian #-}
cosetGaussian =
  let pval = fromIntegral $ modulus @zp
  in \ svar (Dec c) ->
    Dec . zipWithI roundCoset c <$>
    (tweakedGaussianDec (svar*pval*pval) :: rnd (t m Double))


----- inter-ring operations

-- | Embed into an extension ring, for the powerful basis.
embedPow :: (TensorPowDec t r, m `Divides` m') => CycRep t P m r -> CycRep t P m' r
embedPow (Pow v) = Pow $ T.embedPow v
{-# INLINABLE embedPow #-}

-- | Embed into an extension ring, for the CRT basis.  (The output is
-- an 'Either' because the extension ring might not support 'C'.)
embedCRTC :: (m `Divides` m', CRTElt t r)
             => CycRep t C m r -> Either (CycRep t P m' r) (CycRep t C m' r)
{-# INLINABLE embedCRTC #-}
embedCRTC x@(CRTC s v) =
  case crtSentinel of
    -- go to CRTC if valid, else go to Pow
    Left  _  -> Left $ embedPow $ toPow x
    Right s' -> Right $ CRTC s' $ embedCRTCS s s' v

-- | Similar to 'embedCRTC'.  (The output is an 'Either' because the
-- extension ring might support 'C', in which case we never use 'E'.)
embedCRTE :: forall m m' t r . (m `Divides` m', CRTElt t r)
             => CycRep t E m r -> Either (CycRep t P m' r) (CycRep t E m' r)
{-# INLINABLE embedCRTE #-}
embedCRTE x@(CRTE _ v) =
  case crtSentinel of
    -- go to CRTE if valid, else go to Pow
    Left  s -> Right $ CRTE s $ runIdentity T.embedCRT v
    Right _ -> Left $ embedPow $ toPow x

-- | Twace into a subring, for the powerful basis.
twacePow :: (TensorPowDec t r, m `Divides` m')
         => CycRep t P m' r -> CycRep t P m r
{-# INLINABLE twacePow #-}
twacePow (Pow v) = Pow $ twacePowDec v

-- | Twace into a subring, for the decoding basis.
twaceDec :: (TensorPowDec t r, m `Divides` m') => CycRep t D m' r -> CycRep t D m r
{-# INLINABLE twaceDec #-}
twaceDec (Dec v) = Dec $ twacePowDec v

-- | Twace into a subring, for the CRT basis.  (The output is an
-- 'Either' because the subring might not support 'C'.)
twaceCRTC :: (m `Divides` m', CRTElt t r) => CycRep t C m' r -> CycRepPC t m r
{-# INLINE twaceCRTC #-}
twaceCRTC x@(CRTC s' v) =
  case crtSentinel of
    -- go to CRTC if valid for target, else go to Pow
    Left  _ -> Left $ twacePow $ toPow x
    Right s -> Right $ CRTC s $ twaceCRTCS s' s v

-- | Similar to 'twaceCRTC'.  (The output is an 'Either' because the
-- subring might support 'C', in which case we never use 'E'.)
twaceCRTE :: forall t m m' r . (m `Divides` m', CRTElt t r)
             => CycRep t E m' r -> Either (CycRep t P m r) (CycRep t E m r)
{-# INLINABLE twaceCRTE #-}
twaceCRTE x@(CRTE _ v) =
  case crtSentinel of
    -- go to CRTE if valid for target, else go to Pow
    Left  s -> Right $ CRTE s $ runIdentity T.twaceCRT v
    Right _ -> Left $ twacePow $ toPow x

-- | Yield the \(\O_m\)-coefficients of an \(\O_{m'}\)-element,
-- with respect to the relative powerful \(\O_m\)-basis.
coeffsPow :: (TensorPowDec t r, m `Divides` m') => CycRep t P m' r -> [CycRep t P m r]
{-# INLINABLE coeffsPow #-}
coeffsPow (Pow v) = Pow <$> coeffs v

-- | Yield the \(\O_m\)-coefficients of an \(\O_{m'}\) element,
-- with respect to the relative decoding \(\O_m\)-basis.
coeffsDec :: (TensorPowDec t r, m `Divides` m') => CycRep t D m' r -> [CycRep t D m r]
{-# INLINABLE coeffsDec #-}
coeffsDec (Dec v) = Dec <$> coeffs v

-- | The relative powerful basis of \(\O_{m'} / \O_m\).
powBasis :: forall m m' t r .
  (TensorPowDec t r, m `Divides` m') => [CycRep t P m' r]
{-# INLINABLE powBasis #-}
powBasis = fmap Pow $ untag $ powBasisPow @t @r @m

-- | The relative mod-\(r\) CRT set of \(\O_{m'} / \O_m\),
-- represented with respect to the powerful basis (which seems to be
-- the best choice for typical use cases).
crtSet :: forall m m' pp p mbar m'bar t z zpp .
          (m `Divides` m', p ~ PrimePP pp, mbar ~ PFree p m, m'bar ~ PFree p m',
           PPow pp, Prime p, zpp ~ ZqBasic pp z,
           ToInteger z, CRTElt t zpp, TensorCRTSet t (ZqBasic p z))
       => [CycRep t P m' (ZqBasic pp z)]
{-# INLINABLE crtSet #-}
crtSet =
  -- CJP: consider using traceEvent or traceMarker
  --DT.trace ("CycRep.crtSet: m = " ++
  --          show (proxy valueFact (Proxy::Proxy m)) ++ ", m'= " ++
  --          show (proxy valueFact (Proxy::Proxy m'))) $
  let (p,e) = ppPPow @pp
      -- raise to the p^(e-1) power iteratively (one factor of p at a
      -- time), switching back to pow basis each time so that we don't
      -- lose precision!  (This fixes a bug witnessed for moderate
      -- values of e.)
      expon :: (Fact m'bar, ToPowDec t rep (ZqBasic pp z))
        => Int -> CycRep t rep m'bar zpp -> CycRep t P m'bar zpp
      expon 1  = toPow
      expon e' = toPowCE . (^p) . toCRT . expon (e'-1)
  in embedPow . expon e . Dec . fmapI (ZqB . unZqB) <$> -- safe!
     (untag $ crtSetDec @t @_ @mbar :: [t m'bar (ZqBasic p z)])
     \\ pFreeDivides @p @m @m' \\ pSplitTheorems @p @m \\ pSplitTheorems @p @m'


--------- Changing representation ------------------

class ToPowDec t rep r where
  -- | Convert to powerful-basis representation.
  toPow :: (Fact m) => CycRep t rep m r -> CycRep t P m r
  -- | Convert to decoding-basis representation.
  toDec :: (Fact m) => CycRep t rep m r -> CycRep t D m r

-- | separate class because some base rings don't have a CRT basis
class ToCRT t rep r where
  -- | Convert to an appropriate CRT-basis representation.
  toCRT :: (Fact m) => CycRep t rep m r -> Either (CycRep t E m r) (CycRep t C m r)

instance TensorPowDec t r => ToPowDec t P r where
  toPow = id
  toDec (Pow v) = Dec $ powToDec v
  {-# INLINABLE toPow #-}
  {-# INLINABLE toDec #-}

instance CRTElt t r => ToCRT t P r where
  toCRT (Pow v) = case crtSentinel of
                    Right s -> Right $ CRTC s $ crtCS s v
                    Left  s -> Left  $ CRTE s $ runIdentity crt $ fmapI toExt v
  {-# INLINABLE toCRT #-}

instance TensorPowDec t r => ToPowDec t D r where
  toPow (Dec v) = Pow $ decToPow v
  toDec = id
  {-# INLINABLE toPow #-}
  {-# INLINABLE toDec #-}

instance CRTElt t r => ToCRT t D r where
  toCRT = toCRT . toPow
  {-# INLINABLE toCRT #-}

instance CRTElt t r => ToPowDec t C r where
  toPow (CRTC s v) = Pow $ crtInvCS s v
  toDec = toDec . toPow
  {-# INLINABLE toPow #-}
  {-# INLINABLE toDec #-}

instance ToCRT t C r where
  toCRT = Right
  {-# INLINABLE toCRT #-}

instance CRTElt t r => ToPowDec t E r where
  toPow (CRTE _ v) = Pow $ fmapI fromExt $ runIdentity crtInv v
  toDec = toDec . toPow
  {-# INLINABLE toPow #-}
  {-# INLINABLE toDec #-}

instance ToCRT t E r where
  toCRT = Left
  {-# INLINABLE toCRT #-}

{- CJP: non-class-based toPow etc., which can be defined for CycRep as a
   GADT or data family. But it doesn't quite work for us because the
   'CRTElt t r' constraint is too strong for 'r ~ RRq q Double', which
   can't have a CRTrans instance.

{-# INLINABLE toPow #-}
toPow :: (Fact m, CRTElt t r) => CycRep t rep m r -> CycRep t P m r
toPow = \case
  c@(Pow _)  -> c
  (Dec  v)   -> Pow $ decToPow v
  (CRTC s v) -> Pow $ crtInvCS s v
  (CRTE _ v) -> Pow $ fmapI fromExt $ runIdentity crtInv v

{-# INLINABLE toDec #-}
toDec :: (Fact m, CRTElt t r) => CycRep t rep m r -> CycRep t D m r
toDec = \case
  c@(Dec _)  -> c
  (Pow  v)   -> Dec $ powToDec v
  (CRTC s v) -> Dec $ powToDec $ crtInvCS s v
  (CRTE _ v) -> Dec $ powToDec $ fmapI fromExt $ runIdentity crtInv v

{-# INLINABLE toCRT #-}
toCRT :: (Fact m, CRTElt t r) => CycRep t rep m r -> CycRepEC t m r
toCRT = \case
  c@(CRTC _ _) -> Right c
  c@(CRTE _ _) -> Left c
  (Pow v)      -> go v
  (Dec v)      -> go $ decToPow v
  where go v =
          case crtSentinel of
            Right s -> Right $ CRTC s $ crtCS s v
            Left  s -> Left  $ CRTE s $ runIdentity crt $ fmapI toExt v
-}

-- | Convenient version of 'toPow' for 'Either' CRT basis type.
toPowCE :: (Fact m, CRTElt t r) => CycRepEC t m r -> CycRep t P m r
{-# INLINABLE toPowCE #-}
toPowCE (Left u)  = toPow u
toPowCE (Right u) = toPow u


---------- Category-theoretic instances ----------

----- No instances for E because types (and math) don't make sense.

-- | apply coefficient-wise
instance IFunctor t => IFunctor (CycRep t P) where
  type IFElt (CycRep t P) a = IFElt t a
  fmapI    f (Pow v)         = Pow $ fmapI f v
  zipWithI f (Pow v) (Pow w) = Pow $ zipWithI f v w

-- | apply coefficient-wise
instance IFunctor t => IFunctor (CycRep t D) where
  type IFElt (CycRep t D) a = IFElt t a
  fmapI    f (Dec v)         = Dec $ fmapI f v
  zipWithI f (Dec v) (Dec w) = Dec $ zipWithI f v w

-- | apply coefficient-wise
instance Functor (t m) => Functor (CycRep t P m) where
  {-# INLINABLE fmap #-}
  fmap f (Pow x) = Pow $ f <$> x

-- | apply coefficient-wise
instance Functor (t m) => Functor (CycRep t D m) where
  {-# INLINABLE fmap #-}
  fmap f (Dec x) = Dec $ f <$> x

-- No Functor instance for C, because CRTrans a doesn't imply CRTrans b.

instance Applicative (t m) => Applicative (CycRep t P m) where
  pure = Pow . pure
  (Pow f) <*> (Pow v) = Pow $ f <*> v
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance Applicative (t m) => Applicative (CycRep t D m) where
  pure = Dec . pure
  (Dec f) <*> (Dec v) = Dec $ f <*> v
  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

-- CJP: no Applicative instance for C, because 'pure' would circumvent
-- the invariant.  Moreover, `CRTrans (a -> b)` and `CRTrans a`
-- doesn't imply `CRTrans b`.

instance Foldable (t m) => Foldable (CycRep t P m) where
  foldr f z (Pow x) = foldr f z x

instance Foldable (t m) => Foldable (CycRep t D m) where
  foldr f z (Dec x) = foldr f z x

-- no Traversable for C, but it is Foldable
instance Foldable (t m) => Foldable (CycRep t C m) where
  foldr f b (CRTC _ v) = foldr f b v

instance Traversable (t m) => Traversable (CycRep t P m) where
  {-# INLINABLE traverse #-}
  traverse f (Pow v) = Pow <$> traverse f v

instance Traversable (t m) => Traversable (CycRep t D m) where
  {-# INLINABLE traverse #-}
  traverse f (Dec v) = Dec <$> traverse f v

-- CJP: no Traversable instance for C, because no Functor instance
-- (see above)


---------- Utility instances ----------

instance (Random (t m r)) => Random (CycRep t P m r) where
  random g = let (v,g') = random g
             in (Pow v, g')
  randomR _ = error "randomR non-sensical for CycRep"

instance (Random (t m r)) => Random (CycRep t D m r) where
  random g = let (v,g') = random g
             in (Dec v, g')
  randomR _ = error "randomR non-sensical for CycRep"

instance (Random (t m r), Fact m, TensorCRT t Maybe r)
  => Random (CycRepPC t m r) where
  -- create in CRTC basis if possible, otherwise in powerful
  random = let cons = case crtSentinel of
                 Left  _ -> Left  . Pow
                 Right s -> Right . CRTC s
           in \g -> let (v,g') = random g
                    in (cons v, g')
  randomR _ = error "randomR non-sensical for CycRep"

instance (Show (t m r)) => Show (CycRep t P m r) where
  show (Pow x) = "CycRep.Pow " ++ show x

instance (Show (t m r)) => Show (CycRep t D m r) where
  show (Dec x) = "CycRep.Dec " ++ show x

instance (Show (t m r)) => Show (CycRep t C m r) where
  show (CRTC _ x) = "CycRep.CRTC " ++ show x

instance (Show (t m (CRTExt r))) => Show (CycRep t E m r) where
  show (CRTE _ x) = "CycRep.CRTE " ++ show x


instance (NFData (t m r)) => NFData (CycRep t P m r) where
  rnf (Pow x) = rnf x

instance (NFData (t m r)) => NFData (CycRep t D m r) where
  rnf (Dec x) = rnf x

instance (NFData (t m r)) => NFData (CycRep t C m r) where
  rnf (CRTC _ x) = rnf x

instance (NFData (t m (CRTExt r))) => NFData (CycRep t E m r) where
  rnf (CRTE _ x) = rnf x


instance (Protoable (t m r)) => Protoable (CycRep t D m r) where
  type ProtoType (CycRep t D m r) = ProtoType (t m r)
  toProto (Dec t) = toProto t
  fromProto t = Dec <$> fromProto t
