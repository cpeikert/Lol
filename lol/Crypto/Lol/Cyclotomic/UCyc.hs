{-|
Module      : Crypto.Lol.Cyclotomic.UCyc
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
associated with 'UCyc' may result in overflow (and thereby
incorrect answers and potential security flaws) if the input
arguments are too close to the bounds imposed by the base type.
The acceptable range of inputs for each function is determined by
the internal linear transforms and other operations it performs.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Cyclotomic.UCyc
(
-- * Data types and constraints
  UCyc, P, D, C, E, UCycEC, UCycPC, UCRTElt
-- * Changing representation
, toPow, toDec, toCRT, fmapPow, fmapDec
-- * Scalars
, scalarPow, scalarCRT
-- * Basic operations
, mulG, divGPow, divGDec, divGCRTC, gSqNorm
-- * Error sampling
, tGaussian, errorRounded, errorCoset
-- * Inter-ring operations and values
, embedPow, embedDec, embedCRTC, embedCRTE
, twacePow, twaceDec, twaceCRTC, twaceCRTE
, coeffsPow, coeffsDec, powBasis, crtSet
) where

import Crypto.Lol.Cyclotomic.Tensor hiding (divGDec, divGPow, embedCRT,
                                     embedDec, embedPow, scalarCRT,
                                     scalarPow, twaceCRT)

import           Crypto.Lol.CRTrans
import           Crypto.Lol.Cyclotomic.CRTSentinel
import qualified Crypto.Lol.Cyclotomic.Tensor      as T
import           Crypto.Lol.Prelude                as LP
import           Crypto.Lol.Types.FiniteField
import           Crypto.Lol.Types.IFunctor
import           Crypto.Lol.Types.ZPP

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

import Crypto.Lol.Types.Proto

--import qualified Debug.Trace as DT

-- | Nullary index type representing the powerful basis.
data P
-- | Nullary index type representing the decoding basis.
data D
-- | Nullary index type representing the CRT basis over base ring.
data C
-- | Nullary index type representing the CRT basis over extension of
-- base ring.
data E

-- | Convenient synonym for either CRT representation.
type UCycEC t m r = Either (UCyc t m E r) (UCyc t m C r)

-- | Convenient synonym for random sampling.
type UCycPC t m r = Either (UCyc t m P r) (UCyc t m C r)

-- | Represents a cyclotomic ring such as \(\Z[\zeta_m]\),
-- \(\Z_q[\zeta_m]\), and \(\Q(\zeta_m)\) in an explicit
-- representation: @t@ is the 'Tensor' type for storing coefficient
-- tensors; @m@ is the cyclotomic index; @rep@ is the representation
-- (e.g., 'P', 'D', 'C', 'E'); @r@ is the base ring of the
-- coefficients (e.g., \(\Z\), \(\Z_q\)).

data family UCyc (t :: Factored -> * -> *) (m :: Factored) rep r

newtype instance UCyc t m P r = Pow  (t m r)
newtype instance UCyc t m D r = Dec  (t m r)
-- C/ESentinel enforces invariant that exactly one of these can be
-- created for a given (t,m,r).
data    instance UCyc t m C r = CRTC !(CSentinel t m r) !(t m r)
data    instance UCyc t m E r = CRTE !(ESentinel t m r) !(t m (CRTExt r))

-- | Constraints needed for CRT-related operations on 'UCyc' data.
type UCRTElt t r = (Tensor t r, Tensor t (CRTExt r),
                    CRTEmbed r, CRTrans Maybe r, CRTrans Identity (CRTExt r))

-- | Embed a scalar from the base ring.
scalarPow :: (Tensor t r, Fact m, Ring r) => r -> UCyc t m P r
scalarPow = Pow . T.scalarPow
{-# INLINABLE scalarPow #-}

-- | Embed a scalar from the base ring.
scalarCRT :: (Fact m, UCRTElt t r) => r -> UCycEC t m r
scalarCRT r = case crtSentinel of
  Right s -> Right $ CRTC s $ scalarCRTCS s r
  Left  s -> Left  $ CRTE s $ runIdentity T.scalarCRT $ toExt r
{-# INLINABLE scalarCRT #-}

-- Eq instances

instance (Eq (t m r), Fact m) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2
  {-# INLINABLE (==) #-}

instance (Eq (t m r), Fact m) => Eq (UCyc t m D r) where
  (Dec v1) == (Dec v2) = v1 == v2
  {-# INLINABLE (==) #-}

instance (Eq (t m r), Fact m) => Eq (UCyc t m C r) where
  (CRTC _ v1) == (CRTC _ v2) = v1 == v2
  {-# INLINABLE (==) #-}

-- no Eq instance for E due to precision

---------- Numeric Prelude instances ----------

-- ZeroTestable instances

instance (ZeroTestable (t m r), Fact m) => ZeroTestable.C (UCyc t m P r) where
  isZero (Pow v) = isZero v
  {-# INLINABLE isZero #-}

instance (ZeroTestable (t m r), Fact m) => ZeroTestable.C (UCyc t m D r) where
  isZero (Dec v) = isZero v
  {-# INLINABLE isZero #-}

instance (ZeroTestable (t m r), Fact m) => ZeroTestable.C (UCyc t m C r) where
  isZero (CRTC _ v) = isZero v
  {-# INLINABLE isZero #-}

-- no ZT instance for E due to precision

-- Additive instances

instance (Additive r, Tensor t r, IFunctor t, Fact m)
         => Additive.C (UCyc t m P r) where
  zero = Pow $ T.scalarPow zero
  (Pow v1) + (Pow v2) = Pow $ zipWithI (+) v1 v2
  (Pow v1) - (Pow v2) = Pow $ zipWithI (-) v1 v2
  negate (Pow v) = Pow $ fmapI negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (Additive r, Tensor t r, IFunctor t, Fact m)
         => Additive.C (UCyc t m D r) where
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
instance (Fact m, UCRTElt t r) => Additive.C (UCycEC t m r) where

  zero = scalarCRT zero

  -- CJP: precision OK?  Alternatively, switch to pow and back after
  -- adding/subtracting.  Expensive, but necessary given output type.
  (Right (CRTC s v1)) + (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (+) v1 v2
  (Left (CRTE s v1)) + (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (+) v1 v2
  _ + _ = error "UCyc (+) internal error: mixed CRTC/CRTE"

  (Right (CRTC s v1)) - (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (-) v1 v2
  (Left (CRTE s v1)) - (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (-) v1 v2
  _ - _ = error "UCyc (-) internal error: mixed CRTC/CRTE"

  negate (Right (CRTC s v)) = Right $ CRTC s $ fmapI negate v
  negate (Left (CRTE s v)) = Left $ CRTE s $ fmapI negate v

  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- | only for appropriate CRT representation
instance (Fact m, UCRTElt t r) => Ring.C (UCycEC t m r) where

  one = scalarCRT one
  fromInteger c = scalarCRT $ fromInteger c

  (Right (CRTC s v1)) * (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithI (*) v1 v2
  (Left (CRTE s v1)) * (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithI (*) v1 v2
  _ * _ = error "UCyc internal error: mixed CRTC/CRTE"

  {-# INLINABLE one #-}
  {-# INLINABLE fromInteger #-}
  {-# INLINABLE (*) #-}


instance (Ring r, Tensor t r, Fact m) => Module.C r (UCyc t m P r) where
  r *> (Pow v) = Pow $ fmapI (r *) v
  {-# INLINABLE (*>) #-}

instance (Ring r, Tensor t r, Fact m) => Module.C r (UCyc t m D r) where
  r *> (Dec v) = Dec $ fmapI (r *) v
  {-# INLINABLE (*>) #-}

instance (Ring r, Fact m, UCRTElt t r) => Module.C r (UCycEC t m r) where

  r *> (Right (CRTC s v)) = Right $ CRTC s $ fmapI (r *) v
  r *> (Left (CRTE s v)) = Left $ CRTE s $ fmapI (toExt r *) v
  {-# INLINABLE (*>) #-}

-- | \(R_p\) is an \(\F_{p^d}\)-module when \(d\) divides \(\varphi(m)\), by
-- applying \(d\)-dimensional \(\F_p\)-linear transform on \(d\)-dim chunks of
-- powerful basis coeffs.
instance (GFCtx fp d, Fact m, Tensor t fp, Module (GF fp d) (t m fp))
         => Module.C (GF fp d) (UCyc t m P fp) where
  -- can use any r-basis to define module mult, but must be
  -- consistent.
  r *> (Pow v) = Pow $ r LP.*> v


instance (Reduce a b, Tensor t a, Tensor t b, IFunctor t, Fact m)
         => Reduce (UCyc t m P a) (UCyc t m P b) where
  reduce (Pow v) = Pow $ fmapI reduce v
  {-# INLINABLE reduce #-}

instance (Reduce a b, Tensor t a, Tensor t b, IFunctor t, Fact m)
    => Reduce (UCyc t m D a) (UCyc t m D b) where
  reduce (Dec v) = Dec $ fmapI reduce v
  {-# INLINABLE reduce #-}

-- CJP: no Reduce for C because CRT basis may not exist for target
-- type

type instance LiftOf (UCyc t m P r) = UCyc t m P (LiftOf r)
type instance LiftOf (UCyc t m D r) = UCyc t m D (LiftOf r)

instance (Lift' r, Tensor t r, Tensor t (LiftOf r), IFunctor t, Fact m)
         => Lift' (UCyc t m P r) where
  lift (Pow v) = Pow $ fmapI lift v
  {-# INLINABLE lift #-}

instance (Lift' r, Tensor t r, Tensor t (LiftOf r), Fact m)
         => Lift' (UCyc t m D r) where
  lift (Dec v) = Dec $ fmapI lift v
  {-# INLINABLE lift #-}

-- CJP: no Lift' for C because CRT basis may not exist for target type

instance (Rescale a b, Tensor t a, Tensor t b, Fact m)
         => Rescale (UCyc t m P a) (UCyc t m P b) where
  rescale (Pow v) = Pow $ fmapI rescale v
  {-# INLINABLE rescale #-}

instance (Rescale a b, Tensor t a, Tensor t b, Fact m)
         => Rescale (UCyc t m D a) (UCyc t m D b) where
  rescale (Dec v) = Dec $ fmapI rescale v
  {-# INLINABLE rescale #-}

-- CJP: no Rescale for C because CRT basis may not exist for target
-- type

-- CJP: we don't instantiate RescaleCyc because it requires changing bases

-- CJP: we don't instantiate Gadget etc., because (1) their methods
-- wouldn't be efficient, and (2) their superclass constraints are not
-- satisfied anyway (e.g., Ring for P rep).


-- | Type-restricted (and potentially more efficient) 'fmap' for
-- powerful-basis representation.
fmapPow :: (IFunctor t, IFElt t a, IFElt t b, Fact m) => (a -> b) -> UCyc t m P a -> UCyc t m P b
fmapPow f (Pow v) = Pow $ fmapI f v
{-# INLINABLE fmapPow #-}

-- | Type-restricted (and potentially more efficient) 'fmap' for
-- decoding-basis representation.
fmapDec :: (IFunctor t, IFElt t a, IFElt t b, Fact m) => (a -> b) -> UCyc t m D a -> UCyc t m D b
fmapDec f (Dec v) = Dec $ fmapI f v
{-# INLINABLE fmapDec #-}

class MulG rep where
  -- | Multiply by the special element \(g_m\).
  mulG :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m rep r

instance MulG P where mulG (Pow v) = Pow $ mulGPow v
instance MulG D where mulG (Dec v) = Dec $ mulGDec v
instance MulG C where mulG (CRTC s v) = CRTC s $ mulGCRTCS s v
instance MulG E where mulG (CRTE s v) = CRTE s $ runIdentity mulGCRT v

-- Note: We do not implement divGCRTE because we can't tell whether
-- the element is actually divisible by g when using the CRT extension
-- basis.

-- | Divide by the special element \(g_m\).
-- WARNING: this implementation is not a constant-time algorithm, so
-- information about the argument may be leaked through a timing
-- channel.
divGPow :: (Fact m, UCRTElt t r, ZeroTestable r, IntegralDomain r)
        => UCyc t m P r -> Maybe (UCyc t m P r)
{-# INLINABLE divGPow #-}
divGPow (Pow v) = Pow <$> T.divGPow v

-- | Similar to 'divGPow'.
divGDec :: (Fact m, UCRTElt t r, ZeroTestable r, IntegralDomain r)
        => UCyc t m D r -> Maybe (UCyc t m D r)
{-# INLINABLE divGDec #-}
divGDec (Dec v) = Dec <$> T.divGDec v

-- | Similar to 'divGPow'.
divGCRTC :: (Fact m, UCRTElt t r)
        => UCyc t m C r -> UCyc t m C r
{-# INLINE divGCRTC #-}
divGCRTC (CRTC s v) = CRTC s $ divGCRTCS s v

-- | Yield the scaled squared norm of \(g_m \cdot e\) under
-- the canonical embedding, namely,
-- \(\hat{m}^{-1} \cdot \| \sigma(g_m \cdot e) \|^2\) .
gSqNorm :: (Ring r, Tensor t r, Fact m) => UCyc t m D r -> r
gSqNorm (Dec v) = gSqNormDec v
{-# INLINABLE gSqNorm #-}

-- | Sample from the "tweaked" Gaussian error distribution \(t\cdot D\) in
-- the decoding basis, where \(D\) has scaled variance \(v\).
tGaussian :: (TensorGaussian t q, MonadRandom rnd, Fact m, ToRational v)
             => v -> rnd (UCyc t m D q)
tGaussian = fmap Dec . tGaussianDec
{-# INLINABLE tGaussian #-}

-- | Generate an LWE error term from the "tweaked" Gaussian with given
-- scaled variance, deterministically rounded using the decoding
-- basis. (Note: This
-- implementation uses 'Double' precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.)
errorRounded :: forall v rnd t m z .
                (ToInteger z, IFunctor t, IFElt t Double, IFElt t z,
                 TensorGaussian t Double,
                 Fact m, ToRational v, MonadRandom rnd)
                => v -> rnd (UCyc t m D z)
{-# INLINABLE errorRounded #-}
errorRounded svar =
  Dec . fmapI (roundMult one) <$> (tGaussianDec svar :: rnd (t m Double))

-- | Generate an LWE error term from the "tweaked" Gaussian with
-- scaled variance \(v \cdot p^2\), deterministically rounded to the given
-- coset of \(R_p\) using the decoding basis. (Note: This
-- implementation uses 'Double' precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.)
errorCoset :: forall t m zp z v rnd .
              (TensorGaussian t Double, Mod zp, z ~ ModRep zp, Lift zp z, Fact m,
               ToRational v, MonadRandom rnd, Applicative (t m))
              => v -> UCyc t m D zp -> rnd (UCyc t m D z)
{-# INLINABLE errorCoset #-}
errorCoset =
  let pval = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  in \ svar c -> do err :: UCyc t m D Double <- tGaussian (svar*pval*pval)
                    return $! roundCoset <$> c <*> err


----- inter-ring operations

-- | Embed into an extension ring, for the powerful basis.
embedPow :: (Additive r, Tensor t r, m `Divides` m') => UCyc t m P r -> UCyc t m' P r
embedPow (Pow v) = Pow $ T.embedPow v
{-# INLINABLE embedPow #-}

-- | Embed into an extension ring, for the decoding basis.
embedDec :: (Additive r, Tensor t r, m `Divides` m') => UCyc t m D r -> UCyc t m' D r
embedDec (Dec v) = Dec $ T.embedDec v
{-# INLINABLE embedDec #-}

-- | Embed into an extension ring, for the CRT basis.  (The output is
-- an 'Either' because the extension ring might not support 'C'.)
embedCRTC :: (m `Divides` m', UCRTElt t r)
             => UCyc t m C r -> Either (UCyc t m' P r) (UCyc t m' C r)
{-# INLINABLE embedCRTC #-}
embedCRTC x@(CRTC s v) =
  case crtSentinel of
    -- go to CRTC if valid, else go to Pow
    Left  _ -> Left $ embedPow $ toPow x
    Right s' -> Right $ CRTC s' $ embedCRTCS s s' v

-- | Similar to 'embedCRTC'.  (The output is an 'Either' because the
-- extension ring might support 'C', in which case we never use 'E'.)
embedCRTE :: forall m m' t r . (m `Divides` m', UCRTElt t r)
             => UCyc t m E r -> Either (UCyc t m' P r) (UCyc t m' E r)
{-# INLINABLE embedCRTE #-}
embedCRTE x@(CRTE _ v) =
  case crtSentinel of
    -- go to CRTE if valid, else go to Pow
    Left  s -> Right $ CRTE s $ runIdentity T.embedCRT v
    Right _ -> Left $ embedPow $ toPow x

-- | Twace into a subring, for the powerful basis.
twacePow :: (Ring r, Tensor t r, m `Divides` m') => UCyc t m' P r -> UCyc t m P r
twacePow (Pow v) = Pow $ twacePowDec v
{-# INLINABLE twacePow #-}

-- | Twace into a subring, for the decoding basis.
twaceDec :: (Ring r, Tensor t r, m `Divides` m') => UCyc t m' D r -> UCyc t m D r
twaceDec (Dec v) = Dec $ twacePowDec v
{-# INLINABLE twaceDec #-}

-- | Twace into a subring, for the CRT basis.  (The output is an
-- 'Either' because the subring might not support 'C'.)
twaceCRTC :: (m `Divides` m', UCRTElt t r)
             => UCyc t m' C r -> UCycPC t m r
{-# INLINE twaceCRTC #-}
twaceCRTC x@(CRTC s' v) =
  case crtSentinel of
    -- go to CRTC if valid for target, else go to Pow
    Left  _ -> Left $ twacePow $ toPow x
    Right s -> Right $ CRTC s $ twaceCRTCS s' s v

-- | Similar to 'twaceCRTC'.  (The output is an 'Either' because the
-- subring might support 'C', in which case we never use 'E'.)
twaceCRTE :: forall t m m' r . (m `Divides` m', UCRTElt t r)
             => UCyc t m' E r -> Either (UCyc t m P r) (UCyc t m E r)
{-# INLINABLE twaceCRTE #-}
twaceCRTE x@(CRTE _ v) =
  case crtSentinel of
    -- go to CRTE if valid for target, else go to Pow
    Left  s -> Right $ CRTE s $ runIdentity T.twaceCRT v
    Right _ -> Left $ twacePow $ toPow x

-- | Yield the \(\O_m\)-coefficients of an \(\O_{m'}\)-element,
-- with respect to the relative powerful \(\O_m\)-basis.
coeffsPow :: (Ring r, Tensor t r, m `Divides` m') => UCyc t m' P r -> [UCyc t m P r]
{-# INLINABLE coeffsPow #-}
coeffsPow (Pow v) = LP.map Pow $ coeffs v

-- | Yield the \(\O_m\)-coefficients of an \(\O_{m'}\) element,
-- with respect to the relative decoding \(\O_m\)-basis.
coeffsDec :: (Ring r, Tensor t r, m `Divides` m') => UCyc t m' D r -> [UCyc t m D r]
{-# INLINABLE coeffsDec #-}
coeffsDec (Dec v) = LP.map Dec $ coeffs v

-- | The relative powerful basis of \(\O_{m'} / \O_m\).
powBasis :: (Ring r, Tensor t r, m `Divides` m') => Tagged m [UCyc t m' P r]
{-# INLINABLE powBasis #-}
powBasis = (Pow <$>) <$> powBasisPow

-- | The relative mod-\(r\) CRT set of \(\O_{m'} / \O_m\),
-- represented with respect to the powerful basis (which seems to be
-- the best choice for typical use cases).
crtSet :: forall t m m' r p mbar m'bar .
           (m `Divides` m', ZPP r, p ~ CharOf (ZpOf r), mbar ~ PFree p m, m'bar ~ PFree p m',
            UCRTElt t r, TensorCRTSet t (ZpOf r))
          => Tagged m [UCyc t m' P r]
{-# INLINABLE crtSet #-}
crtSet =
  -- CJP: consider using traceEvent or traceMarker
  --DT.trace ("UCyc.crtSet: m = " ++
  --          show (proxy valueFact (Proxy::Proxy m)) ++ ", m'= " ++
  --          show (proxy valueFact (Proxy::Proxy m'))) $
  let (p,e) = proxy modulusZPP (Proxy::Proxy r)
      -- raise to the p^(e-1) power iteratively (one factor of p at a
      -- time), switching back to pow basis each time so that we don't
      -- lose precision!  (This fixes a bug witnessed for moderate
      -- values of e.)
      expon 1  = toPow
      expon e' = toPowCE . (^p) . toCRT . expon (e'-1)
      pp  = Proxy::Proxy p
      pm  = Proxy::Proxy m
      pm' = Proxy::Proxy m'
  in retag (fmap (embedPow .
                  expon e .
                  Dec . fmapI liftZp) <$>
            (crtSetDec :: Tagged mbar [t m'bar (ZpOf r)]))
     \\ pFreeDivides pp pm pm' \\ pSplitTheorems pp pm \\ pSplitTheorems pp pm'


--------- Changing representation ------------------

class ConvertRep rep where
  -- | Convert to powerful-basis representation.
  toPow :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m P r
  -- | Convert to decoding-basis representation.
  toDec :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m D r
  -- | Convert to an appropriate CRT-basis representation.
  toCRT :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCycEC t m r

instance ConvertRep P where
  toPow = id
  toDec (Pow v) = Dec $ lInv v
  toCRT (Pow v) = case crtSentinel of
                    Right s -> Right $ CRTC s $ crtCS s v
                    Left  s -> Left  $ CRTE s $ runIdentity crt $ fmapI toExt v

instance ConvertRep D where
  toPow (Dec v) = Pow $ l v
  toDec = id
  toCRT = toCRT . toPow

instance ConvertRep C where
  toPow (CRTC s v) = Pow $ crtInvCS s v
  toDec = toDec . toPow
  toCRT = Right

instance ConvertRep E where
  toPow (CRTE _ v) = Pow $ fmapI fromExt $ runIdentity crtInv v
  toDec = toDec . toPow
  toCRT = Left

-- | Convenient version of 'toPow' for 'Either' CRT basis type.
toPowCE :: (Fact m, UCRTElt t r) => UCycEC t m r -> UCyc t m P r
{-# INLINABLE toPowCE #-}
toPowCE (Left u) = toPow u
toPowCE (Right u) = toPow u


---------- Category-theoretic instances ----------

-- CJP: no instances for E because types (and math) don't make sense.

-- | apply coefficient-wise (with respect to powerful basis)
instance (Applicative (t m), IFunctor t, Fact m) => Functor (UCyc t m P) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

-- | apply coefficient-wise (with respect to decoding basis)
instance (Applicative (t m), IFunctor t, Fact m) => Functor (UCyc t m D) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

-- CJP: no Functor instance for C, because CRTrans for a doesn't imply
-- it for b.

instance (IFunctor t, Applicative (t m), Fact m) => Applicative (UCyc t m P) where
  pure = Pow . pure
  (Pow f) <*> (Pow v) = Pow $ f <*> v

  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Applicative (t m), IFunctor t, Fact m) => Applicative (UCyc t m D) where
  pure = Dec . pure
  (Dec f) <*> (Dec v) = Dec $ f <*> v

  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

-- CJP: no Applicative instance for C, because 'pure' would circumvent
-- the invariant.  Moreover, having CRTrans for (a -> b) and for a
-- doesn't imply it for b.

instance (Foldable (t m), IFunctor t, Fact m) => Foldable (UCyc t m P) where
  {-# INLINABLE foldr #-}
  foldr f b (Pow v) = F.foldr f b v

instance (Foldable (t m), IFunctor t, Fact m) => Foldable (UCyc t m D) where
  {-# INLINABLE foldr #-}
  foldr f b (Dec v) = F.foldr f b v

instance (Foldable (t m), IFunctor t, Fact m) => Foldable (UCyc t m C) where
  {-# INLINABLE foldr #-}
  foldr f b (CRTC _ v) = F.foldr f b v


instance (Traversable (t m), Applicative (t m), IFunctor t, Fact m)
         => Traversable (UCyc t m P) where
  {-# INLINABLE traverse #-}
  traverse f (Pow v) = Pow <$> traverse f v

instance (Traversable (t m), Applicative (t m), IFunctor t, Fact m)
         => Traversable (UCyc t m D) where
  {-# INLINABLE traverse #-}
  traverse f (Dec v) = Dec <$> traverse f v

-- CJP: no Traversable instance for C, because CRTrans for a doesn't
-- imply it for b.


---------- Utility instances ----------

instance (Random (t m r), UCRTElt t r, Fact m) => Random (UCyc t m P r) where

  random g = let (v,g') = random g
             in (Pow v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Random (t m r), UCRTElt t r, Fact m) => Random (UCyc t m D r) where

  random g = let (v,g') = random g
             in (Dec v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Random (t m r), UCRTElt t r, Fact m)
         => Random (UCycPC t m r) where

  -- create in CRTC basis if possible, otherwise in powerful
  random = let cons = case crtSentinel of
                 Left  _ -> Left  . Pow
                 Right s -> Right . CRTC s
           in \g -> let (v,g') = random g
                    in (cons v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Show (t m r), Fact m, Tensor t r) => Show (UCyc t m P r) where
  show (Pow x) = "UCyc.Pow " ++ show x

instance (Show (t m r), Fact m, Tensor t r) => Show (UCyc t m D r) where
  show (Dec x) = "UCyc.Dec " ++ show x

instance (Show (t m r), Fact m, Tensor t r) => Show (UCyc t m C r) where
  show (CRTC _ x) = "UCyc.CRTC " ++ show x

instance (Show (t m (CRTExt r)), Fact m, Tensor t (CRTExt r)) => Show (UCyc t m E r) where
  show (CRTE _ x) = "UCyc.CRTE " ++ show x

instance (NFData (t m r)) => NFData (UCyc t m P r) where
  rnf (Pow x)    = rnf x

instance (NFData (t m r)) => NFData (UCyc t m D r) where
  rnf (Dec x)    = rnf x

instance (NFData (t m r)) => NFData (UCyc t m C r) where
  rnf (CRTC _ x) = rnf x

instance (NFData (t m (CRTExt r))) => NFData (UCyc t m E r) where
  rnf (CRTE _ x) = rnf x

instance (Protoable (t m r)) => Protoable (UCyc t m D r) where
  type ProtoType (UCyc t m D r) = ProtoType (t m r)
  toProto (Dec t) = toProto t
  fromProto t = Dec <$> fromProto t
