{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, InstanceSigs, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | A low-level implementation of cyclotomic rings that allows (and
-- requires) the programmer to control the underlying representation
-- of ring elements, i.e., powerful, decoding, or CRT basis.
--
-- __WARNING:__ as with all fixed-point arithmetic, the functions
-- associated with 'UCyc' may result in overflow (and thereby
-- incorrect answers and potential security flaws) if the input
-- arguments are too close to the bounds imposed by the base type.
-- The acceptable range of inputs for each function is determined by
-- the internal linear transforms and other operations it performs.

module Crypto.Lol.Cyclotomic.UCyc
(
-- * Data types and constraints
  UCyc, P, D, C, UCElt, NFElt
-- * Changing representation
, toPow, toDec, toCRT, fmapPow, fmapDec, unzipCyc, unzipUCElt
-- * Scalars
, scalarPow, scalarCRT          -- scalarDec suppressed
-- * Basic operations
, mulG, divG, gSqNorm
-- * Error sampling
, tGaussian, errorRounded, errorCoset
-- * Inter-ring operations and values
, embedPow, embedDec, embedCRT
, twacePow, twaceDec, twaceCRT
, coeffsPow, coeffsDec, powBasis, crtSet
) where

import Crypto.Lol.Cyclotomic.Tensor hiding (embedCRT, embedDec, embedPow,
                                     scalarCRT, scalarPow, -- scalarDec
                                     twaceCRT)

import           Crypto.Lol.CRTrans
import qualified Crypto.Lol.Cyclotomic.Tensor as T
import           Crypto.Lol.LatticePrelude    as LP
import           Crypto.Lol.Types.FiniteField
import           Crypto.Lol.Types.ZPP

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Module       as Module (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative    as A
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random
import Data.Foldable          as F
import Data.Maybe
import Data.Traversable
import Test.QuickCheck

--import qualified Debug.Trace as DT

-- | Nullary index type representing the powerful basis.
data P
-- | Nullary index type representing the decoding basis.
data D
-- | Nullary index type representing a CRT basis.
data C

-- | Represents a cyclotomic ring such as @Z[zeta]@,
-- @Zq[zeta]@, and @Q(zeta)@ in an explicit representation: @t@ is the
-- 'Tensor' type for storing coefficient tensors; @m@ is the
-- cyclotomic index; @rep@ is the representation ('P', 'D', or 'C');
-- @r@ is the base ring of the coefficients (e.g., @Z@, @Zq@).
--
-- The 'Functor', 'Applicative', 'Foldable' and 'Traversable'
-- instances all work coefficient-wise (in the specified basis).
data UCyc t m rep r where
  Pow  :: !(t m r) -> UCyc t m P r
  Dec  :: !(t m r) -> UCyc t m D r
  -- Invariant: for a given (t,m,r), exactly one of these two is ever
  -- used: CRTr if crtFuncs exists, otherwise CRTe
  CRTr :: !(t m r) -> UCyc t m C r
  CRTe :: !(t m (CRTExt r)) -> UCyc t m C r

-- | Constraints needed for many operations involving the 'UCyc' CRT ('C')
-- representation.
type UCElt t r = (Tensor t, CRTEmbed r, CRTElt t r, CRTElt t (CRTExt r))

-- | Convenient synonym for 'deepseq'-able element type.
type NFElt r = (NFData r, NFData (CRTExt r))

-- | Embed a scalar from the base ring.
scalarPow :: (Tensor t, Fact m, Ring r, TElt t r) => r -> UCyc t m P r
scalarPow = Pow . T.scalarPow
{-# INLINABLE scalarPow #-}

{- CJP: suppressed

-- | Embed a scalar from the base ring.
scalarDec :: (Tensor t, Fact m, Ring r, TElt t r) => r -> UCyc t m D r
scalarDec = Dec . T.scalarDec
{-# INLINABLE scalarDec #-}

-}

-- | Embed a scalar from the base ring.
scalarCRT :: (Fact m, UCElt t r) => r -> UCyc t m C r
scalarCRT = fromMaybe
               (CRTe . fromJust' "UCyc: no CRT over CRTExt" T.scalarCRT . toExt)
               ((CRTr .) <$> T.scalarCRT)
{-# INLINABLE scalarCRT #-}

-- Eq instances

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m D r) where
  (Dec v1) == (Dec v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Fact m, UCElt t r) => Eq (UCyc t m C r) where
  (CRTr v1) == (CRTr v2) = v1 == v2 \\ witness entailEqT v1
  -- compare in pow due to precision
  u1 == u2 = toPow u1 == toPow u2
  {-# INLINABLE (==) #-}


---------- Numeric Prelude instances ----------

-- ZeroTestable instances

instance (ZeroTestable r, Tensor t, Fact m, TElt t r)
    => ZeroTestable.C (UCyc t m P r) where
  isZero (Pow v) = isZero v \\ witness entailZTT v
  {-# INLINABLE isZero #-}

instance (ZeroTestable r, Tensor t, Fact m, TElt t r)
    => ZeroTestable.C (UCyc t m D r) where
  isZero (Dec v) = isZero v \\ witness entailZTT v
  {-# INLINABLE isZero #-}

instance (ZeroTestable r, Fact m, UCElt t r)
    => ZeroTestable.C (UCyc t m C r) where
  isZero (CRTr v) = isZero v \\ witness entailZTT v
  -- use powerful basis due to precision
  isZero u = isZero $ toPow u
  {-# INLINABLE isZero #-}

-- Additive instances

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m P r) where
  zero = Pow $ T.scalarPow zero
  (Pow v1) + (Pow v2) = Pow $ zipWithT (+) v1 v2
  (Pow v1) - (Pow v2) = Pow $ zipWithT (-) v1 v2
  negate (Pow v) = Pow $ fmapT negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m D r) where
  zero = Dec $ T.scalarPow zero -- scalarPow works because it's zero
  (Dec v1) + (Dec v2) = Dec $ zipWithT (+) v1 v2
  (Dec v1) - (Dec v2) = Dec $ zipWithT (-) v1 v2
  negate (Dec v) = Dec $ fmapT negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (Fact m, UCElt t r) => Additive.C (UCyc t m C r) where
  zero = scalarCRT zero

  -- CJP: precision OK?  Alternatively, switch to pow and back after
  -- adding/subtracting.  Expensive, but necessary given output type.
  (CRTr v1) + (CRTr v2) = CRTr $ zipWithT (+) v1 v2
  (CRTe v1) + (CRTe v2) = CRTe $ zipWithT (+) v1 v2
  _ + _ = error "UCyc (+) internal error: mixed CRTr/CRTe"

  (CRTr v1) - (CRTr v2) = CRTr $ zipWithT (-) v1 v2
  (CRTe v1) - (CRTe v2) = CRTe $ zipWithT (-) v1 v2
  _ - _ = error "UCyc (-) internal error: mixed CRTr/CRTe"

  negate (CRTr v) = CRTr $ fmapT negate v
  negate (CRTe v) = CRTe $ fmapT negate v

  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- Ring instance: only for CRT

instance (Fact m, UCElt t r) => Ring.C (UCyc t m C r) where
  one = scalarCRT one
  fromInteger c = scalarCRT $ fromInteger c

  -- CJP: precision OK?  Alternatively, switch to pow (and back) after
  -- multiplying.  Expensive, but necessary given output type.
  (CRTr v1) * (CRTr v2) = CRTr $ zipWithT (*) v1 v2
  (CRTe v1) * (CRTe v2) = CRTe $ zipWithT (*) v1 v2
  _ * _ = error "UCyc internal error: mixed CRTr/CRTe"

  {-# INLINABLE one #-}
  {-# INLINABLE fromInteger #-}
  {-# INLINABLE (*) #-}


instance (Ring r, Tensor t, Fact m, TElt t r) => Module.C r (UCyc t m P r) where
  r *> (Pow v) = Pow $ fmapT (r *) v
  {-# INLINABLE (*>) #-}

instance (Ring r, Tensor t, Fact m, TElt t r) => Module.C r (UCyc t m D r) where
  r *> (Dec v) = Dec $ fmapT (r *) v
  {-# INLINABLE (*>) #-}

instance (Ring r, Fact m, UCElt t r) => Module.C r (UCyc t m C r) where
  r *> (CRTr v) = CRTr $ fmapT (r *) v
  r *> (CRTe v) = CRTe $ fmapT (toExt r *) v
  {-# INLINABLE (*>) #-}

instance (GFCtx fp d, Fact m, UCElt t fp) => Module.C (GF fp d) (UCyc t m P fp) where
  -- can use any r-basis to define module mult, but must be
  -- consistent.
  r *> (Pow v) = Pow $ r LP.*> v \\ witness entailModuleT (r,v)


instance (Reduce a b, Tensor t, Fact m, TElt t a, TElt t b)
    => Reduce (UCyc t m P a) (UCyc t m P b) where
  reduce (Pow v) = Pow $ fmapT reduce v
  {-# INLINABLE reduce #-}

instance (Reduce a b, Tensor t, Fact m, TElt t a, TElt t b)
    => Reduce (UCyc t m D a) (UCyc t m D b) where
  reduce (Dec v) = Dec $ fmapT reduce v
  {-# INLINABLE reduce #-}

-- CJP: no Reduce for C rep because we can't efficiently handle CRTe case

type instance LiftOf (UCyc t m P r) = UCyc t m P (LiftOf r)
type instance LiftOf (UCyc t m D r) = UCyc t m D (LiftOf r)

instance (Lift' r, Tensor t, Fact m, TElt t r, TElt t (LiftOf r))
         => Lift' (UCyc t m P r) where
  lift (Pow v) = Pow $ fmapT lift v
  {-# INLINABLE lift #-}

instance (Lift' r, Tensor t, Fact m, TElt t r, TElt t (LiftOf r))
         => Lift' (UCyc t m D r) where
  lift (Dec v) = Dec $ fmapT lift v
  {-# INLINABLE lift #-}


instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m P a) (UCyc t m P b) where
  rescale (Pow v) = Pow $ fmapT rescale v
  {-# INLINABLE rescale #-}

instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m D a) (UCyc t m D b) where
  rescale (Dec v) = Dec $ fmapT rescale v
  {-# INLINABLE rescale #-}


-- CJP: we don't instantiate RescaleCyc because it requires changing bases

-- CJP: we don't instantiate Gadget etc., because (1) their methods
-- wouldn't be efficient, and (2) their superclasses are not even
-- well-defined (e.g., Ring for P rep).


-- | Type-restricted (and potentially more efficient) map for
-- powerful-basis representation.
fmapPow :: (Tensor t, Fact m, TElt t a, TElt t b)
           => (a -> b) -> UCyc t m P a -> UCyc t m P b
fmapPow f (Pow v) = Pow $ fmapT f v
{-# INLINABLE fmapPow #-}

-- | Type-restricted (and potentially more efficient) map for
-- decoding-basis representation.
fmapDec :: (Tensor t, Fact m, TElt t a, TElt t b)
           => (a -> b) -> UCyc t m D a -> UCyc t m D b
fmapDec f (Dec v) = Dec $ fmapT f v
{-# INLINABLE fmapDec #-}

-- | Unzip for unrestricted types.
unzipCyc :: (Tensor t, Fact m)
            => UCyc t m rep (a,b) -> (UCyc t m rep a, UCyc t m rep b)
{-# INLINABLE unzipCyc #-}
unzipCyc (Pow v) = Pow *** Pow $ unzipT v
unzipCyc (Dec v) = Dec *** Dec $ unzipT v
unzipCyc (CRTr v) = CRTr *** CRTr $ unzipT v
unzipCyc (CRTe v) = CRTe *** CRTe $ unzipT v

-- | Type-restricted (and potentially more efficient) unzip.
unzipUCElt :: (Tensor t, Fact m, UCElt t (a,b), UCElt t a, UCElt t b)
              => UCyc t m rep (a,b) -> (UCyc t m rep a, UCyc t m rep b)
{-# INLINABLE unzipUCElt #-}
unzipUCElt (Pow v) = Pow *** Pow $ unzipTElt v
unzipUCElt (Dec v) = Dec *** Dec $ unzipTElt v
unzipUCElt (CRTr v) = CRTr *** CRTr $ unzipTElt v
unzipUCElt (CRTe v) = CRTe *** CRTe $ unzipTElt v

-- | Multiply by the special element @g@.
mulG :: (Tensor t, Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m rep r
{-# INLINABLE mulG #-}
mulG (Pow v) = Pow $ mulGPow v
mulG (Dec v) = Dec $ mulGDec v
-- fromJust is safe here because we're already in CRTr
mulG (CRTr v) = CRTr $ fromJust' "UCyc.mulG CRTr" mulGCRT v
mulG (CRTe v) = CRTe $ fromJust' "UCyc.mulG CRTe" mulGCRT v

-- | Divide by the special element @g@.
divG :: (Tensor t, Fact m, UCElt t r) => UCyc t m rep r -> Maybe (UCyc t m rep r)
{-# INLINABLE divG #-}
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
-- fromJust is OK here because we're already in CRTr
divG (CRTr v) = Just $ CRTr $ fromJust' "UCyc.divG CRTr" divGCRT v
divG (CRTe v) = Just $ CRTe $ fromJust' "UCyc.divG CRTe" divGCRT v

-- | Yield the scaled squared norm of @g_m \cdot e@ under
-- the canonical embedding, namely,
-- @\hat{m}^{ -1 } \cdot || \sigma(g_m \cdot e) ||^2@ .
gSqNorm :: (Ring r, Tensor t, Fact m, TElt t r) => UCyc t m D r -> r
gSqNorm (Dec v) = gSqNormDec v
{-# INLINABLE gSqNorm #-}

-- | Sample from the "tweaked" Gaussian error distribution @t*D@ in
-- the decoding basis, where @D@ has scaled variance @v@.
tGaussian :: (Tensor t, Fact m, OrdFloat q, Random q, TElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (UCyc t m D q)
tGaussian = fmap Dec . tGaussianDec
{-# INLINABLE tGaussian #-}

-- | Generate an LWE error term from the "tweaked" Gaussian with given
-- scaled variance, deterministically rounded using the decoding
-- basis. (Note: This
-- implementation uses Double precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.)
errorRounded :: forall v rnd t m z .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd)
                => v -> rnd (UCyc t m D z)
{-# INLINABLE errorRounded #-}
errorRounded svar =
  Dec . fmapT (roundMult one) <$> (tGaussianDec svar :: rnd (t m Double))

-- | Generate an LWE error term from the "tweaked" Gaussian with
-- scaled variance @v * p^2@, deterministically rounded to the given
-- coset of @R_p@ using the decoding basis. (Note: This
-- implementation uses Double precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.)
errorCoset :: forall t m zp z v rnd .
  (Mod zp, z ~ ModRep zp, Lift zp z, Tensor t, Fact m,
   TElt t zp, TElt t z, ToRational v, MonadRandom rnd)
  => v -> UCyc t m D zp -> rnd (UCyc t m D z)
{-# INLINABLE errorCoset #-}
errorCoset =
  let pval = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  in \ svar c -> do err <- tGaussian (svar*pval*pval) :: rnd (UCyc t m D Double)
                    return $! roundCoset <$> c <*> err


----- inter-ring operations

-- | Embed into an extension ring, for the powerful basis.
embedPow :: (Additive r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m P r -> UCyc t m' P r
embedPow (Pow v) = Pow $ T.embedPow v
{-# INLINABLE embedPow #-}

-- | Embed into an extension ring, for the decoding basis.
embedDec :: (Additive r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m D r -> UCyc t m' D r
embedDec (Dec v) = Dec $ T.embedDec v
{-# INLINABLE embedDec #-}

-- | Embed into an extension ring, for a CRT basis.  (The output is
-- an 'Either' because in some cases it is most efficient to preserve
-- the 'UCyc' internal invariant by producing output with respect to
-- the powerful basis.)
embedCRT :: forall t m m' r . (m `Divides` m', UCElt t r)
            => UCyc t m C r -> Either (UCyc t m' P r) (UCyc t m' C r)
{-# INLINABLE embedCRT #-}
embedCRT x@(CRTr v) = fromMaybe (Left $ embedPow $ toPow x)
                      (Right . CRTr <$> (T.embedCRT <*> pure v))
embedCRT x@(CRTe v) =
    -- preserve invariant: CRTe iff CRTr is invalid for m'
    fromMaybe (Right $ CRTe $ fromJust' "UCyc.embedCRT CRTe" T.embedCRT v)
              (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) A.*>
               pure (Left $ embedPow $ toPow x))

-- | Twace into a subring, for the powerful basis.
twacePow :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m' P r -> UCyc t m P r
twacePow (Pow v) = Pow $ twacePowDec v
{-# INLINABLE twacePow #-}

-- | Twace into a subring, for the decoding basis.
twaceDec :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m' D r -> UCyc t m D r
twaceDec (Dec v) = Dec $ twacePowDec v
{-# INLINABLE twaceDec #-}

-- | Twace into a subring, for a CRT basis.  (The output is an
-- 'Either' because in some cases it is most efficient to preserve the
-- 'UCyc' internal invariant by producing output with respect to the
-- powerful basis.)
twaceCRT :: forall t m m' r . (m `Divides` m', UCElt t r)
            => UCyc t m' C r -> Either (UCyc t m P r) (UCyc t m C r)
{-# INLINABLE twaceCRT #-}
twaceCRT x@(CRTr v) =
  -- stay in CRTr only iff it's valid for target, else go to Pow
  fromMaybe (Left $ twacePow $ toPow x) (Right . CRTr <$> (T.twaceCRT <*> pure v))
twaceCRT x@(CRTe v) =
  -- stay in CRTe iff CRTr is invalid for target, else go to Pow
  fromMaybe (Right $ CRTe $ fromJust' "UCyc.twace CRTe" T.twaceCRT v)
            (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) A.*>
             Just (Left $ twacePow $ toPow x))

-- | Yield the @O_m@-coefficients of an @O_m'@-element, with respect to
-- the relative powerful @O_m@-basis.
coeffsPow :: (Ring r, Tensor t, m `Divides` m', TElt t r)
             => UCyc t m' P r -> [UCyc t m P r]
{-# INLINABLE coeffsPow #-}
coeffsPow (Pow v) = LP.map Pow $ coeffs v

-- | Yield the @O_m@-coefficients of an @O_m'@ element, with respect to
-- the relative decoding @O_m@-basis.
coeffsDec :: (Ring r, Tensor t, m `Divides` m', TElt t r)
             => UCyc t m' D r -> [UCyc t m D r]
{-# INLINABLE coeffsDec #-}
coeffsDec (Dec v) = LP.map Dec $ coeffs v

-- | The relative powerful basis of @O_m' / O_m@.
powBasis :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => Tagged m [UCyc t m' P r]
{-# INLINABLE powBasis #-}
powBasis = (Pow <$>) <$> powBasisPow

-- | The relative mod-@r@ CRT set of @O_m' / O_m@, represented with
-- respect to the powerful basis (which seems to be the best choice
-- for typical use cases).
crtSet :: forall t m m' r p mbar m'bar .
           (m `Divides` m', ZPP r, p ~ CharOf (ZpOf r),
            mbar ~ PFree p m, m'bar ~ PFree p m',
            UCElt t r, TElt t (ZpOf r))
           => Tagged m [UCyc t m' P r]
{-# INLINABLE crtSet #-}
crtSet =
  -- CJP: consider using traceEvent or traceMarker
  --DT.trace ("UCyc.crtSet: m = " ++
  --          show (proxy valueFact (Proxy::Proxy m)) ++ ", m'= " ++
  --          show (proxy valueFact (Proxy::Proxy m'))) $
  let (p,e) = proxy modulusZPP (Proxy::Proxy r)
      pp = Proxy::Proxy p
      pm = Proxy::Proxy m
      pm' = Proxy::Proxy m'
  in retag (fmap (embedPow .
                  (if e > 1 then toPow . (^(p^(e-1))) . toCRT else toPow) .
                  Dec . fmapT liftZp) <$>
            (crtSetDec :: Tagged mbar [t m'bar (ZpOf r)]))
     \\ pFreeDivides pp pm pm' \\ pSplitTheorems pp pm \\ pSplitTheorems pp pm'


--------- Conversion methods ------------------


-- | Convert to powerful-basis representation.
toPow :: (Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v
toPow (CRTr v) = Pow $ fromJust' "UCyc.toPow CRTr" crtInv v
toPow (CRTe v) =
    Pow $ fmapT fromExt $ fromJust' "UCyc.toPow CRTe" crtInv v

-- | Convert to decoding-basis representation.
toDec :: (Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m D r
{-# INLINABLE toDec #-}
toDec (Pow v) = Dec $ lInv v
toDec x@(Dec _) = x
toDec x@(CRTr _) = toDec $ toPow x
toDec x@(CRTe _) = toDec $ toPow x

-- | Convert to a CRT-basis representation.
toCRT :: forall t m rep r . (Fact m, UCElt t r)
         => UCyc t m rep r -> UCyc t m C r
{-# INLINABLE toCRT #-}
toCRT = let crte = CRTe . fromJust' "UCyc.toCRT: no crt for Ext" crt
            crtr = fmap (CRTr .) crt
            fromPow :: t m r -> UCyc t m C r
            fromPow v = fromMaybe (crte $ fmapT toExt v) (crtr <*> Just v)
        in \x -> case x of
                   (CRTr _) -> x
                   (CRTe _) -> x
                   (Pow v) -> fromPow v
                   (Dec v) -> fromPow $ l v


---------- Category-theoretic instances ----------

-- CJP: no Applicative, Foldable, Traversable for C because types
-- (and math) don't work out for the CRTe case.

instance (Tensor t, Fact m) => Functor (UCyc t m P) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

instance (Tensor t, Fact m) => Functor (UCyc t m D) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

instance (Tensor t, Fact m) => Applicative (UCyc t m P) where
  pure = Pow . pure \\ proxy entailIndexT (Proxy::Proxy (t m r))
  (Pow f) <*> (Pow v) = Pow $ f <*> v \\ witness entailIndexT v

  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}

instance (Tensor t, Fact m) => Applicative (UCyc t m D) where
  pure = Dec . pure \\ proxy entailIndexT (Proxy::Proxy (t m r))
  (Dec f) <*> (Dec v) = Dec $ f <*> v \\ witness entailIndexT v

  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}


instance (Tensor t, Fact m) => Foldable (UCyc t m P) where
  {-# INLINABLE foldr #-}
  foldr f b (Pow v) = F.foldr f b v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Foldable (UCyc t m D) where
  {-# INLINABLE foldr #-}
  foldr f b (Dec v) = F.foldr f b v \\ witness entailIndexT v


instance (Tensor t, Fact m) => Traversable (UCyc t m P) where
  {-# INLINABLE traverse #-}
  traverse f (Pow v) = Pow <$> traverse f v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Traversable (UCyc t m D) where
  {-# INLINABLE traverse #-}
  traverse f (Dec v) = Dec <$> traverse f v \\ witness entailIndexT v


---------- Utility instances ----------

instance (Random r, Tensor t, Fact m, CRTElt t r)
         => Random (Either (UCyc t m P r) (UCyc t m C r)) where

  -- create in CRTr basis if possible, otherwise in powerful
  random = let cons = fromMaybe (Left . Pow)
                      (proxyT hasCRTFuncs (Proxy::Proxy (t m r))
                       >> Just (Right . CRTr))
           in \g -> let (v,g') = random g \\ witness entailRandomT v
                    in (cons v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Show r, Show (CRTExt r), Tensor t, Fact m, TElt t r, TElt t (CRTExt r))
    => Show (UCyc t m rep r) where
  show (Pow  v) = "UCyc Pow: "  ++ show v \\ witness entailShowT v
  show (Dec  v) = "UCyc Dec: "  ++ show v \\ witness entailShowT v
  show (CRTr v) = "UCyc CRTr: " ++ show v \\ witness entailShowT v
  show (CRTe v) = "UCyc CRTe: " ++ show v \\ witness entailShowT v

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m P r) where
  arbitrary = Pow <$> arbitrary
  shrink = shrinkNothing

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m D r) where
  arbitrary = Dec <$> arbitrary
  shrink = shrinkNothing

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m C r) where
  arbitrary = CRTr <$> arbitrary
  shrink = shrinkNothing

instance (Tensor t, Fact m, NFElt r, TElt t r, TElt t (CRTExt r))
         => NFData (UCyc t m rep r) where
  rnf (Pow x)    = rnf x \\ witness entailNFDataT x
  rnf (Dec x)    = rnf x \\ witness entailNFDataT x
  rnf (CRTr x)   = rnf x \\ witness entailNFDataT x
  rnf (CRTe x)   = rnf x \\ witness entailNFDataT x
