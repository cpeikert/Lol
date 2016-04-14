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
  UCyc, P, D, C, E, UCycEC, UCRTElt, NFElt
-- * Changing representation
, toPow, toDec, toCRT, fmapPow, fmapDec
, unzipPow, unzipDec, unzipCRTC, unzipCRTE
-- * Scalars
, scalarPow, scalarCRT
-- * Basic operations
, mulG, divG, gSqNorm
-- * Error sampling
, tGaussian, errorRounded, errorCoset
-- * Inter-ring operations and values
, embedPow, embedDec, embedCRTC, embedCRTE
, twacePow, twaceDec, twaceCRTC, twaceCRTE
, coeffsPow, coeffsDec, powBasis, crtSet
) where

import Crypto.Lol.Cyclotomic.Tensor hiding (embedCRT, embedDec, embedPow,
                                            scalarCRT, scalarPow, twaceCRT)

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
import Control.Monad.Identity
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
-- | Nullary index type representing the CRT basis over base ring.
data C
-- | Nullary index type representing the CRT basis over extension of
-- base ring.
data E

-- | Convenient synonym for either CRT representation.
type UCycEC t m r = Either (UCyc t m E r) (UCyc t m C r)

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
  -- Internal invariant: for a given (t,m,r), exactly one of these two
  -- types can have values created : CRTr if crtFuncs exists,
  -- otherwise CRTe
  CRTr :: !(t m r) -> UCyc t m C r
  CRTe :: !(t m (CRTExt r)) -> UCyc t m E r

-- | Constraints needed for CRT-related operations on 'UCyc' data.
type UCRTElt t r = (Tensor t, CRTEmbed r,
                    CRTrans Maybe r, TElt t r,
                    CRTrans Identity (CRTExt r), TElt t (CRTExt r))

-- | Convenient synonym for 'deepseq'-able element type.
type NFElt r = (NFData r, NFData (CRTExt r))

-- | Embed a scalar from the base ring.
scalarPow :: (Tensor t, Fact m, Ring r, TElt t r) => r -> UCyc t m P r
scalarPow = Pow . T.scalarPow
{-# INLINABLE scalarPow #-}

-- | Embed a scalar from the base ring.
scalarCRT :: (Fact m, UCRTElt t r) => r -> UCycEC t m r
scalarCRT = fromMaybe
            (Left . CRTe . runIdentity T.scalarCRT . toExt)
            (((Right .) CRTr .) <$> T.scalarCRT)
{-# INLINABLE scalarCRT #-}

-- Eq instances

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m D r) where
  (Dec v1) == (Dec v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m C r) where
  (CRTr v1) == (CRTr v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

-- no Eq instance for E due to precision

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

instance (ZeroTestable r, Tensor t, Fact m, TElt t r)
    => ZeroTestable.C (UCyc t m C r) where
  isZero (CRTr v) = isZero v \\ witness entailZTT v
  {-# INLINABLE isZero #-}

-- no ZT instance for E due to precision

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

-- no Additive instances for C/E alone, because 'zero' would violate
-- 'UCyc' invariant if C/E were invalid representations

instance (Fact m, UCRTElt t r) => Additive.C (UCycEC t m r) where

  zero = scalarCRT zero

  -- CJP: precision OK?  Alternatively, switch to pow and back after
  -- adding/subtracting.  Expensive, but necessary given output type.
  (Right (CRTr v1)) + (Right (CRTr v2)) = Right $ CRTr $ zipWithT (+) v1 v2
  (Left (CRTe v1)) + (Left (CRTe v2)) = Left $ CRTe $ zipWithT (+) v1 v2
  _ + _ = error "UCyc (+) internal error: mixed CRTr/CRTe"

  (Right (CRTr v1)) - (Right (CRTr v2)) = Right $ CRTr $ zipWithT (-) v1 v2
  (Left (CRTe v1)) - (Left (CRTe v2)) = Left $ CRTe $ zipWithT (-) v1 v2
  _ - _ = error "UCyc (-) internal error: mixed CRTr/CRTe"

  negate (Right (CRTr v)) = Right $ CRTr $ fmapT negate v
  negate (Left (CRTe v)) = Left $ CRTe $ fmapT negate v

  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- Ring instance: only for CRT

instance (Fact m, UCRTElt t r) => Ring.C (UCycEC t m r) where
  
  one = scalarCRT one
  fromInteger c = scalarCRT $ fromInteger c

  (Right (CRTr v1)) * (Right (CRTr v2)) = Right $ CRTr $ zipWithT (*) v1 v2
  (Left (CRTe v1)) * (Left (CRTe v2)) = Left $ CRTe $ zipWithT (*) v1 v2
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

instance (Ring r, Fact m, UCRTElt t r) => Module.C r (UCycEC t m r) where

  r *> (Right (CRTr v)) = Right $ CRTr $ fmapT (r *) v
  r *> (Left (CRTe v)) = Left $ CRTe $ fmapT (toExt r *) v
  {-# INLINABLE (*>) #-}

instance (GFCtx fp d, Fact m, Tensor t, TElt t fp)
         => Module.C (GF fp d) (UCyc t m P fp) where
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

-- CJP: no Reduce for C because CRT basis may not exist for target
-- type

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

-- CJP: no Lift' for C because CRT basis may not exist for target type

instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m P a) (UCyc t m P b) where
  rescale (Pow v) = Pow $ fmapT rescale v
  {-# INLINABLE rescale #-}

instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m D a) (UCyc t m D b) where
  rescale (Dec v) = Dec $ fmapT rescale v
  {-# INLINABLE rescale #-}

-- CJP: no Rescale for C because CRT basis may not exist for target
-- type

-- CJP: we don't instantiate RescaleCyc because it requires changing bases

-- CJP: we don't instantiate Gadget etc., because (1) their methods
-- wouldn't be efficient, and (2) their superclass constraints are not
-- satisfied anyway (e.g., Ring for P rep).


-- | Type-restricted (and potentially more efficient) 'fmap' for
-- powerful-basis representation.
fmapPow :: (Tensor t, Fact m, TElt t a, TElt t b)
           => (a -> b) -> UCyc t m P a -> UCyc t m P b
fmapPow f (Pow v) = Pow $ fmapT f v
{-# INLINABLE fmapPow #-}

-- | Type-restricted (and potentially more efficient) 'fmap' for
-- decoding-basis representation.
fmapDec :: (Tensor t, Fact m, TElt t a, TElt t b)
           => (a -> b) -> UCyc t m D a -> UCyc t m D b
fmapDec f (Dec v) = Dec $ fmapT f v
{-# INLINABLE fmapDec #-}

-- | Unzip in the powerful basis.
unzipPow :: (Tensor t, Fact m, TElt t (a,b), TElt t a, TElt t b)
            => UCyc t m P (a,b) -> (UCyc t m P a, UCyc t m P b)
{-# INLINABLE unzipPow #-}
unzipPow (Pow v) = Pow *** Pow $ unzipT v

-- | Unzip in the decoding basis.
unzipDec :: (Tensor t, Fact m, TElt t (a,b), TElt t a, TElt t b)
            => UCyc t m D (a,b) -> (UCyc t m D a, UCyc t m D b)
{-# INLINABLE unzipDec #-}
unzipDec (Dec v) = Dec *** Dec $ unzipT v

-- | Unzip in the CRT basis over the base ring.  The output components
-- are 'Either's because each target base ring may not support 'C'.
unzipCRTC :: (Tensor t, Fact m, UCRTElt t (a,b), UCRTElt t a, UCRTElt t b)
             => UCyc t m C (a,b)
             -> (Either (UCyc t m P a) (UCyc t m C a),
                 Either (UCyc t m P b) (UCyc t m C b))
unzipCRTC (CRTr v)
  = let (a,b) = unzipT v
        (ac,bc) = CRTr *** CRTr $ (a,b)
        -- safe because we're already in CRT C
        crtinv = fromJust' "UCyc unzipCRTC" crtInv
        (ap,bp) = Pow *** Pow $ unzipT $ crtinv v
    in (fromMaybe (Left ap) (witnessT hasCRTFuncs a A.*> pure (Right ac)),
        fromMaybe (Left bp) (witnessT hasCRTFuncs b A.*> pure (Right bc)))

-- | Unzip in the CRT basis over the extension of the base ring.  The
-- output components are 'Either's because each target base might
-- instead support 'C'.
unzipCRTE :: (Tensor t, Fact m, UCRTElt t (a,b), UCRTElt t a, UCRTElt t b)
             => UCyc t m E (a,b)
             -> (Either (UCyc t m P a) (UCyc t m E a),
                 Either (UCyc t m P b) (UCyc t m E b))
unzipCRTE (CRTe v)
  = let (ae,be) = CRTe *** CRTe $ unzipT v
        (a',b') = unzipT $ fmapT fromExt $ runIdentity crtInv v
        (ap,bp) = Pow *** Pow $ (a',b')
    in (fromMaybe (Right ae) (witnessT hasCRTFuncs a' A.*> pure (Left ap)),
        fromMaybe (Right be) (witnessT hasCRTFuncs b' A.*> pure (Left bp)))


-- | Multiply by the special element @g@.
mulG :: (Tensor t, Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m rep r
{-# INLINABLE mulG #-}
mulG (Pow v) = Pow $ mulGPow v
mulG (Dec v) = Dec $ mulGDec v
-- fromJust is safe here because we're already in CRTr
mulG (CRTr v) = CRTr $ fromJust' "UCyc.mulG CRTr" mulGCRT v
mulG (CRTe v) = CRTe $ runIdentity mulGCRT v

-- | Divide by the special element @g@.
divG :: (Tensor t, Fact m, UCRTElt t r, ZeroTestable r, IntegralDomain r)
        => UCyc t m rep r -> Maybe (UCyc t m rep r)
{-# INLINABLE divG #-}
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
-- fromJust is OK here because we're already in CRTr
divG (CRTr v) = Just $ CRTr $ fromJust' "UCyc.divG CRTr" divGCRT v
divG (CRTe v) = Just $ CRTe $ runIdentity divGCRT v

-- | Yield the scaled squared norm of @g_m \cdot e@ under
-- the canonical embedding, namely,
-- @\hat{m}^{ -1 } \cdot || \sigma(g_m \cdot e) ||^2@ .
gSqNorm :: (Ring r, Tensor t, Fact m, TElt t r) => UCyc t m D r -> r
gSqNorm (Dec v) = gSqNormDec v
{-# INLINABLE gSqNorm #-}

-- | Sample from the "tweaked" Gaussian error distribution @t*D@ in
-- the decoding basis, where @D@ has scaled variance @v@.  (Note: This
-- implementation uses Double precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.)
tGaussian :: (Tensor t, Fact m, OrdFloat q, Random q, TElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (UCyc t m D q)
tGaussian = fmap Dec . tGaussianDec
{-# INLINABLE tGaussian #-}

-- | Generate an LWE error term from the "tweaked" Gaussian with given
-- scaled variance, deterministically rounded using the decoding
-- basis.
errorRounded :: forall v rnd t m z .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd)
                => v -> rnd (UCyc t m D z)
{-# INLINABLE errorRounded #-}
errorRounded svar =
  Dec . fmapT (roundMult one) <$> (tGaussianDec svar :: rnd (t m Double))

-- | Generate an LWE error term from the "tweaked" Gaussian with
-- scaled variance @v * p^2@, deterministically rounded to the given
-- coset of @R_p@ using the decoding basis.
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

-- | Embed into an extension ring, for the CRT basis.  (The output is
-- an 'Either' because the extension ring might not support 'C'.)
embedCRTC :: (m `Divides` m', UCRTElt t r)
             => UCyc t m C r -> Either (UCyc t m' P r) (UCyc t m' C r)
{-# INLINABLE embedCRTC #-}
embedCRTC x@(CRTr v) = fromMaybe (Left $ embedPow $ toPow x)
                       (Right . CRTr <$> (T.embedCRT <*> pure v))

-- | Similar to 'embedCRTC'.  (The output is an 'Either' because the
-- extension ring might support 'C', in which case we never use 'E'.)
embedCRTE :: forall m m' t r . (m `Divides` m', UCRTElt t r)
             => UCyc t m E r -> Either (UCyc t m' P r) (UCyc t m' E r)
{-# INLINABLE embedCRTE #-}
embedCRTE x@(CRTe v) =
    -- preserve invariant: CRTe iff CRTr is invalid for m'
    fromMaybe (Right $ CRTe $ runIdentity T.embedCRT v)
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

-- | Twace into a subring, for the CRT basis.  (The output is an
-- 'Either' because the subring might not support 'C'.)
twaceCRTC :: (m `Divides` m', UCRTElt t r)
             => UCyc t m' C r -> Either (UCyc t m P r) (UCyc t m C r)
{-# INLINABLE twaceCRTC #-}
twaceCRTC x@(CRTr v) =
  -- stay in CRTr only iff it's valid for target, else go to Pow
  fromMaybe (Left $ twacePow $ toPow x) (Right . CRTr <$> (T.twaceCRT <*> pure v))

-- | Similar to 'twaceCRTC'.  (The output is an 'Either' because the
-- subring might support 'C', in which case we never use 'E'.)
twaceCRTE :: forall t m m' r . (m `Divides` m', UCRTElt t r)
             => UCyc t m' E r -> Either (UCyc t m P r) (UCyc t m E r)
{-# INLINABLE twaceCRTE #-}
twaceCRTE x@(CRTe v) =
  -- stay in CRTe iff CRTr is invalid for target, else go to Pow
  fromMaybe (Right $ CRTe $ runIdentity T.twaceCRT v)
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
            UCRTElt t r, TElt t (ZpOf r))
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
                  (if e > 1 then toPowCE . (^(p^(e-1))) . toCRT else toPow) .
                  Dec . fmapT liftZp) <$>
            (crtSetDec :: Tagged mbar [t m'bar (ZpOf r)]))
     \\ pFreeDivides pp pm pm' \\ pSplitTheorems pp pm \\ pSplitTheorems pp pm'


--------- Conversion methods ------------------


-- | Convert to powerful-basis representation.
toPow :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v
toPow (CRTr v) = Pow $ fromJust' "UCyc.toPow CRTr" crtInv v
toPow (CRTe v) = Pow $ fmapT fromExt $ runIdentity crtInv v

-- | Convenient version of 'toPow' for 'Either' CRT basis type.
toPowCE :: (Fact m, UCRTElt t r) => UCycEC t m r -> UCyc t m P r
{-# INLINABLE toPowCE #-}
toPowCE (Left u) = toPow u
toPowCE (Right u) = toPow u

-- | Convert to decoding-basis representation.
toDec :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m D r
{-# INLINABLE toDec #-}
toDec (Pow v) = Dec $ lInv v
toDec x@(Dec _) = x
toDec x@(CRTr _) = toDec $ toPow x
toDec x@(CRTe _) = toDec $ toPow x

-- | Convert to a CRT-basis representation.
toCRT :: forall t m rep r . (Fact m, UCRTElt t r)
         => UCyc t m rep r -> UCycEC t m r
{-# INLINABLE toCRT #-}
toCRT = let crte = Left . CRTe . runIdentity crt
            crtr = ((Right .) CRTr .) <$> crt
            fromPow :: t m r -> UCycEC t m r
            fromPow v = fromMaybe (crte $ fmapT toExt v) (crtr <*> Just v)
        in \x -> case x of
                   (CRTr _) -> Right x
                   (CRTe _) -> Left x
                   (Pow v) -> fromPow v
                   (Dec v) -> fromPow $ l v


---------- Category-theoretic instances ----------

-- CJP: no instances for E because types (and math) don't make sense.

instance (Tensor t, Fact m) => Functor (UCyc t m P) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

instance (Tensor t, Fact m) => Functor (UCyc t m D) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

instance (Tensor t, Fact m) => Functor (UCyc t m C) where
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

instance (Tensor t, Fact m) => Applicative (UCyc t m C) where
  pure = CRTr . pure \\ proxy entailIndexT (Proxy::Proxy (t m r))
  (CRTr f) <*> (CRTr v) = CRTr $ f <*> v \\ witness entailIndexT v

  {-# INLINABLE pure #-}
  {-# INLINABLE (<*>) #-}


instance (Tensor t, Fact m) => Foldable (UCyc t m P) where
  {-# INLINABLE foldr #-}
  foldr f b (Pow v) = F.foldr f b v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Foldable (UCyc t m D) where
  {-# INLINABLE foldr #-}
  foldr f b (Dec v) = F.foldr f b v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Foldable (UCyc t m C) where
  {-# INLINABLE foldr #-}
  foldr f b (CRTr v) = F.foldr f b v \\ witness entailIndexT v


instance (Tensor t, Fact m) => Traversable (UCyc t m P) where
  {-# INLINABLE traverse #-}
  traverse f (Pow v) = Pow <$> traverse f v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Traversable (UCyc t m D) where
  {-# INLINABLE traverse #-}
  traverse f (Dec v) = Dec <$> traverse f v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Traversable (UCyc t m C) where
  {-# INLINABLE traverse #-}
  traverse f (CRTr v) = CRTr <$> traverse f v \\ witness entailIndexT v


---------- Utility instances ----------

instance (Random r, UCRTElt t r, Fact m)
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

-- no Arbitrary for C or EC due to invariant

instance (Tensor t, Fact m, NFElt r, TElt t r, TElt t (CRTExt r))
         => NFData (UCyc t m rep r) where
  rnf (Pow x)    = rnf x \\ witness entailNFDataT x
  rnf (Dec x)    = rnf x \\ witness entailNFDataT x
  rnf (CRTr x)   = rnf x \\ witness entailNFDataT x
  rnf (CRTe x)   = rnf x \\ witness entailNFDataT x
