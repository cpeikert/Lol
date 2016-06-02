{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

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
import           Crypto.Lol.Cyclotomic.CRTSentinel
import qualified Crypto.Lol.Cyclotomic.Tensor      as T
import           Crypto.Lol.Prelude                as LP
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

-- | Represents a cyclotomic ring such as @Z[zeta]@,
-- @Zq[zeta]@, and @Q(zeta)@ in an explicit representation: @t@ is the
-- 'Tensor' type for storing coefficient tensors; @m@ is the
-- cyclotomic index; @rep@ is the representation ('P', 'D', or 'C');
-- @r@ is the base ring of the coefficients (e.g., @Z@, @Zq@).
--
-- The 'Functor', 'Applicative', 'Foldable' and 'Traversable'
-- instances all work coefficient-wise (in the specified basis).
data UCyc t (m :: Factored) rep r where
  Pow  :: !(t m r) -> UCyc t m P r
  Dec  :: !(t m r) -> UCyc t m D r
  -- Use CRTSentinel to enforce invariant that exactly one of these
  -- can be created for a given (t,m,r).
  CRTC :: !(CSentinel t m r) -> !(t m r) -> UCyc t m C r
  CRTE :: !(ESentinel t m r) -> !(t m (CRTExt r)) -> UCyc t m E r

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
scalarCRT r = case crtSentinel of
  Right s -> Right $ CRTC s $ scalarCRTCS s r
  Left  s -> Left  $ CRTE s $ runIdentity T.scalarCRT $ toExt r
{-# INLINABLE scalarCRT #-}

-- Eq instances

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m D r) where
  (Dec v1) == (Dec v2) = v1 == v2 \\ witness entailEqT v1
  {-# INLINABLE (==) #-}

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m C r) where
  (CRTC _ v1) == (CRTC _ v2) = v1 == v2 \\ witness entailEqT v1
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
  isZero (CRTC _ v) = isZero v \\ witness entailZTT v
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
  (Right (CRTC s v1)) + (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithT (+) v1 v2
  (Left (CRTE s v1)) + (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithT (+) v1 v2
  _ + _ = error "UCyc (+) internal error: mixed CRTC/CRTE"

  (Right (CRTC s v1)) - (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithT (-) v1 v2
  (Left (CRTE s v1)) - (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithT (-) v1 v2
  _ - _ = error "UCyc (-) internal error: mixed CRTC/CRTE"

  negate (Right (CRTC s v)) = Right $ CRTC s $ fmapT negate v
  negate (Left (CRTE s v)) = Left $ CRTE s $ fmapT negate v

  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

-- Ring instance: only for CRT

instance (Fact m, UCRTElt t r) => Ring.C (UCycEC t m r) where

  one = scalarCRT one
  fromInteger c = scalarCRT $ fromInteger c

  (Right (CRTC s v1)) * (Right (CRTC _ v2)) = Right $ CRTC s $ zipWithT (*) v1 v2
  (Left (CRTE s v1)) * (Left (CRTE _ v2)) = Left $ CRTE s $ zipWithT (*) v1 v2
  _ * _ = error "UCyc internal error: mixed CRTC/CRTE"

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

  r *> (Right (CRTC s v)) = Right $ CRTC s $ fmapT (r *) v
  r *> (Left (CRTE s v)) = Left $ CRTE s $ fmapT (toExt r *) v
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
-- are 'Either' because each target base ring may not support 'C'.
unzipCRTC :: (Tensor t, Fact m, UCRTElt t (a,b), UCRTElt t a, UCRTElt t b)
             => UCyc t m C (a,b)
             -> (Either (UCyc t m P a) (UCyc t m C a),
                 Either (UCyc t m P b) (UCyc t m C b))
unzipCRTC (CRTC s v)
  = let (ac,bc) = unzipT v
        (ap,bp) = Pow *** Pow $ unzipT $ crtInvCS s v
    in (fromMaybe (Left ap) (Right <$> (CRTC <$> crtCSentinel <*> pure ac)),
        fromMaybe (Left bp) (Right <$> (CRTC <$> crtCSentinel <*> pure bc)))

-- | Unzip in the CRT basis over the extension of the base ring.  The
-- output components are 'Either' because each target base might
-- instead support 'C'.
unzipCRTE :: (Tensor t, Fact m, UCRTElt t (a,b), UCRTElt t a, UCRTElt t b)
             => UCyc t m E (a,b)
             -> (Either (UCyc t m P a) (UCyc t m E a),
                 Either (UCyc t m P b) (UCyc t m E b))
unzipCRTE (CRTE _ v)
  = let (ae,be) = unzipT v
        (a',b') = unzipT $ fmapT fromExt $ runIdentity crtInv v
        (ap,bp) = Pow *** Pow $ (a',b')
    in (fromMaybe (Left ap) (Right <$> (CRTE <$> crtESentinel <*> pure ae)),
        fromMaybe (Left bp) (Right <$> (CRTE <$> crtESentinel <*> pure be)))


-- | Multiply by the special element @g@.
mulG :: (Tensor t, Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m rep r
{-# INLINABLE mulG #-}
mulG (Pow v) = Pow $ mulGPow v
mulG (Dec v) = Dec $ mulGDec v
mulG (CRTC s v) = CRTC s $ mulGCRTCS s v
mulG (CRTE s v) = CRTE s $ runIdentity mulGCRT v

-- | Divide by the special element @g@.
divG :: (Tensor t, Fact m, UCRTElt t r, ZeroTestable r, IntegralDomain r)
        => UCyc t m rep r -> Maybe (UCyc t m rep r)
{-# INLINABLE divG #-}
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
divG (CRTC s v) = Just $ CRTC s $ divGCRTCS s v
divG (CRTE s v) = Just $ CRTE s $ runIdentity divGCRT v

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
  in \ svar c -> do err :: UCyc t m D Double <- tGaussian (svar*pval*pval)
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
-- {-# SPECIALIZE toPow :: (Fact m, Reflects q Int64) => UCyc RT m D (ZqBasic q Int64) -> UCyc RT m P (ZqBasic q Int64) #-}
-- EAC: I can't specialize toPow due to the constraint synonym TElt. See GHC ticket 12068
-- For future reference, it seemed to help in general to simplify constraints as much as possible
-- (i.e. replacing (Ring (ZqBasic q z)) with (Ring z, Reflects t z)) and
-- removing type synonyms wherever possible.

{-
EAC: I tried specializing the function
toPow :: (Fact m, C) for C \subseteq [Tensor t , CRTEmbed r , CRTrans Maybe r , TElt t r , CRTrans Identity (CRTExt r) , TElt t (CRTExt r)]
using
{-# SPECIALIZE toPow :: (Fact m, Reflects q Int64) => UCyc RT m D (ZqBasic q Int64) -> UCyc RT m P (ZqBasic q Int64) #-}

The results for subsets with (Tensor t) were identical to those without (Tensor t), so we ignore (Tensor t) in what follows

There were three outcomes:
1. "Good": no warnings.
2. "NB": Warning: Forall'd constraint ‘Reflects k q Int64’ is not bound in RULE lhs
3. "Bad": Warning: RULE left-hand side too complicated to desugar

BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r,                              TElt t (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r
BAD:  CRTEmbed r, CRTrans Maybe r,           CRTrans Identity (CRTExt r), TElt t (CRTExt r)
good: CRTEmbed r, CRTrans Maybe r,           CRTrans Identity (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r,                                        TElt t (CRTExt r)
good: CRTEmbed r, CRTrans Maybe r
BAD:  CRTEmbed r,                  TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:  CRTEmbed r,                  TElt t r, CRTrans Identity (CRTExt r)
BAD:  CRTEmbed r,                  TElt t r,                              TElt t (CRTExt r)
good: CRTEmbed r,                  TElt t r
BAD:  CRTEmbed r,                            CRTrans Identity (CRTExt r), TElt t (CRTExt r)
good: CRTEmbed r,                            CRTrans Identity (CRTExt r)
good: CRTEmbed r,                                                         TElt t (CRTExt r)
good: CRTEmbed r
BAD:              CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
good:             CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r)
BAD:              CRTrans Maybe r, TElt t r,                              TElt t (CRTExt r)
good:             CRTrans Maybe r, TElt t r
good:             CRTrans Maybe r,           CRTrans Identity (CRTExt r), TElt t (CRTExt r)
good:             CRTrans Maybe r,           CRTrans Identity (CRTExt r)
good:             CRTrans Maybe r,                                        TElt t (CRTExt r)
good:             CRTrans Maybe r
NB:                                TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
NB:                                TElt t r, CRTrans Identity (CRTExt r)
NB:                                TElt t r,                              TElt t (CRTExt r)
NB:                                TElt t r
NB:                                          CRTrans Identity (CRTExt r), TElt t (CRTExt r)
NB:                                          CRTrans Identity (CRTExt r)
NB:                                                                       TElt t (CRTExt r)
NB:

Assigning variables to constraints as follows,

A=CRTEmbed r
B=CRTrans Maybe r
C=TElt t r
D=CRTrans Identity (CRTExt r)
E=TElt t (CRTExt r)

using outcome values

BAD=0
NB=0
GOOD=1

the formula for results is

y=(-A)*B*(-C) + (-A)*B*(-E) + A*(-C)*(-E) + A*(-B)*(-C)*(-D) + A*(-B)*(-D)*(-E)

Below this line, I've reordered the subsets into logical groups.

-- always nb if we don't have either of (CRTEmbed r) or (CRTrans Maybe r)
-- I think this error makes sense: type families aren't injective, so references
-- to TElt and CRTExt may not refer to 'r' on their RHS.
NB:                                TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
NB:                                TElt t r, CRTrans Identity (CRTExt r)
NB:                                TElt t r,                              TElt t (CRTExt r)
NB:                                TElt t r
NB:                                          CRTrans Identity (CRTExt r), TElt t (CRTExt r)
NB:                                          CRTrans Identity (CRTExt r)
NB:                                                                       TElt t (CRTExt r)
NB:

-- always bad if we have both TElt constraints.
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r,                              TElt t (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:  CRTEmbed r,                  TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:              CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:  CRTEmbed r,                  TElt t r,                              TElt t (CRTExt r)
BAD:              CRTrans Maybe r, TElt t r,                              TElt t (CRTExt r)

-- always good if we don't have either TElt constraint.
good: CRTEmbed r, CRTrans Maybe r
good: CRTEmbed r, CRTrans Maybe r,           CRTrans Identity (CRTExt r)
good: CRTEmbed r,                            CRTrans Identity (CRTExt r)
good:             CRTrans Maybe r,           CRTrans Identity (CRTExt r)
good: CRTEmbed r
good:             CRTrans Maybe r

-- bad if we have both (CRTEmbed r) and (CRTrans Maybe r) with at least one of the TElt constraints
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r, TElt t r
BAD:  CRTEmbed r, CRTrans Maybe r,           CRTrans Identity (CRTExt r), TElt t (CRTExt r)
BAD:  CRTEmbed r, CRTrans Maybe r,                                        TElt t (CRTExt r)

-- a few more good cases: symmetric for (CRTEmbed r)  and (CRTrans Maybe r)
good: CRTEmbed r,                  TElt t r
good:             CRTrans Maybe r, TElt t r
good: CRTEmbed r,                                                         TElt t (CRTExt r)
good:             CRTrans Maybe r,                                        TElt t (CRTExt r)

-- strange cases: works for (CRTrans Maybe r), fails for (CRTEmbed r)
--   removing the (Ring (CRTExt r)) superclass constraint from CRTEmbed makes all four of these work.
--   (but doesn't change the behavior of other failing cases)
BAD:  CRTEmbed r,                  TElt t r, CRTrans Identity (CRTExt r)
good:             CRTrans Maybe r, TElt t r, CRTrans Identity (CRTExt r)
BAD:  CRTEmbed r,                            CRTrans Identity (CRTExt r), TElt t (CRTExt r)
good:             CRTrans Maybe r,           CRTrans Identity (CRTExt r), TElt t (CRTExt r)

-}

toPow :: (Fact m, UCRTElt t r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v
toPow (CRTC s v) = Pow $ crtInvCS s v
toPow (CRTE _ v) = Pow $ fmapT fromExt $ runIdentity crtInv v

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
toDec x@(CRTC _ _) = toDec $ toPow x
toDec x@(CRTE _ _) = toDec $ toPow x

-- | Convert to a CRT-basis representation.
toCRT :: forall t m rep r . (Fact m, UCRTElt t r)
         => UCyc t m rep r -> UCycEC t m r
{-# INLINABLE toCRT #-}
toCRT = let fromPow :: t m r -> UCycEC t m r
            fromPow = case crtSentinel of
              Right s -> Right . CRTC s . crtCS s
              Left  s -> Left  . CRTE s . runIdentity crt . fmapT toExt
        in \x -> case x of
                   (CRTC _ _) -> Right x
                   (CRTE _ _) -> Left x
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

-- CJP: no Functor instance for C, because CRTrans for a doesn't imply
-- it for b.

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

-- CJP: no Applicative instance for C, because 'pure' would circumvent
-- the invariant.  Moreover, having CRTrans for (a -> b) and for a
-- doesn't imply it for b.

instance (Tensor t, Fact m) => Foldable (UCyc t m P) where
  {-# INLINABLE foldr #-}
  foldr f b (Pow v) = F.foldr f b v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Foldable (UCyc t m D) where
  {-# INLINABLE foldr #-}
  foldr f b (Dec v) = F.foldr f b v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Foldable (UCyc t m C) where
  {-# INLINABLE foldr #-}
  foldr f b (CRTC _ v) = F.foldr f b v \\ witness entailIndexT v


instance (Tensor t, Fact m) => Traversable (UCyc t m P) where
  {-# INLINABLE traverse #-}
  traverse f (Pow v) = Pow <$> traverse f v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Traversable (UCyc t m D) where
  {-# INLINABLE traverse #-}
  traverse f (Dec v) = Dec <$> traverse f v \\ witness entailIndexT v

-- CJP: no Traversable instance for C, because CRTrans for a doesn't
-- imply it for b.


---------- Utility instances ----------

instance (Random r, UCRTElt t r, Fact m)
         => Random (UCyc t m P r) where

  random g = let (v,g') = random g \\ witness entailRandomT v
             in (Pow v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Random r, UCRTElt t r, Fact m)
         => Random (UCyc t m D r) where

  random g = let (v,g') = random g \\ witness entailRandomT v
             in (Dec v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Random r, UCRTElt t r, Fact m)
         => Random (Either (UCyc t m P r) (UCyc t m C r)) where

  -- create in CRTC basis if possible, otherwise in powerful
  random = let cons = case crtSentinel of
                 Left  _ -> Left  . Pow
                 Right s -> Right . CRTC s
           in \g -> let (v,g') = random g \\ witness entailRandomT v
                    in (cons v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m P r) where
  arbitrary = Pow <$> arbitrary
  shrink = shrinkNothing

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m D r) where
  arbitrary = Dec <$> arbitrary
  shrink = shrinkNothing

-- no Arbitrary for C or E due to invariant

instance (Tensor t, Fact m, NFElt r, TElt t r, TElt t (CRTExt r))
         => NFData (UCyc t m rep r) where
  rnf (Pow x)      = rnf x \\ witness entailNFDataT x
  rnf (Dec x)      = rnf x \\ witness entailNFDataT x
  rnf (CRTC _ x)   = rnf x \\ witness entailNFDataT x
  rnf (CRTE _ x)   = rnf x \\ witness entailNFDataT x

instance (Fact m, Protoable (t m r)) => Protoable (UCyc t m D r) where
  type ProtoType (UCyc t m D r) = ProtoType (t m r)
  toProto (Dec t) = toProto t
  fromProto t = Dec <$> fromProto t
