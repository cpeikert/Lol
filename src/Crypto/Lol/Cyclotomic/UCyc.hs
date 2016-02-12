{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, InstanceSigs, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | An implementation of cyclotomic rings.
--
-- The 'Functor', 'Applicative', 'Foldable', and 'Traversable'
-- instances of 'UCyc', as well as the 'fmapC' and 'fmapCM' functions,
-- work over the element's @r@-basis representation (or
-- 'pure' scalar representation as a special case, to satisfy the
-- 'Applicative' laws), and the output remains in that representation.
-- If the input's representation is not one of these, the result is
-- a runtime error.  To ensure a valid representation when using the
-- methods from these classes, first call 'forceBasis' or one of its
-- specializations ('forcePow', 'forceDec', 'forceAny').
--
-- __WARNING:__ as with all fixed-point arithmetic, the functions
-- associated with 'UCyc' may result in overflow (and thereby
-- incorrect answers and potential security flaws) if the input
-- arguments are too close to the bounds imposed by the base type.
-- The acceptable range of inputs for each function is determined by
-- the internal linear transforms and other operations it performs.

module Crypto.Lol.Cyclotomic.UCyc
(
-- * Data type
  UCyc, P, D, C, UCElt
-- * Changing representation
, toPow, toDec, toCRT, unzipUCyc
-- * Basic operations
, mulG, divG
-- * Error sampling and norm
, tGaussian, errorRounded, errorCoset, gSqNorm
-- * Inter-ring operations and values
, embedPow, embedDec, embedCRT
, twacePow, twaceDec, twaceCRT
, coeffsPow, coeffsDec, powBasis, crtSet
) where

import           Crypto.Lol.CRTrans
import           Crypto.Lol.Cyclotomic.Tensor hiding (embedCRT, embedDec,
                                               embedPow, twaceCRT)
import qualified Crypto.Lol.Cyclotomic.Tensor as T
import           Crypto.Lol.LatticePrelude    as LP hiding ((*>))
import           Crypto.Lol.Types.ZPP

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
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
-- | Nullary index type representing a CRT basis.
data C

-- | Represents cyclotomic rings such as @Z[zeta]@,
-- @Zq[zeta]@, and @Q(zeta)@ in an explicit representation: @t@ is the
-- 'Tensor' type for storing coefficients; @m@ is the cyclotomic
-- index; @rep@ is the representation; @r@ is the base ring of the
-- coefficients (e.g., @Z@, @Zq@).
data UCyc t m rep r where
  Pow  :: !(t m r) -> UCyc t m P r
  Dec  :: !(t m r) -> UCyc t m D r
  -- Invariant: for a given (t,m,r), exactly one of these two is ever
  -- used: CRTr if crtFuncs exists, otherwise CRTe
  CRTr :: !(t m r) -> UCyc t m C r
  CRTe :: !(t m (CRTExt r)) -> UCyc t m C r

{-
  -- super-optimized storage of scalars
  Scalar :: !r -> UCyc t m r

  -- optimized storage of subring elements
  Sub  :: (l `Divides` m) => !(UCyc t l r) -> UCyc t m r
-}

-- | Constraints needed to use CRT ('C') representation for 'UCyc'.
type UCElt t r = (Tensor t, CRTEmbed r, CRTElt t r, CRTElt t (CRTExt r))

scalarCRT' :: (Fact m, UCElt t r) => r -> UCyc t m C r
scalarCRT' = fromMaybe
             (CRTe . fromJust' "UCyc: no CRT over CRTExt" scalarCRT . toExt)
             (liftM (CRTr .) scalarCRT)

-- type CElt t r = (Tensor t, CRTEmbed r, CRTElt t r, CRTElt t (CRTExt r), Eq r, Random r)

{-
-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.scalarCyc', but for 'UCyc'.
scalarCyc :: (Fact m, CElt t a) => a -> UCyc t m a
scalarCyc = Scalar
-}

-- Eq instances

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailEqT v1

instance (Eq r, Tensor t, Fact m, TElt t r) => Eq (UCyc t m D r) where
  (Dec v1) == (Dec v2) = v1 == v2 \\ witness entailEqT v1

{- CJP: No Eq for C due to precision in CRTe case

instance (Eq r, Fact m, TElt t r) => Eq (UCyc t m C r) where
  (CRTr v1) == (CRTr v2) = v1 == v2 \\ witness entailEqT v1

-}


{-
  -- compare in compositum
  (Sub (c1 :: UCyc t l1 r)) == (Sub (c2 :: UCyc t l2 r)) =
    (embed' c1 :: UCyc t (FLCM l1 l2) r) == embed' c2
    \\ lcmDivides (Proxy::Proxy l1) (Proxy::Proxy l2)

  -- otherwise compare in power basis for fidelity, which involves
  -- the most efficient transforms in all cases
  p1 == p2 = forcePow p1 == forcePow p2
-}

---------- Numeric Prelude instances ----------

-- ZeroTestable instances

instance (ZeroTestable r, Tensor t, Fact m, TElt t r)
    => ZeroTestable.C (UCyc t m P r) where
  isZero (Pow v) = isZero v \\ witness entailZTT v

instance (ZeroTestable r, Tensor t, Fact m, TElt t r)
    => ZeroTestable.C (UCyc t m D r) where
  isZero (Dec v) = isZero v \\ witness entailZTT v

{- No ZT for C, due to precision in CRTe case

instance (ZeroTestable r, Fact m, TElt t r) => ZeroTestable.C (UCyc t m C r) where
  isZero (CRTr v) = isZero v \\ witness entailZTT v

-}

{-
  isZero x@(CRTe _) = isZero $ forcePow x
  isZero (Sub c) = isZero c
-}

-- Additive instances

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m P r) where
  zero = Pow $ scalarPow zero
  (Pow v1) + (Pow v2) = Pow $ zipWithT (+) v1 v2
  (Pow v1) - (Pow v2) = Pow $ zipWithT (-) v1 v2
  negate (Pow v) = Pow $ fmapT negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (Additive r, Tensor t, Fact m, TElt t r) => Additive.C (UCyc t m D r) where
  zero = Dec $ scalarDec zero
  (Dec v1) + (Dec v2) = Dec $ zipWithT (+) v1 v2
  (Dec v1) - (Dec v2) = Dec $ zipWithT (-) v1 v2
  negate (Dec v) = Dec $ fmapT negate v
  {-# INLINABLE zero #-}
  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE negate #-}

instance (Fact m, UCElt t r) => Additive.C (UCyc t m C r) where
  zero = scalarCRT' zero

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

{-
instance (CElt t r, Fact m) => Additive.C (UCyc t m r) where

  zero = Scalar zero

  -- optimized addition of zero
  (Scalar c1) + v2 | isZero c1 = v2
  v1 + (Scalar c2) | isZero c2 = v1

  -- SAME CONSTRUCTORS
  (Scalar c1) + (Scalar c2) = Scalar (c1+c2)
  (Pow v1) + (Pow v2) = Pow $ v1 + v2 \\ witness entailRingT v1
  (Dec v1) + (Dec v2) = Dec $ v1 + v2 \\ witness entailRingT v1
  (CRTr v1) + (CRTr v2) = CRTr $ v1 + v2 \\ witness entailRingT v1
  (CRTe v1) + (CRTe v2) = CRTe $ v1 + v2 \\ witness entailRingT v1
  -- Sub plus Sub: work in compositum
  (Sub (c1 :: UCyc t m1 r)) + (Sub (c2 :: UCyc t m2 r)) =
    (Sub $ (embed' c1 :: UCyc t (FLCM m1 m2) r) + embed' c2)
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- SCALAR PLUS SOMETHING ELSE

  p1@(Scalar _) + p2@(Pow _) = forcePow p1 + p2
  p1@(Scalar _) + p2@(Dec _) = forceDec p1 + p2
  p1@(Scalar _) + p2@(CRTr _) = toCRT' p1 + p2
  p1@(Scalar _) + p2@(CRTe _) = toCRT' p1 + p2
  (Scalar c1) + (Sub c2) = Sub $ Scalar c1 + c2 -- must re-wrap Scalar!

  p1@(Pow _) + p2@(Scalar _) = p1 + forcePow p2
  p1@(Dec _) + p2@(Scalar _) = p1 + forceDec p2
  p1@(CRTr _) + p2@(Scalar _) = p1 + toCRT' p2
  p1@(CRTe _) + p2@(Scalar _) = p1 + toCRT' p2
  (Sub c1) + (Scalar c2) = Sub $ c1 + Scalar c2

  -- SUB PLUS NON-SUB, NON-SCALAR: work in full ring
  (Sub c1) + c2 = embed' c1 + c2
  c1 + (Sub c2) = c1 + embed' c2

  -- mixed Dec and Pow: use linear time conversions
  p1@(Dec _) + p2@(Pow _) = forcePow p1 + p2
  p1@(Pow _) + p2@(Dec _) = p1 + forcePow p2

  -- one CRTr: convert other to CRTr
  p1@(CRTr _) + p2 = p1 + toCRT' p2
  p1 + p2@(CRTr _) = toCRT' p1 + p2

  -- else, one is CRTe: convert both to Pow for fidelity and best
  -- efficiency
  p1 + p2 = forcePow p1 + forcePow p2

  negate (Scalar c) = Scalar (negate c)
  negate (Pow v) = Pow $ fmapT negate v
  negate (Dec v) = Dec $ fmapT negate v
  negate (CRTr v) = CRTr $ fmapT negate v
  negate (CRTe v) = CRTe $ fmapT negate v
  negate (Sub c) = Sub $ negate c
-}

-- Ring instances: only for CRT

instance (Fact m, UCElt t r) => Ring.C (UCyc t m C r) where
  one = scalarCRT' one
  fromInteger c = scalarCRT' $ fromInteger c

  -- CJP: precision OK?  Alternatively, switch to pow (and back) after
  -- multiplying.  Expensive, but necessary given output type.
  (CRTr v1) * (CRTr v2) = CRTr $ zipWithT (*) v1 v2
  (CRTe v1) * (CRTe v2) = CRTe $ zipWithT (*) v1 v2
  _ * _ = error "UCyc internal error: mixed CRTr/CRTe"

  {-# INLINABLE one #-}
  {-# INLINABLE fromInteger #-}
  {-# INLINABLE (*) #-}


{-
instance (CElt t r, Fact m) => Ring.C (UCyc t m r) where

  --{-# SPECIALIZE instance Ring.C (UCyc RT F288 (ZqBasic 577 Int64)) #-}

  one = Scalar one

  {-# INLINABLE (*) #-}

  -- optimized mul-by-zero
  v1@(Scalar c1) * _ | isZero c1 = v1
  _ * v2@(Scalar c2) | isZero c2 = v2

  -- BOTH IN A CRT BASIS
  (CRTr v1) * (CRTr v2) = CRTr $ v1 * v2 \\ witness entailRingT v1
  (CRTe v1) * (CRTe v2) = forcePow $ CRTe $ v1 * v2 \\ witness entailRingT v1

  -- CRTr/CRTe mixture
  (CRTr _) * (CRTe _) = error "UCyc.(*): mixed CRTr/CRTe"
  (CRTe _) * (CRTr _) = error "UCyc.(*): mixed CRTr/CRTe"

  -- AT LEAST ONE SCALAR
  (Scalar c1) * (Scalar c2) = Scalar $ c1 * c2

  (Scalar c) * (Pow v) = Pow $ fmapT (*c) v
  (Scalar c) * (Dec v) = Dec $ fmapT (*c) v
  (Scalar c) * (CRTr v) = CRTr $ fmapT (*c) v
  (Scalar c) * (CRTe v) = CRTe $ fmapT (* toExt c) v
  (Scalar c1) * (Sub c2) = Sub $ Scalar c1 * c2

  (Pow v) * (Scalar c) = Pow $ fmapT (*c) v
  (Dec v) * (Scalar c) = Dec $ fmapT (*c) v
  (CRTr v) * (Scalar c) = CRTr $ fmapT (*c) v
  (CRTe v) * (Scalar c) = CRTe $ fmapT (* toExt c) v
  (Sub c1) * (Scalar c2) = Sub $ c1 * Scalar c2

  -- TWO SUBS: work in a CRT rep for compositum
  (Sub (c1 :: UCyc t m1 r)) * (Sub (c2 :: UCyc t m2 r)) =
    -- re-wrap c1, c2 as Subs of the composition, and force them to CRT
    (Sub $ (toCRT' $ Sub c1 :: UCyc t (FLCM m1 m2) r) * toCRT' (Sub c2))
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- ELSE: work in appropriate CRT rep
  p1 * p2 = toCRT' p1 * toCRT' p2

  fromInteger = Scalar . fromInteger
-}


instance (Reduce a b, Tensor t, Fact m, TElt t a, TElt t b)
    => Reduce (UCyc t m P a) (UCyc t m P b) where
  reduce (Pow v) = Pow $ fmapT reduce v

instance (Reduce a b, Tensor t, Fact m, TElt t a, TElt t b)
    => Reduce (UCyc t m D a) (UCyc t m D b) where
  reduce (Dec v) = Dec $ fmapT reduce v

{-
  reduce (Scalar c) = Scalar $ reduce c
  reduce (Sub (c :: UCyc t l a)) = Sub (reduce c :: UCyc t l b)
-}


type instance LiftOf (UCyc t m P r) = UCyc t m P (LiftOf r)
type instance LiftOf (UCyc t m D r) = UCyc t m D (LiftOf r)

instance (Lift' r, Tensor t, Fact m, TElt t r, TElt t (LiftOf r))
         => Lift' (UCyc t m P r) where
  lift (Pow v) = Pow $ fmapT lift v

instance (Lift' r, Tensor t, Fact m, TElt t r, TElt t (LiftOf r))
         => Lift' (UCyc t m D r) where
  lift (Dec v) = Dec $ fmapT lift v


instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m P a) (UCyc t m P b) where
  rescale (Pow v) = Pow $ fmapT rescale v

instance (Rescale a b, Tensor t, Fact m, TElt t a, TElt t b)
         => Rescale (UCyc t m D a) (UCyc t m D b) where
  rescale (Dec v) = Dec $ fmapT rescale v




{-

CJP: these may need to get moved to Cyc, because they have different
output representations.

instance (Mod a, Field b, Lift a (ModRep a), Reduce (LiftOf a) b,
          Tensor t, Fact m)
         => RescaleCyc (UCyc t m C a) (UCyc t m C b) where

  rescalePow = let aval = proxy modulus (Proxy::Proxy a)
               in \x -> let (a,b) = unzipUCyc x
                            z = liftCyc bas a
    in Scalar (recip (reduce aval)) * (b - reduce z)

-- generic RescaleCyc instance

instance {-# OVERLAPS #-} (Rescale a b, CElt t a, CElt t b)
         => RescaleCyc (UCyc t) a b where

  -- Optimized for subring constructors, for powerful basis.
  -- Analogs for decoding basis are not quite correct, because (* -1)
  -- doesn't commute with 'rescale' due to tiebreakers!
  rescaleCyc U.Pow (Scalar c) = Scalar $ rescale c
  rescaleCyc U.Pow (Sub (c :: UCyc t l a)) =
    Sub (U.rescaleCyc U.Pow c :: UCyc t l b)

  rescaleCyc b x = fmapC rescale $ forceBasis (Just b) x

-- specialized instance for product rings: ~2x faster algorithm
instance (Mod a, Field b, Lift a (ModRep a), Reduce (LiftOf a) b,
          CElt t a, CElt t b, CElt t (a,b), CElt t (LiftOf a))
         => RescaleCyc (UCyc t) (a,b) b where

  -- optimized for subrings and powerful basis (see comments in other
  -- instance for why this doesn't work for decoding basis)
  rescaleCyc U.Pow (Scalar c) = Scalar $ rescale c
  rescaleCyc U.Pow (Sub (c :: UCyc t l (a,b))) =
    Sub (U.rescaleCyc U.Pow c :: UCyc t l b)

  rescaleCyc bas x =
    let aval = proxy modulus (Proxy::Proxy a)
        y = forceAny x
        (a,b) = unzipUCyc y
        z = liftCyc bas a
    in Scalar (recip (reduce aval)) * (b - reduce z)

-}


{-

CJP: not clear that we want to instantiate Gadget and Decompose here,
because they have superclasses that would necessarily be inefficient, or
possibly not even well-defined (e.g., Ring for P rep).

-- promote Gadget from base ring
instance (Gadget gad zq, Fact m, CElt t zq) => Gadget gad (UCyc t m zq) where
  gadget = (scalarCyc <$>) <$> gadget
  -- specialization of 'encode', done efficiently (via 'adviseCRT').
  encode s = ((* adviseCRT s) <$>) <$> gadget

-- promote Decompose, using the powerful basis
instance (Decompose gad zq, Tensor t, Fact m) => Decompose gad (UCyc t m P zq) where

  type DecompOf (UCyc t m P zq) = UCyc t m P (DecompOf zq)

  -- traverse: Traversable (UCyc t m P) and Applicative (Tagged gad ZL)
  decompose x = fromZL $ traverse (toZL . decompose) x
  {-# INLINABLE decompose #-}

-- promote Correct, using the decoding basis
instance (Correct gad zq, Tensor t, Fact m) => Correct gad (UCyc t m D zq) where
  -- sequence: Monad [] and Traversable (UCyc t m D)
  -- sequenceA: Applicative (UCyc t m) and Traversable (TaggedT gad [])
  correct bs = second sequence $ unzipUCyc $ (correct . pasteT) <$>
               sequenceA (peelT bs)
  {-# INLINABLE correct #-}

instance (Decompose gad zq, Fact m,
         -- these imply (superclass) Reduce on UCyc; needed for Sub case
          CElt t zq, CElt t (DecompOf zq), Reduce (DecompOf zq) zq)
  => Decompose gad (UCyc t m zq) where

  type DecompOf (UCyc t m zq) = UCyc t m (DecompOf zq)

  -- faster implementations: decompose directly in subring, which is
  -- correct because we decompose in powerful basis
  decompose (Scalar c) = pasteT $ Scalar <$> peelT (decompose c)
  decompose (Sub c) = pasteT $ Sub <$> peelT (decompose c)

toZL :: Tagged s [a] -> TaggedT s ZipList a
toZL = coerce

fromZL :: TaggedT s ZipList a -> Tagged s [a]
fromZL = coerce
-}



unzipUCyc :: (Tensor t, Fact m)
            => UCyc t m rep (a,b) -> (UCyc t m rep a, UCyc t m rep b)
unzipUCyc (Pow v) = Pow *** Pow $ unzipT v
unzipUCyc (Dec v) = Dec *** Dec $ unzipT v
unzipUCyc (CRTr v) = CRTr *** CRTr $ unzipT v
unzipUCyc (CRTe v) = CRTe *** CRTe $ unzipT v





{-
-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.liftCyc', but for 'UCyc'.
liftCyc :: (Lift b a, Fact m, CElt t a, CElt t b)
           => U.Basis -> UCyc t m b -> UCyc t m a
-- optimized for subrings and powerful basis (see comments in
-- RescaleCyc instances for why this doesn't work for decoding)
liftCyc U.Pow (Scalar c) = Scalar $ lift c
liftCyc U.Pow (Sub c) = Sub $ liftCyc U.Pow c

-}

{-
-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.adviseCRT', but for 'UCyc'.
adviseCRT :: (Fact m, CElt t r) => UCyc t m r -> UCyc t m r
adviseCRT x@(Scalar _) = x
adviseCRT (Sub c) = Sub $ adviseCRT c
-}

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.mulG', but for 'UCyc'.
mulG :: (Tensor t, Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m rep r
-- CJP: should there be individual functions for each P, D, C?
-- They would have different constraints and names.
mulG (Pow v) = Pow $ mulGPow v
mulG (Dec v) = Dec $ mulGDec v
-- fromJust is safe here because we're already in CRTr
mulG (CRTr v) = CRTr $ fromJust' "UCyc.mulG CRTr" mulGCRT v
mulG (CRTe v) = CRTe $ fromJust' "UCyc.mulG CRTe" mulGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.divG', but for 'UCyc'.
divG :: (Tensor t, Fact m, UCElt t r) => UCyc t m rep r -> Maybe (UCyc t m rep r)
-- CJP: same question as for mulG
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
-- fromJust is OK here because we're already in CRTr
divG (CRTr v) = Just $ CRTr $ fromJust' "UCyc.divG CRTr" divGCRT v
divG (CRTe v) = Just $ CRTe $ fromJust' "UCyc.divG CRTe" divGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.tGaussian', but for 'UCyc'.
tGaussian :: (Tensor t, Fact m, OrdFloat q, Random q, TElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (UCyc t m D q)
tGaussian = liftM Dec . tGaussianDec

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.gSqNorm', but for 'UCyc'.
gSqNorm :: (Ring r, Tensor t, Fact m, TElt t r) => UCyc t m D r -> r
gSqNorm (Dec v) = gSqNormDec v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.errorRounded', but for 'UCyc'.
errorRounded :: forall v rnd t m z .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd)
                => v -> rnd (UCyc t m D z)
errorRounded svar =
  Dec . fmapT (roundMult one) <$> (tGaussianDec svar :: rnd (t m Double))

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.errorCoset', but for 'UCyc'.
errorCoset :: forall t m zp z v rnd .
  (Mod zp, z ~ ModRep zp, Lift zp z, Tensor t, Fact m,
   TElt t zp, TElt t z, ToRational v, MonadRandom rnd)
  => v -> UCyc t m D zp -> rnd (UCyc t m D z)
errorCoset =
  let pval = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  in \ svar c ->
    roundCosetCyc c <$> (tGaussian (svar*pval*pval) :: rnd (UCyc t m D Double))

-- | Deterministically round to the given coset @c+pR@, using the
-- decoding basis.
roundCosetCyc ::
    (Mod zp, z ~ ModRep zp, Lift zp z, RealField q,
     Tensor t, Fact m, TElt t q, TElt t zp, TElt t z)
    => UCyc t m D zp -> UCyc t m D q -> UCyc t m D z
roundCosetCyc c x = roundCoset <$> c <*> x

----- inter-ring operations

-- | Embed into an extension ring, for the powerful basis.
embedPow :: (Additive r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m P r -> UCyc t m' P r
embedPow (Pow v) = Pow $ T.embedPow v

-- | Embed into an extension ring, for the decoding basis.
embedDec :: (Additive r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m D r -> UCyc t m' D r
embedDec (Dec v) = Dec $ T.embedDec v

-- | Embed into an extension ring, for a CRT basis.  The output is
-- an 'Either' because in some cases it is most efficient to preserve
-- the 'UCyc' internal invariant by producing output in the powerful
-- basis.
embedCRT :: forall t m m' r . (m `Divides` m', UCElt t r)
            => UCyc t m C r -> Either (UCyc t m' P r) (UCyc t m' C r)
embedCRT x@(CRTr v) = fromMaybe (Left $ embedPow $ toPow x)
                      (Right . CRTr <$> (T.embedCRT <*> pure v))
embedCRT x@(CRTe v) =
    -- preserve invariant: CRTe iff CRTr is invalid for m'
    fromMaybe (Right $ CRTe $ fromJust' "UCyc.embedCRT CRTe" T.embedCRT v)
              (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) *>
               pure (Left $ embedPow $ toPow x))


{-
embed (Scalar c) = Scalar c
embed (Sub (c :: UCyc t l r)) = Sub c
  \\ transDivides (Proxy::Proxy l) (Proxy::Proxy m) (Proxy::Proxy m')
-}

twacePow :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m' P r -> UCyc t m P r
twacePow (Pow v) = Pow $ twacePowDec v

twaceDec :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => UCyc t m' D r -> UCyc t m D r
twaceDec (Dec v) = Dec $ twacePowDec v

twaceCRT :: forall t m m' r . (m `Divides` m', UCElt t r)
            => UCyc t m' C r -> Either (UCyc t m P r) (UCyc t m C r)
twaceCRT x@(CRTr v) =
  -- stay in CRTr only iff it's valid for target, else go to Pow
  fromMaybe (Left $ twacePow $ toPow x) (Right . CRTr <$> (T.twaceCRT <*> pure v))
twaceCRT x@(CRTe v) =
  -- stay in CRTe iff CRTr is invalid for target, else go to Pow
  fromMaybe (Right $ CRTe $ fromJust' "UCyc.twace CRTe" T.twaceCRT v)
            (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) *>
             Just (Left $ twacePow $ toPow x))

{-
twace (Scalar c) = Scalar c
-- twace on Sub goes to the largest common subring of O_l and O_m
twace (Sub (c :: UCyc t l r)) =
  Sub (twace c :: UCyc t (FGCD l m) r)
  \\ gcdDivides (Proxy::Proxy l) (Proxy::Proxy m)
-}

coeffsPow :: (Ring r, Tensor t, m `Divides` m', TElt t r)
             => UCyc t m' P r -> [UCyc t m P r]
{-# INLINABLE coeffsPow #-}
coeffsPow (Pow v) = LP.map Pow $ coeffs v

coeffsDec :: (Ring r, Tensor t, m `Divides` m', TElt t r)
             => UCyc t m' D r -> [UCyc t m D r]
{-# INLINABLE coeffsDec #-}
coeffsDec (Dec v) = LP.map Dec $ coeffs v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.powBasis', but for 'UCyc'.
powBasis :: (Ring r, Tensor t, m `Divides` m', TElt t r)
            => Tagged m [UCyc t m' P r]
{-# INLINABLE powBasis #-}
powBasis = map Pow <$> powBasisPow

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.crtSet', but for 'UCyc'.
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


toPow :: (Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v
toPow (CRTr v) = Pow $ fromJust' "UCyc.toPow CRTr" crtInv v
toPow (CRTe v) =
    Pow $ fmapT fromExt $ fromJust' "UCyc.toPow CRTe" crtInv v

toDec :: (Fact m, UCElt t r) => UCyc t m rep r -> UCyc t m D r
{-# INLINABLE toDec #-}
toDec (Pow v) = Dec $ lInv v
toDec x@(Dec _) = x
toDec x@(CRTr _) = toDec $ toPow x
toDec x@(CRTe _) = toDec $ toPow x

toCRT :: forall t m rep r . (Fact m, UCElt t r)
         => UCyc t m rep r -> UCyc t m C r
{-# INLINABLE toCRT #-}
toCRT = let crte = CRTe . fromJust' "UCyc.toCRT: no crt for Ext" crt
            crtr = liftM (CRTr .) crt
            fromPow :: t m r -> UCyc t m C r
            fromPow v = fromMaybe (crte $ fmapT toExt v) (crtr <*> Just v)
        in \x -> case x of
                   (CRTr _) -> x
                   (CRTe _) -> x
                   (Pow v) -> fromPow v
                   (Dec v) -> fromPow $ l v

{-
-- | Force a cyclotomic element into the powerful basis.
forcePow (Scalar c) = Pow $ scalarPow c
forcePow (Sub c) = embed' $ forcePow c -- OK: embed' preserves Pow

-- | Force a cyclotomic element into the decoding basis.
forceDec x@(Scalar _) = forceDec $ forcePow x -- TODO: use scalarDec instead
forceDec (Sub c) = embed' $ forceDec c      -- OK: embed' preserves Dec

toCRT' (Sub (c :: UCyc t l r)) =
    case (proxyT hasCRTFuncs (Proxy::Proxy (t l r)),
          proxyT hasCRTFuncs (Proxy::Proxy (t m r))) of
      (Just _, Just _) -> embed' $ toCRT' c -- fastest; embed' preserves CRTr
      (_, Nothing) -> embed' $ toCRTe c -- faster; temp violate CRTr/e invariant
      _ -> toCRT' $ embed' c            -- fallback
-}

---------- Category-theoretic instances ----------

-- CJP: no Applicative, Foldable, Traversable for C because types
-- (and math) don't work out for the CRTe case.

instance (Applicative (UCyc t m rep)) => Functor (UCyc t m rep) where
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
  show (Pow  v) = "UCyc Pow "  ++ show v \\ witness entailShowT v
  show (Dec  v) = "UCyc Dec "  ++ show v \\ witness entailShowT v
  show (CRTr v) = "UCyc CRTr " ++ show v \\ witness entailShowT v
  show (CRTe v) = "UCyc CRTe " ++ show v \\ witness entailShowT v

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m P r) where
  arbitrary = liftM Pow arbitrary
  shrink = shrinkNothing

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m D r) where
  arbitrary = liftM Dec arbitrary
  shrink = shrinkNothing

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m C r) where
  arbitrary = liftM CRTr arbitrary
  shrink = shrinkNothing

instance (Tensor t, Fact m, NFData r, TElt t r,
          NFData (CRTExt r), TElt t (CRTExt r))
         => NFData (UCyc t m rep r) where
  rnf (Pow x)    = rnf x \\ witness entailNFDataT x
  rnf (Dec x)    = rnf x \\ witness entailNFDataT x
  rnf (CRTr x)   = rnf x \\ witness entailNFDataT x
  rnf (CRTe x)   = rnf x \\ witness entailNFDataT x
