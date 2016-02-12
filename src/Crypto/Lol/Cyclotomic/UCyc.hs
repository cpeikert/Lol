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
  UCyc, CElt, RElt
-- * Basic operations
, mulG, divG
, scalarCyc, liftCyc
, adviseCRT
-- * Error sampling and norm
, tGaussian, errorRounded, errorCoset, gSqNorm
-- * Sub/extension rings
, embed, twace, coeffsCyc, powBasis, crtSet
-- * Representations
, forceBasis, forcePow, forceDec, forceAny
-- * Specialized maps
, fmapC, fmapCM
, U.Basis(..), U.RescaleCyc
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor   as T
import Crypto.Lol.Cyclotomic.Utility
import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude      as LP hiding ((*>))
import Crypto.Lol.Types.ZPP

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.Random
import Data.Coerce
import Data.Foldable          as F
import Data.Maybe
import Data.Traversable
import Test.QuickCheck

--import qualified Debug.Trace as DT

-- | Nullary type representing powerful basis.
data P
-- | Nullary type representing decoding basis.
data D
-- | Nullary type representing CRT basis.
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

-- | Collection of constraints needed for most functions over a
-- particular base ring @r@.
type RElt t r = (TElt t r, CRTrans r, IntegralDomain r, ZeroTestable r, NFData r)

type CElt t r = (Tensor t, RElt t r, RElt t (CRTExt r), CRTEmbed r, Eq r, Random r)

{-
-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.scalarCyc', but for 'UCyc'.
scalarCyc :: (Fact m, CElt t a) => a -> UCyc t m a
scalarCyc = Scalar
-}

-- Eq instances

instance (Eq r, Fact m, TElt t r) => Eq (UCyc t m P r) where
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailEqT v1

instance (Eq r, Fact m, TElt t r) => Eq (UCyc t m D r) where
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

instance (ZeroTestable r, Fact m, TElt t r) => ZeroTestable.C (UCyc t m P r) where
  isZero (Pow v) = isZero v \\ witness entailZTT v

instance (ZeroTestable r, Fact m, TElt t r) => ZeroTestable.C (UCyc t m D r) where
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

crtCons :: forall t m r . (Tensor t, Fact m, TElt t r, 
                           ZeroTestable r, IntegralDomain r, CRTrans r)
           => t m r -> UCyc t m C r
crtCons = fromMaybe CRTe (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) >> Just CRTr)

instance (Ring r, Fact m, TElt t r) => Additive.C (UCyc t m P r) where
  zero = let v = scalarPow zero in Pow v \\ witness entailRingT v
  (Pow v1) + (Pow v2) = Pow (v1+v2) \\ witness entailRingT v1
  (Pow v1) - (Pow v2) = Pow (v1-v2) \\ witness entailRingT v1
  negate (Pow v) = Pow $ negate v \\ witness entailRingT v

instance (Ring r, Fact m, TElt t r) => Additive.C (UCyc t m D r) where
  zero = let v = scalarDec zero in Dec v \\ witness entailRingT v
  (Dec v1) + (Dec v2) = Dec (v1+v2) \\ witness entailRingT v1
  (Dec v1) - (Dec v2) = Dec (v1-v2) \\ witness entailRingT v1
  negate (Dec v) = Dec $ negate v \\ witness entailRingT v

instance (Ring r, Fact m, TElt t r) => Additive.C (UCyc t m C r) where
  zero = let v = scalarCRT zero in crtCons v \\ witness entailRingT v
  (CRTr v1) + (CRTr v2) = CRTr (v1+v2) \\ witness entailRingT v1
  (CRTr v1) - (CRTr v2) = CRTr (v1-v2) \\ witness entailRingT v1
  negate (CRTr v) = CRTr $ negate v \\ witness entailRingT v

-- CJP: precision OK?
instance (Ring r, Fact m, TElt t r) => Additive.C (UCyc t m CE r) where
  -- CJP: this 'zero' works; we don't use scalarCRT because it's a Maybe
  zero = let v = scalarPow zero in CRTe v \\ witness entailRingT v
  (CRTe v1) + (CRTe v2) = CRTe (v1+v2) \\ witness entailRingT v1
  (CRTe v1) - (CRTe v2) = CRTe (v1-v2) \\ witness entailRingT v1
  negate (CRTe v) = CRTe $ negate v \\ witness entailRingT v

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

-- Ring instances

-- CJP: this instance is meaningful only if a CRTr basis actually
-- exists, but that is only known at compile time
instance (Ring r, Fact m, TElt t r) => Ring.C (UCyc t m C r) where
  one = let v = fromJust' "UCyc C one: no scalarCRT" scalarCRT one
        in CRTr v

  (CRTr v1) * (CRTr v2) = CRTr (v1*v2) \\ witness entailRingT v1

  fromInteger c =
    let v = fromJust' "UCyc C fromInteger: no scalarCRT" scalarCRT $
            fromInteger c
    in CRTr v

instance (Ring r, Fact m, TElt t r) => Ring.C (UCyc t m CE r) where
  one = let v = fromJust' "UCyc C one: no scalarCRT" scalarCRT one
        in CRTe v

  (CRTe v1) * (CRTe v2) = CRTe (v1*v2) \\ witness entailRingT v1

  fromInteger c =
    let v = fromJust' "UCyc C fromInteger: no scalarCRT" scalarCRT $
            fromInteger c
    in CRTe v


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


{-
-- reduces in any basis
instance (Reduce a b, Fact m, CElt t a, CElt t b)
  => Reduce (UCyc t m a) (UCyc t m b) where

  -- optimized for subring constructors
  reduce (Scalar c) = Scalar $ reduce c
  reduce (Sub (c :: UCyc t l a)) = Sub (reduce c :: UCyc t l b)
  reduce x = fmapC reduce $ forceAny x

-- promote Gadget from base ring
instance (Gadget gad zq, Fact m, CElt t zq) => Gadget gad (UCyc t m zq) where
  gadget = (scalarCyc <$>) <$> gadget
  -- specialization of 'encode', done efficiently (via 'adviseCRT').
  encode s = ((* adviseCRT s) <$>) <$> gadget
-}


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
  correct bs = second sequence $ unzipCyc $ (correct . pasteT) <$>
               sequenceA (peelT bs)
  {-# INLINABLE correct #-}


{-
instance (Decompose gad zq, Fact m,
         -- these imply (superclass) Reduce on UCyc; needed for Sub case
          CElt t zq, CElt t (DecompOf zq), Reduce (DecompOf zq) zq)
  => Decompose gad (UCyc t m zq) where

  type DecompOf (UCyc t m zq) = UCyc t m (DecompOf zq)

  -- faster implementations: decompose directly in subring, which is
  -- correct because we decompose in powerful basis
  decompose (Scalar c) = pasteT $ Scalar <$> peelT (decompose c)
  decompose (Sub c) = pasteT $ Sub <$> peelT (decompose c)
-}

toZL :: Tagged s [a] -> TaggedT s ZipList a
toZL = coerce

fromZL :: TaggedT s ZipList a -> Tagged s [a]
fromZL = coerce


instance (Rescale a b, Tensor t, Fact m, RElt t a, RElt t b)
         => Rescale (UCyc t m P a) (UCyc t m P b) where
  rescale (Pow v) = Pow $ fmapR rescale v

instance (Rescale a b, Tensor t, Fact m, RElt t a, RElt t b)
         => Rescale (UCyc t m D a) (UCyc t m D b) where
  rescale (Dec v) = Dec $ fmapR rescale v



instance (Mod a, Field b, Lift a (ModRep a), Reduce (LiftOf a) b,
          Tensor t, Fact m)
         => RescaleCyc (UCyc t m C a) (UCyc t m C b) where

  rescalePow = let aval = proxy modulus (Proxy::Proxy a)
               in \x -> let (a,b) = unzipCyc x
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
        (a,b) = unzipCyc y
        z = liftCyc bas a
    in Scalar (recip (reduce aval)) * (b - reduce z)

unzipCyc :: (Tensor t, Fact m)
            => UCyc t m rep (a,b) -> (UCyc t m rep a, UCyc t m rep b)
unzipCyc (Pow v) = Pow *** Pow $ unzipT v
unzipCyc (Dec v) = Dec *** Dec $ unzipT v
unzipCyc (CRTr v) = CRTr *** CRTr $ unzipT v
unzipCyc (CRTe v) = CRTe *** CRTe $ unzipT v


type instance LiftOf (UCyc t m P r) = UCyc t m P (LiftOf r)
type instance LiftOf (UCyc t m D r) = UCyc t m D (LiftOf r)
type instance LiftOf (UCyc t m C r) = UCyc t m C (LiftOf r)

instance (Lift r, Tensor t, Fact m, RElt (t m) r, RElt (t m) (LiftOf r))
         => Lift (UCyc t m P r) where
  lift (Pow v) = Pow $ fmapR lift v

{-
-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.liftCyc', but for 'UCyc'.
liftCyc :: (Lift b a, Fact m, CElt t a, CElt t b)
           => U.Basis -> UCyc t m b -> UCyc t m a
-- optimized for subrings and powerful basis (see comments in
-- RescaleCyc instances for why this doesn't work for decoding)
liftCyc U.Pow (Scalar c) = Scalar $ lift c
liftCyc U.Pow (Sub c) = Sub $ liftCyc U.Pow c

liftCyc U.Pow x = fmapC lift $ forceBasis (Just U.Pow) x
liftCyc U.Dec x = fmapC lift $ forceBasis (Just U.Dec) x
-}

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.adviseCRT', but for 'UCyc'.
adviseCRT :: (Fact m, CElt t r) => UCyc t m r -> UCyc t m r
adviseCRT x@(Scalar _) = x
adviseCRT (Sub c) = Sub $ adviseCRT c
adviseCRT x = toCRT' x

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.mulG', but for 'UCyc'.
mulG :: (Fact m, CElt t r) => UCyc t m r -> UCyc t m r
mulG (Scalar c) = Pow $ mulGPow $ scalarPow c -- must go to full ring
mulG (Sub c) = mulG $ embed' c                -- must go to full ring
mulG (Pow v) = Pow $ mulGPow v
mulG (Dec v) = Dec $ mulGDec v
-- fromMaybe is safe here because we're already in CRTr
mulG (CRTr v) = CRTr $ fromJust' "UCyc.mulG CRTr" mulGCRT v
mulG (CRTe v) = CRTe $ fromJust' "UCyc.mulG CRTe" mulGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.divG', but for 'UCyc'.
divG :: (Fact m, CElt t r) => UCyc t m r -> Maybe (UCyc t m r)
divG (Scalar c) = liftM Pow (divGPow $ scalarPow c) -- full ring
divG (Sub c) = divG $ embed' c                      -- full ring
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
-- fromMaybe is safe here because we're already in CRTr
divG (CRTr v) = Just $ CRTr $ fromJust' "UCyc.divG CRTr" divGCRT v
divG (CRTe v) = Just $ CRTe $ fromJust' "UCyc.divG CRTe" divGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.tGaussian', but for 'UCyc'.
tGaussian :: (Fact m, OrdFloat q, Random q, CElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (UCyc t m q)
tGaussian = liftM Dec . tGaussianDec

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.gSqNorm', but for 'UCyc'.
gSqNorm :: (Fact m, CElt t r) => UCyc t m r -> r
gSqNorm (Dec v) = gSqNormDec v
gSqNorm c = gSqNorm $ forceDec c

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.errorRounded', but for 'UCyc'.
errorRounded :: forall v rnd t m z .
                (ToInteger z, Fact m, CElt t z, ToRational v, MonadRandom rnd)
                => v -> rnd (UCyc t m z)
errorRounded svar =
  fmapC (roundMult one) <$> (tGaussian svar :: rnd (UCyc t m Double))

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.errorCoset', but for 'UCyc'.
errorCoset :: forall t m zp z v rnd .
  (Mod zp, z ~ ModRep zp, Lift zp z, Fact m,
   CElt t zp, CElt t z, ToRational v, MonadRandom rnd)
  => v -> UCyc t m zp -> rnd (UCyc t m z)
errorCoset =
  let pval = fromIntegral $ proxy modulus (Proxy::Proxy zp)
  -- we don't force* here because tGaussian is always in Dec
  in \ svar c ->
    roundCosetDec c <$> (tGaussian (svar * pval * pval) :: rnd (UCyc t m Double))

-- | Deterministically round to the given coset @c+pR@, using the
-- decoding basis.
roundCosetDec ::
    (Mod zp, z ~ ModRep zp, Lift zp z, RealField q,
     Fact m, CElt t q, CElt t zp, CElt t z)
    => UCyc t m zp -> UCyc t m q -> UCyc t m z
roundCosetDec c x = roundCoset <$> forceDec c <*> forceDec x

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.embed', but for 'UCyc'.
embed :: forall t r m m' . (m `Divides` m') => UCyc t m r -> UCyc t m' r
embed (Scalar c) = Scalar c
embed (Sub (c :: UCyc t l r)) = Sub c
  \\ transDivides (Proxy::Proxy l) (Proxy::Proxy m) (Proxy::Proxy m')
embed c = Sub c

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.twace', but for 'UCyc'.
twace :: forall t r m m' . (CElt t r, m `Divides` m')
         => UCyc t m' r -> UCyc t m r
twace (Scalar c) = Scalar c
-- twace on Sub goes to the largest common subring of O_l and O_m
twace (Sub (c :: UCyc t l r)) =
  Sub (twace c :: UCyc t (FGCD l m) r)
  \\ gcdDivides (Proxy::Proxy l) (Proxy::Proxy m)
twace (Pow v) = Pow $ twacePowDec v
twace (Dec v) = Dec $ twacePowDec v
-- stay in CRTr only iff it's valid for target, else go to Pow
twace x@(CRTr v) =
  fromMaybe (twace $ forcePow x) (CRTr <$> (twaceCRT <*> pure v))
-- stay in CRTe iff CRTr is invalid for target, else go to Pow
twace x@(CRTe v) =
  fromMaybe (CRTe $ fromJust' "UCyc.twace CRTe" twaceCRT v)
            (proxy (pasteT hasCRTFuncs) (Proxy::Proxy (t m r)) *>
             pure (twace $ forcePow x))

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.coeffsCyc', but for 'UCyc'.
coeffsCyc :: (m `Divides` m', CElt t r)
             => U.Basis -> UCyc t m' r -> [UCyc t m r]
coeffsCyc U.Pow (Pow v) = LP.map Pow $ coeffs v
coeffsCyc U.Dec (Dec v) = LP.map Dec $ coeffs v
coeffsCyc U.Pow x = coeffsCyc U.Pow $ forcePow x
coeffsCyc U.Dec x = coeffsCyc U.Dec $ forceDec x

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.powBasis', but for 'UCyc'.
powBasis :: (m `Divides` m', CElt t r) => Tagged m [UCyc t m' r]
powBasis = map Pow <$> powBasisPow

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.crtSet', but for 'UCyc'.
crtSet :: forall t m m' r p mbar m'bar .
           (m `Divides` m', ZPP r, p ~ CharOf (ZpOf r),
            mbar ~ PFree p m, m'bar ~ PFree p m',
            CElt t r, CElt t (ZpOf r))
           => Tagged m [UCyc t m' r]
crtSet =
  -- CJP: consider using traceEvent or traceMarker
  --DT.trace ("UCyc.fcCrtSet: m = " ++
  --          show (proxy valueFact (Proxy::Proxy m)) ++ ", m'= " ++
  --          show (proxy valueFact (Proxy::Proxy m'))) $
  let (p,e) = proxy modulusZPP (Proxy::Proxy r)
      pp = Proxy::Proxy p
      pm = Proxy::Proxy m
      pm' = Proxy::Proxy m'
  in retag (fmap (embed . (^(p^(e-1))) . Dec . fmapT liftZp) <$>
            (crtSetDec :: Tagged mbar [t m'bar (ZpOf r)]))
     \\ pFreeDivides pp pm pm'
     \\ pSplitTheorems pp pm \\ pSplitTheorems pp pm'

----- "Unsafe" functions that expose or rely upon internal representation

-- | Yield an equivalent element whose internal representation /must/
-- be in the indicated basis: powerful or decoding (for 'Just' 'Pow'
-- and 'Just' 'Dec' arguments, respectively), or any @r@-basis of the
-- implementation's choice (for 'Nothing' argument).  (See also the
-- convenient specializations 'forcePow', 'forceDec', 'forceAny'.)
forceBasis :: (Fact m, CElt t r) => Maybe U.Basis -> UCyc t m r -> UCyc t m r
{-# INLINABLE forceBasis #-}
forceBasis (Just U.Pow) x = forcePow x
forceBasis (Just U.Dec) x = forceDec x
forceBasis Nothing x@(Scalar _) = forcePow x
-- force as outermost op to ensure we get an r-basis
forceBasis Nothing (Sub c) = forceBasis Nothing $ embed' c
forceBasis Nothing x@(CRTe _) = forcePow x
forceBasis Nothing x = x

-- | Monadic version of 'fmapC'.
fmapCM :: (Fact m, CElt t a, CElt t b, Monad mon)
  => (a -> mon b) -> UCyc t m a -> mon (UCyc t m b)
{-# INLINABLE fmapCM #-}

-- must embed into full ring
fmapCM _ (Scalar _) = error "can't fmapCM on Scalar. Must forceBasis first!"
fmapCM _ (Sub _) = error "can't fmapCM on Sub. Must forceBasis first!"
fmapCM _ (CRTe _) =  error "can't fmapCM on CRTe.  Must forceBasis first!"

fmapCM f (Pow v) = liftM Pow $ fmapTM f v
fmapCM f (Dec v) = liftM Dec $ fmapTM f v
fmapCM f (CRTr v) = liftM CRTr $ fmapTM f v




---------- HELPER FUNCTIONS (NOT FOR EXPORT) ----------

-- | Force embed, to a non-'Sub' constructor.  Preserves Scalar, Pow,
-- Dec constructors, and CRTr/CRTe constructor if valid in both source
-- and target ring (we rely on this in toCRT').
embed' :: forall t r l m .
          (CElt t r, l `Divides` m) => UCyc t l r -> UCyc t m r
embed' (Scalar x) = Scalar x    -- must re-wrap!
embed' (Pow v) = Pow $ embedPow v
embed' (Dec v) = Dec $ embedDec v
-- stay in CRTr only if it's possible, otherwise go to Pow
embed' x@(CRTr v) =
    fromMaybe (embed' $ forcePow x) (CRTr <$> (embedCRT <*> pure v))
embed' x@(CRTe v) = -- go to CRTe iff CRTr is invalid for target index
    fromMaybe (CRTe $ fromJust' "UCyc.embed' CRTe" embedCRT v)
              (proxy (pasteT hasCRTFuncs) (Proxy::Proxy (t m r)) *>
               pure (embed' $ forcePow x))
embed' (Sub (c :: UCyc t k r)) = embed' c
  \\ transDivides (Proxy::Proxy k) (Proxy::Proxy l) (Proxy::Proxy m)


--------- Conversion methods ------------------

embed :: UCyc t l rep r -> UCyc t m rep r
embed (Pow v) = Pow $ embedPow v
embed (Dec v) = Dec $ embedDec v
-- what to do about CRTr?  Do we need separate functions?

toPow :: (Tensor t, Fact m, TElt t r) => UCyc t m rep r -> UCyc t m P r
{-# INLINABLE toPow #-}
toPow x@(Pow _) = x
toPow (Dec v) = Pow $ l v
toPow (CRTr v) = Pow $ fromJust' "UCyc.toPow CRTr" crtInv v
toPow (CRTe v) =
    Pow $ fmapT fromExt $ fromJust' "UCyc.toPow CRTe" crtInv v

toDec :: UCyc t m rep r -> UCyc t m D r
{-# INLINABLE toDec #-}
toDec (Pow v) = Dec $ lInv v
toDec x@(Dec _) = x
toDec x@(CRTr _) = toDec $ toPow x
toDec x@(CRTe _) = toDec $ toPow x

toCRT :: UCyc t m rep r -> Either (UCyc t m C r) (UCyc t m CE r)
{-# INLINABLE toCRT #-}
toCRT x@(CRTr _) = Left x
toCRT x@(CRTe _) = Right x
toCRT x = fromMaybe (toCRTe x) (toCRTr <*> pure x)

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

toCRTr :: (CElt t r, Fact m) => Maybe (UCyc t m rep r -> UCyc t m C r)
{-# INLINABLE toCRTr #-}
toCRTr = do -- Maybe monad
  crt' <- crt
  return (\x -> case x of
           (Pow v) -> CRTr $ crt' v
           (Dec v) -> CRTr $ crt' $ l v
           c@(CRTr _) -> c
           c@(CRTe _) -> fromJust' "UCyc.toCRTr CRTe" toCRTr $ toPow c)

toCRTe :: forall t m r . (CElt t r, Fact m) => UCyc t m rep r -> UCyc t m CE r
{-# INLINABLE toCRTe #-}
toCRTe = let m = proxy valueFact (Proxy::Proxy m)
             crt' = fromJust' ("UCyc.toCRTe: no crt: " ++ show m) crt
                    :: t m (CRTExt r) -> t m (CRTExt r) -- must exist
         in \x -> case x of
                    (Pow v) -> CRTe $ crt' $ fmapT toExt v
                    (Dec v) -> CRTe $ crt' $ fmapT toExt $ l v
                    c@(CRTr _) -> toCRTe $ toPow c
                    c@(CRTe _) -> c

---------- Category-theoretic instances ----------

-- CJP: no Applicative, Foldable, Traversable for CE because types
-- (and math) don't work out

instance (Applicative (UCyc t m rep)) => Functor (UCyc t m rep) where
  -- Functor instance is implied by Applicative laws
  {-# INLINABLE fmap #-}
  fmap f x = pure f <*> x

instance (Tensor t, Fact m) => RFunctor (UCyc t m P) where
  type RElt (UCyc t m P) a = RElt (t m) a

  {-# INLINABLE fmapR #-}
  fmapR f (Pow v) = Pow $ fmapT f v

instance (Tensor t, Fact m) => RFunctor (UCyc t m D) where
  type RElt (UCyc t m D) a = RElt (t m) a

  {-# INLINABLE fmapR #-}
  fmapR f (Dec v) = Dec $ fmapT f v

instance (Tensor t, Fact m) => RFunctor (UCyc t m C) where
  type RElt (UCyc t m C) a = RElt (t m) a

  {-# INLINABLE fmapR #-}
  fmapR f (CRTr v) = CRTr $ fmapT f v


instance (Tensor t, Fact m) => Applicative (UCyc t m P) where
  {-# INLINABLE pure #-}
  pure = Pow . scalarPow

  {-# INLINABLE (<*>) #-}
  (Pow f) <*> (Pow v) = Pow $ f <*> v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Applicative (UCyc t m D) where
  {-# INLINABLE pure #-}
  pure = Dec . scalarDec

  {-# INLINABLE (<*>) #-}
  (Dec f) <*> (Dec v) = Dec $ f <*> v \\ witness entailIndexT v

instance (Tensor t, Fact m) => Applicative (UCyc t m C) where
  {-# INLINABLE pure #-}
  pure = CRTr . fromJust' "UCyc C: Applicative: no scalarCRT" scalarCRT

  {-# INLINABLE (<*>) #-}
  (CRTr f) <*> (CRTr v) = CRTr $ f <*> v \\ witness entailIndexT v


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

instance (Tensor t, Fact m, TElt t r,
          CRTrans r, Random r, ZeroTestable r, IntegralDomain r)
         => Random (Either (UCyc t m C r) (UCyc t m P r)) where

  -- create in CRTr basis if possible, otherwise in powerful
  random = let cons = fromMaybe Pow
                      (proxyT hasCRTFuncs (Proxy::Proxy (t m r))
                       >> Just CRTr)
           in \g -> let (v,g') = random g \\ witness entailRandomT v
                    in (cons v, g')

  randomR _ = error "randomR non-sensical for UCyc"

instance (Show r, Fact m, TElt t r) => Show (UCyc t m rep r) where
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
