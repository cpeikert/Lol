{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances, GADTs, InstanceSigs,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RankNTypes, RebindableSyntax, ScopedTypeVariables,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | An implementation of cyclotomic rings.  WARNING: this module
-- provides an experts-only, "unsafe" interface that may result in
-- runtime errors if not used correctly!
-- 'Crypto.Lol.Cyclotomic.Cyc.Cyc' provides a safe interface, and
-- should be used in applications whenever possible.
--
-- 'UCyc' transparently handles all necessary conversions between
-- internal representations to support fast ring operations, and
-- efficiently stores and operates upon elements that are known to
-- reside in subrings.
--
-- The 'Functor', 'Applicative', 'Foldable', and 'Traversable'
-- instances of 'UCyc', as well as the 'fmapC' and 'fmapCM' functions,
-- work over the element's /current/ @r@-basis representation (or
-- 'pure' scalar representation as a special case, to satisfy the
-- 'Applicative' laws), and the output remains in that representation.
-- If the input's representation is not one of these, the behavior is
-- a runtime error.  To ensure a valid representation when using the
-- methods from these classes, first call 'forceBasis' or one of its
-- specializations ('forcePow', 'forceDec', 'forceAny').

module Crypto.Lol.Cyclotomic.UCyc
(
-- * Data type
  UCyc, CElt
-- * Basic operations
, mulG, divG
, scalarCyc, liftCyc
, adviseCRT
-- * Error sampling
, tGaussian, errorRounded, errorCoset
-- * Sub/extension rings
, embed, twace, coeffsCyc, powBasis, crtSet
-- * Representations
, forceBasis, forcePow, forceDec, forceAny
-- * Specialized maps
, fmapC, fmapCM
, U.Basis(..), U.RescaleCyc
) where

import           Crypto.Lol.CRTrans
import           Crypto.Lol.Cyclotomic.Tensor  as T
import qualified Crypto.Lol.Cyclotomic.Utility as U
import           Crypto.Lol.Gadget
import           Crypto.Lol.LatticePrelude     as LP
import           Crypto.Lol.Types.FiniteField
import           Crypto.Lol.Types.ZPP

import Algebra.Additive     as Additive (C)
import Algebra.Ring         as Ring (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative    hiding ((*>))
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.Random
import Data.Coerce
import Data.Foldable          as F
import Data.Maybe
import Data.Traversable
import Data.Typeable
import Test.QuickCheck

import qualified Debug.Trace as DT

-- | A data type for representing cyclotomic rings such as @Z[zeta]@,
-- @Zq[zeta]@, and @Q(zeta)@: @t@ is the 'Tensor' type for storing
-- coefficients; @m@ is the cyclotomic index; @r@ is the base ring of
-- the coefficients (e.g., @Z@, @Zq@).
data UCyc t (m :: Factored) r where
  Pow  :: !(t m r) -> UCyc t m r -- representation wrt powerful basis
  Dec  :: !(t m r) -> UCyc t m r -- decoding basis

  -- Invariant: use CRTr if and only if crtFuncs exists for (t m r);
  -- otherwise use CRTe (because crtFuncs is guaranteed to exist for
  -- (t m (CRTExt r))
  CRTr :: !(t m r) -> UCyc t m r -- wrt CRT basis over r, if it exists
  CRTe :: !(t m (CRTExt r)) -> UCyc t m r -- wrt CRT basis over r-extension

  -- super-optimized storage of scalars
  Scalar :: !r -> UCyc t m r

  -- optimized storage of subring elements
  Sub  :: (l `Divides` m) => !(UCyc t l r) -> UCyc t m r


  --EAC: Consider this representation for product rings, but beware of combinatorial explosion of cases.
  --Product :: !(UCyc t m a) -> !(UCyc t m b) -> UCyc t m (a,b)
  deriving (Typeable)

-- | Shorthand for frequently reused constraints that are needed for
--  change of basis.
type UCCtx t r = (Tensor t, CRTrans r, CRTrans (CRTExt r), CRTEmbed r,
                  ZeroTestable r, TElt t r, TElt t (CRTExt r))

-- | Shorthand for frequently reused constraints that are needed for
-- most functions involving 'UCyc' and 'Crypto.Lol.Cyclotomic.Cyc.Cyc'.

-- EAC: duplicated UCCtx for haddock
type CElt t r = (Tensor t, CRTrans r, CRTrans (CRTExt r), CRTEmbed r,
                 ZeroTestable r, TElt t r, TElt t (CRTExt r), Eq r, NFData r)

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.scalarCyc', but for 'UCyc'.
scalarCyc :: (Fact m, CElt t a) => a -> UCyc t m a
scalarCyc = Scalar

-- Eq instance
instance (UCCtx t r, Fact m, Eq r) => Eq (UCyc t m r) where
  -- handle same bases when fidelity allows (i.e., *not* CRTe basis)
  (Scalar v1) == (Scalar v2) = v1 == v2
  (Pow v1) == (Pow v2) = v1 == v2 \\ witness entailFullT v1
  (Dec v1) == (Dec v2) = v1 == v2 \\ witness entailFullT v1
  (CRTr v1) == (CRTr v2) = v1 == v2 \\ witness entailFullT v1

  (Sub (c1 :: UCyc t l1 r)) == (Sub (c2 :: UCyc t l2 r)) =
    (embed' c1 :: UCyc t (FLCM l1 l2) r) == embed' c2
    \\ lcmDivides (Proxy::Proxy l1) (Proxy::Proxy l2)

  -- otherwise compare in power basis for fidelity, which involves
  -- the most efficient transforms in all cases
  p1 == p2 = toPow' p1 == toPow' p2

---------- Numeric Prelude instances ----------

-- ZeroTestable instance
instance (UCCtx t r, Fact m) => ZeroTestable.C (UCyc t m r) where
  isZero (Scalar v) = isZero v
  isZero (Pow v) = isZero v \\ witness entailFullT v
  isZero (Dec v) = isZero v \\ witness entailFullT v
  isZero (CRTr v) = isZero v \\ witness entailFullT v
  isZero x@(CRTe _) = isZero $ toPow' x
  isZero (Sub c) = isZero c

-- Additive instance
instance (UCCtx t r, Fact m) => Additive.C (UCyc t m r) where

  zero = Scalar zero

  -- optimized addition of zero
  (Scalar c1) + v2 | isZero c1 = v2
  v1 + (Scalar c2) | isZero c2 = v1

  -- SAME CONSTRUCTORS
  (Scalar c1) + (Scalar c2) = Scalar (c1+c2)
  (Pow v1) + (Pow v2) = Pow $ v1 + v2 \\ witness entailFullT v1
  (Dec v1) + (Dec v2) = Dec $ v1 + v2 \\ witness entailFullT v1
  (CRTr v1) + (CRTr v2) = CRTr $ v1 + v2 \\ witness entailFullT v1
  -- CJP: is this OK for precision?
  (CRTe v1) + (CRTe v2) = CRTe $ v1 + v2 \\ witness entailFullT v1
  -- Sub plus Sub: work in compositum
  (Sub (c1 :: UCyc t m1 r)) + (Sub (c2 :: UCyc t m2 r)) =
    (Sub $ (embed' c1 :: UCyc t (FLCM m1 m2) r) + embed' c2)
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- SCALAR PLUS SOMETHING ELSE

  p1@(Scalar _) + p2@(Pow _) = toPow' p1 + p2
  p1@(Scalar _) + p2@(Dec _) = toDec' p1 + p2
  p1@(Scalar _) + p2@(CRTr _) = toCRT' p1 + p2
  p1@(Scalar _) + p2@(CRTe _) = toCRT' p1 + p2
  (Scalar v1) + (Sub c2) = Sub $ Scalar v1 + c2

  p1@(Pow _) + p2@(Scalar _) = p1 + toPow' p2
  p1@(Dec _) + p2@(Scalar _) = p1 + toDec' p2
  p1@(CRTr _) + p2@(Scalar _) = p1 + toCRT' p2
  p1@(CRTe _) + p2@(Scalar _) = p1 + toCRT' p2
  (Sub c1) + (Scalar v2) = Sub $ c1 + Scalar v2

  -- SUB PLUS SOMETHING ELSE (NON-SCALAR): work in full ring
  (Sub c1) + c2 = embed' c1 + c2
  c1 + (Sub c2) = c1 + embed' c2

  -- mixed Dec and Pow: use linear time conversions
  p1@(Dec _) + p2@(Pow _) = toPow' p1 + p2
  p1@(Pow _) + p2@(Dec _) = p1 + toPow' p2

  -- one CRTr: convert other to CRTr
  p1@(CRTr _) + p2 = p1 + toCRT' p2
  p1 + p2@(CRTr _) = toCRT' p1 + p2

  -- else, one is CRTe: convert both to Pow for fidelity and best
  -- efficiency
  p1 + p2 = toPow' p1 + toPow' p2

  negate (Scalar c) = Scalar (negate c)
  negate (Pow v) = Pow $ fmapT negate v
  negate (Dec v) = Dec $ fmapT negate v
  negate (CRTr v) = CRTr $ fmapT negate v
  negate (CRTe v) = CRTe $ fmapT negate v
  negate (Sub c) = Sub $ negate c

-- Ring instance
instance (UCCtx t r, Fact m) => Ring.C (UCyc t m r) where

  one = Scalar one

  -- optimized mul-by-zero
  v1@(Scalar c1) * _ | isZero c1 = v1
  _ * v2@(Scalar c2) | isZero c2 = v2

  -- BOTH IN A CRT BASIS
  (CRTr v1) * (CRTr v2) = CRTr $ v1 * v2 \\ witness entailFullT v1
  (CRTe v1) * (CRTe v2) = toPow' $ CRTe $ v1 * v2 \\ witness entailFullT v1

  -- AT LEAST ONE SCALAR
  (Scalar c1) * (Scalar c2) = Scalar $ c1 * c2

  (Scalar c) * (Pow v) = Pow $ fmapT (*c) v
  (Scalar c) * (Dec v) = Dec $ fmapT (*c) v
  (Scalar c) * (CRTr v) = CRTr $ fmapT (*c) v
  s@(Scalar _) * c'@(CRTe _) = s * toPow' c'
  (Scalar c) * (Sub c2) = Sub $ Scalar c * c2

  (Pow v) * (Scalar c) = Pow $ fmapT (*c) v
  (Dec v) * (Scalar c) = Dec $ fmapT (*c) v
  (CRTr v) * (Scalar c) = CRTr $ fmapT (*c) v
  c'@(CRTe _) * s@(Scalar _) = toPow' c' * s
  (Sub c1) * (Scalar c) = Sub $ c1 * Scalar c

  -- AT LEAST ONE SUB

  -- two Subs: work in compositum
  (Sub (c1 :: UCyc t m1 r)) * (Sub (c2 :: UCyc t m2 r)) =
    (Sub $ (embed' c1 :: UCyc t (FLCM m1 m2) r) * embed' c2)
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- Sub times something else (non-Scalar): work in full ring
  (Sub c1) * p2 = embed' c1 * p2
  p1 * (Sub c2) = p1 * embed' c2

  -- ELSE: work in appropriate CRT basis
  p1 * p2 = toCRT' p1 * toCRT' p2

  fromInteger = Scalar . fromInteger

-- reduces in any basis
instance (Reduce a b, Fact m, CElt t a, CElt t b)
  => Reduce (UCyc t m a) (UCyc t m b) where

  reduce = fmapC reduce . forceAny

-- promote Gadget from base ring
instance (Gadget gad zq, Fact m, CElt t zq) => Gadget gad (UCyc t m zq) where
  gadget = (scalarCyc <$>) <$> gadget
  -- specialization of 'encode', done efficiently (via 'adviseCRT').
  encode s = ((* adviseCRT s) <$>) <$> gadget

-- promote Decompose, using the powerful basis
instance (Decompose gad zq, Fact m, CElt t zq,
          Reduce (UCyc t m (DecompOf zq)) (UCyc t m zq))
  => Decompose gad (UCyc t m zq) where

  type DecompOf (UCyc t m zq) = UCyc t m (DecompOf zq)

  -- traverse: Traversable (c m) and Applicative (Tagged gad ZL)
  decompose = fromZL . traverse (toZL . decompose) . forcePow
    where toZL :: Tagged s [a] -> TaggedT s ZipList a
          toZL = coerce
          fromZL :: TaggedT s ZipList a -> Tagged s [a]
          fromZL = coerce

-- promote Correct, using the decoding basis
instance (Correct gad zq, Fact m, CElt t zq)
         => Correct gad (UCyc t m zq) where
  -- sequenceA: Applicative (c m) and Traversable (TaggedT [])
  correct bs = (correct . pasteT) <$> (sequenceA $ forceDec <$> peelT bs)

-- generic RescaleCyc instance

instance {-# OVERLAPS #-} (Rescale a b, CElt t a, CElt t b)
         => U.RescaleCyc (UCyc t) a b where
  rescaleCyc b = fmapC rescale . forceBasis (Just b)

-- specialized instance for product rings: ~2x faster algorithm
instance (Mod a, Field b, Lift a z, Reduce z b,
          CElt t a, CElt t b, CElt t (a,b), CElt t z)
         => U.RescaleCyc (UCyc t) (a,b) b where
  rescaleCyc bas =
    let aval = proxy modulus (Proxy::Proxy a)
  -- CJP: could use unzipC here to get (a,b) in one pass, but it
  -- requires adding that method, and unzipT to Tensor and all its
  -- instances. Probably not worth it.
    in \x -> let y = forceAny x
                 a = fmapC fst y
                 b = fmapC snd y
                 z = liftCyc bas a
             in (pure (recip (fromIntegral aval))) * (b - reduce z)

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.liftCyc', but for 'UCyc'.
liftCyc :: (Lift b a, Fact m, CElt t a, CElt t b)
           => U.Basis -> UCyc t m b -> UCyc t m a
liftCyc U.Pow = fmapC lift . forceBasis (Just U.Pow)
liftCyc U.Dec = fmapC lift . forceBasis (Just U.Dec)

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
-- fromJust is safe here because we're already in CRTr
mulG (CRTr v) = CRTr $ fromMaybe (error "FC.mulG CRTr") mulGCRT v
mulG (CRTe v) = CRTe $ fromMaybe (error "FC.mulG CRTe") mulGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.divG', but for 'UCyc'.
divG :: (Fact m, CElt t r) => UCyc t m r -> Maybe (UCyc t m r)
divG (Scalar c) = liftM Pow (divGPow $ scalarPow c) -- full ring
divG (Sub c) = divG $ embed' c                      -- full ring
divG (Pow v) = Pow <$> divGPow v
divG (Dec v) = Dec <$> divGDec v
-- fromJust is safe here because we're already in CRTr
divG (CRTr v) = Just $ CRTr $ fromMaybe (error "FC.divG CRTr") divGCRT v
divG (CRTe v) = Just $ CRTe $ fromMaybe (error "FC.divG CRTe") divGCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.tGaussian', but for 'UCyc'.
tGaussian :: (Fact m, OrdFloat q, Random q, CElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (UCyc t m q)
tGaussian = liftM Dec . tGaussianDec

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
twace :: forall t r m m' . (UCCtx t r, m `Divides` m')
         => UCyc t m' r -> UCyc t m r
twace (Scalar c) = Scalar c
-- twace on Sub goes to the largest common subring of O_l and O_m
twace (Sub (c :: UCyc t l r)) =
  Sub (twace c :: UCyc t (FGCD l m) r)
  \\ gcdDivides (Proxy::Proxy l) (Proxy::Proxy m)
twace (Pow v) = Pow $ twacePowDec v
twace (Dec v) = Dec $ twacePowDec v
-- stay in CRTr only if it's possible, otherwise go to Pow
twace x@(CRTr v) =
  fromMaybe (twace $ toPow' x) (CRTr <$> (twaceCRT <*> pure v))
-- CJP: stay in CRTe: precision OK?
twace (CRTe v) = CRTe $ fromMaybe (error "FC.twace CRTe") twaceCRT v

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.coeffsCyc', but for 'UCyc'.
coeffsCyc :: (m `Divides` m', CElt t r) 
             => U.Basis -> UCyc t m' r -> [UCyc t m r]
coeffsCyc U.Pow (Pow v) = LP.map Pow $ coeffs v
coeffsCyc U.Dec (Dec v) = LP.map Dec $ coeffs v
coeffsCyc U.Pow x = coeffsCyc U.Pow $ toPow' x
coeffsCyc U.Dec x = coeffsCyc U.Dec $ toDec' x

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.powBasis', but for 'UCyc'.
powBasis :: (m `Divides` m', CElt t r) => Tagged m [UCyc t m' r]
powBasis = map Pow <$> powBasisPow

-- | Same as 'Crypto.Lol.Cyclotomic.Cyc.crtSet', but for 'UCyc'.
crtSet :: forall t m m' r p mbar m'bar .
           (m `Divides` m', ZPP r, p ~ CharOf (ZPOf r), 
            mbar ~ PFree p m, m'bar ~ PFree p m',
            CElt t r, CElt t (ZPOf r))
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
            (crtSetDec :: Tagged mbar [t m'bar (ZPOf r)]))
     \\ pFreeDivides pp pm pm'
     \\ pSplitTheorems pp pm \\ pSplitTheorems pp pm'

----- "Unsafe" functions that expose or rely upon internal representation

-- | Yield an equivalent element whose internal representation /must/
-- be in the indicated basis: powerful or decoding (for 'Just' 'Pow'
-- and 'Just' 'Dec' arguments, respectively), or any @r@-basis of the
-- implementation's choice (for 'Nothing' argument).  (See also the
-- convenient specializations 'forcePow', 'forceDec', 'forceAny'.)
forceBasis :: (Fact m, CElt t r) => Maybe U.Basis -> UCyc t m r -> UCyc t m r
forceBasis (Just U.Pow) x = toPow' x
forceBasis (Just U.Dec) x = toDec' x
forceBasis Nothing x@(Scalar _) = toPow' x
forceBasis Nothing (Sub c) = forceBasis Nothing $ embed' c
forceBasis Nothing x@(CRTe _) = toPow' x
forceBasis Nothing x = x

forcePow, forceDec, forceAny :: (Fact m, CElt t r) => UCyc t m r -> UCyc t m r
-- | Force a cyclotomic element into the powerful basis.
forcePow = forceBasis (Just U.Pow)
-- | Force a cyclotomic element into the decoding basis.
forceDec = forceBasis (Just U.Dec)
-- | Force a cyclotomic into any @r@-basis of the implementation's
-- choice.
forceAny = forceBasis Nothing

-- | A more specialized version of 'fmap': apply a function
-- coordinate-wise in the current representation.  The caller must
-- ensure that the current representation is an @r@-basis (one of
-- powerful, decoding, or CRT, if it exists), usually by using
-- 'forceBasis' or its specializations ('forcePow', 'forceDec',
-- 'forceAny').  Otherwise, behavior is undefined.
fmapC :: (Fact m, CElt t a, CElt t b) => (a -> b) -> UCyc t m a -> UCyc t m b

-- must be in an r-basis for correct semantics, e.g., f 0 = 1
fmapC _ (Scalar _) = error "can't fmapC on Scalar.  Must forceBasis first!"
fmapC _ (Sub _) = error "can't fmapC on Sub.  Must forceBasis first!"
fmapC _ (CRTe _) = error "can't fmapC on CRTe.  Must forceBasis first!"

fmapC f (Pow v) = Pow $ fmapT f v
fmapC f (Dec v) = Dec $ fmapT f v
fmapC f (CRTr v) = CRTr $ fmapT f v

-- | Monadic version of 'fmapC'.
fmapCM :: (Fact m, CElt t a, CElt t b, Monad mon)
  => (a -> mon b) -> UCyc t m a -> mon (UCyc t m b)

-- must embed into full ring
fmapCM _ (Scalar _) = error "can't fmapCM on Scalar. Must forceBasis first!"
fmapCM _ (Sub _) = error "can't fmapCM on Sub. Must forceBasis first!"
fmapCM _ (CRTe _) =  error "can't fmapCM on CRTe.  Must forceBasis first!"

fmapCM f (Pow v) = liftM Pow $ fmapTM f v
fmapCM f (Dec v) = liftM Dec $ fmapTM f v
fmapCM f (CRTr v) = liftM CRTr $ fmapTM f v




---------- HELPER FUNCTIONS (NOT FOR EXPORT) ----------

-- | Force embed, to a non-Sub constructor.
embed' :: forall t r l m .
          (UCCtx t r, l `Divides` m) => UCyc t l r -> UCyc t m r
embed' (Scalar v) = Scalar v
embed' (Pow v) = Pow $ embedPow v
embed' (Dec v) = Dec $ embedDec v
-- stay in CRTr only if it's possible, otherwise go to Pow
embed' x@(CRTr v) =
    fromMaybe (embed' $ toPow' x) (CRTr <$> (embedCRT <*> pure v))
-- Staying in CRTe might not be safe, because the target tensor
-- might have implemented a CRTr even if the source tensor
-- hasn't.  Mathematically this is impossible (because target has
-- CRTr only if source does), so this is purely about implementation.
embed' x@(CRTe _) = embed' $ toPow' x
embed' (Sub (c :: UCyc t k r)) = embed' c
  \\ transDivides (Proxy::Proxy k) (Proxy::Proxy l) (Proxy::Proxy m)


--------- Basis conversion methods ------------------

toPow', toDec' :: (UCCtx t r, Fact m) => UCyc t m r -> UCyc t m r
-- forces the argument into the powerful basis
toPow' (Scalar c) = Pow $ scalarPow c
toPow' (Sub c) = toPow' $ embed' c
toPow' x@(Pow _) = x
toPow' (Dec v) = Pow $ l v
toPow' (CRTr v) = Pow $ fromMaybe (error "FC.toPow'") crtInv v
toPow' (CRTe v) = Pow $ fmapT fromExt $ fromMaybe (error "FC.toPow'") crtInv v

-- forces the argument into the decoding basis
toDec' x@(Scalar _) = toDec' $ toPow' x -- use scalarDec instead
toDec' (Sub c) = toDec' $ embed' c
toDec' (Pow v) = Dec $ lInv v
toDec' x@(Dec _) = x
toDec' (CRTr v) = Dec $ lInv $ fromMaybe (error "FC.toDec'") crtInv v
toDec' (CRTe v) = Dec $ lInv $ fmapT fromExt $ fromMaybe (error "FC.toDec'") crtInv v

-- forces the argument into a CRT basis, according to the invariant
-- about which one should be used
toCRT' :: forall t m r . (UCCtx t r, Fact m) => UCyc t m r -> UCyc t m r
toCRT' (Sub c) = toCRT' $ embed' c
toCRT' x@(CRTr _) = x
toCRT' x@(CRTe _) = x
toCRT' x = fromMaybe (toCRTe x) (toCRTr <*> pure x)
  -- CJP: defining these helpers internally so they can't be called
  -- from anywhere else.  Therefore, the only way to convert to a
  -- CRT basis is through the toCRT' method.
  where
    toCRTr = do -- Maybe monad
      crt' <- crt
      scalarCRT' <- scalarCRT
      return $ \x -> case x of
        (Scalar c) -> CRTr $ scalarCRT' c
        (Pow v) -> CRTr $ crt' v
        (Dec v) -> CRTr $ crt' $ l v
          -- deliberately omit CRTe case, which should
          -- never happen by internal invariant, so trigger
          -- error if it does
    toCRTe = let m = proxy valueFact (Proxy::Proxy m)
                 crt' = fromMaybe (error $ "FC.toCRT': no crt': " ++ (show m)) crt :: t m (CRTExt r) -> t m (CRTExt r) -- must exist
                 scalarCRT' = fromMaybe (error "FC.toCRT': no scalar crt'") scalarCRT :: CRTExt r -> t m (CRTExt r)
             in \x -> case x of
               (Scalar c) -> CRTe $ scalarCRT' $ toExt c
               (Pow v) -> CRTe $ crt' $ fmapT toExt v
               (Dec v) -> CRTe $ crt' $ fmapT toExt $ l v

---------- "Container" instances ----------

instance (Tensor t, Fact m) => Functor (UCyc t m) where
  -- Functor instance is implied by Applicative laws
  fmap f x = pure f <*> x

errApp name = error $ "UCyc.Applicative: can't/won't handle " ++ name ++
              "; call forcePow|Dec first"

instance (Tensor t, Fact m) => Applicative (UCyc t m) where

  -- This implementation is restricted to the Scalar, Pow, Dec, or
  -- CRTr constructors, in order to force the client to choose a
  -- concrete @r@-basis and avoid unanticipated non-failure behavior.
  -- Encountering a CRTe, or Sub constructor almost certainly means
  -- that the client expressed something it did not intend (since it
  -- cannot force such constructors to be used), so we want to raise
  -- an exception early instead of doing unintended work.

  -- This implementation has one corner case that may
  -- yield unexpected non-failure behavior: consider
  --   fmap f (pure a) = (pure f) <*> (pure a) = (pure $ f a)
  -- which is required by the Applicative homomorphism law.

  -- If the (pure a) is intended as an element of the base ring (which
  -- is the custom), then its Pow coeffs are *not* all a's, so the
  -- (likely intended) expression
  --   fmap f $ forcePow (pure a)
  -- may be a different result.  If the client forgets the force, we
  -- can't recognize it here and throw an error.  (This is certainly the
  -- client's fault; if it's not specifying a basis before fmap'ing
  -- then it shouldn't expect the results to make sense.  We just
  -- can't catch the error here.)

  -- A solution is to introduce an explicit Pure constructor that's
  -- only ever applied in 'pure', and throw an error if we encounter a
  -- Scalar here.  Arithmetically we'd treat Pures as Scalars, but in
  -- a one-way fashion (outputs of arith ops are never Pure).

  pure = Scalar

  -- homomorphism (of pure)
  (Scalar f) <*> (Scalar a) = Scalar $ f a

  -- constructors must match
  (Pow v1) <*> (Pow v2) = Pow $ v1 <*> v2 \\ witness entailIndexT v1
  (Dec v1) <*> (Dec v2) = Dec $ v1 <*> v2 \\ witness entailIndexT v1
  (CRTr v1) <*> (CRTr v2) = CRTr $ v1 <*> v2 \\ witness entailIndexT v1

  -- ... but we can also match Scalar with (almost) anything
  (Scalar f) <*> (Pow v) = Pow $ pure f <*> v \\ witness entailIndexT v
  (Scalar f) <*> (Dec v) = Dec $ pure f <*> v \\ witness entailIndexT v
  (Scalar f) <*> (CRTr v) = CRTr $ pure f <*> v \\ witness entailIndexT v

  (Pow v) <*> (Scalar a) = Pow $ v <*> pure a \\ witness entailIndexT v
  (Dec v) <*> (Scalar a) = Dec $ v <*> pure a \\ witness entailIndexT v
  (CRTr v) <*> (Scalar a) = CRTr $ v <*> pure a \\ witness entailIndexT v

  -- cases we can't/won't handle
  (Pow _) <*> (Dec _) = error "UCyc.Applicative: Pow/Dec combo"
  (Dec _) <*> (Pow _) = error "UCyc.Applicative: Pow/Dec combo"
  (Sub _) <*> _  = errApp "Sub"
  _ <*> (Sub _)  = errApp "Sub"
  (CRTe _) <*> _ = errApp "CRTe"
  _ <*> (CRTe _) = errApp "CRTe"

instance (Tensor t, Fact m) => Foldable (UCyc t m) where
  foldr f b (Scalar r) = f r b
  foldr f b (Sub c) = F.foldr f b c
  foldr f b (Pow v) = F.foldr f b v \\ witness entailIndexT v
  foldr f b (Dec v) = F.foldr f b v \\ witness entailIndexT v
  foldr f b (CRTr v) = F.foldr f b v \\ witness entailIndexT v
  foldr _ _ (CRTe _) = error "UCyc.Foldable: can't handle CRTe"

instance (Tensor t, Fact m) => Traversable (UCyc t m) where
  traverse f (Scalar r) = Scalar <$> f r
  traverse f (Sub c) = Sub <$> traverse f c
  traverse f (Pow v) = Pow <$> traverse f v \\ witness entailIndexT v
  traverse f (Dec v) = Dec <$> traverse f v \\ witness entailIndexT v
  traverse f (CRTr v) = CRTr <$> traverse f v \\ witness entailIndexT v
  traverse _ (CRTe _) = error "UCyc.Traversable: can't handle CRTe"

---------- Utility instances ----------

instance (Tensor t, Fact m, TElt t r, CRTrans r) => Random (UCyc t m r) where

  -- create in CRTr basis if legal, otherwise in powerful
  random = let cons = fromMaybe Pow
                      (proxyT hasCRTFuncs (Proxy::Proxy (t m r)) >> Just CRTr)
           in \g -> let (v,g') = random g
                                 \\ proxy entailFullT (Proxy::Proxy (t m r))
                    in (cons v, g')

  randomR _ = error "randomR non-sensical for cyclotomic rings"

instance (Show r, Show (t m r), Show (t m (CRTExt r)))
  => Show (UCyc t m r) where

  show (Scalar c) = "scalar " ++ show c
  show (Sub _) = "subring (not showing due to missing constraints)"
  show (Pow v) = "powerful basis coeffs " ++ show v
  show (Dec v) = "decoding basis coeffs " ++ show v
  show (CRTr v) = "CRTr basis coeffs " ++ show v
  show (CRTe v) = "CRTe basis coeffs " ++ show v

instance (Arbitrary (t m r)) => Arbitrary (UCyc t m r) where
  arbitrary = liftM Pow arbitrary
  shrink = shrinkNothing

instance (Tensor t, Fact m, NFData r, TElt t r, TElt t (CRTExt r))
         => NFData (UCyc t m r) where
  rnf (Pow x)    = rnf x \\ witness entailFullT x
  rnf (Dec x)    = rnf x \\ witness entailFullT x
  rnf (CRTr x)   = rnf x \\ witness entailFullT x
  rnf (CRTe x)   = rnf x \\ witness entailFullT x
  rnf (Scalar x) = rnf x
  rnf (Sub x)    = rnf x
