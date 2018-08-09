{-|
Module      : Crypto.Lol.Cyclotomic.Cyc
Description : An implementation of cyclotomic rings that hides and
              automatically manages the internal representations of
              ring elements.
Copyright   : (c) Eric Crockett, 2011-2018
                  Chris Peikert, 2011-2018
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

An implementation of cyclotomic rings that hides the
internal representations of ring elements (e.g., the choice of
basis), and also offers more efficient storage and operations on
subring elements (including elements from the base ring itself).

For an implementation that allows (and requires) the programmer to
control the underlying representation, see
"Crypto.Lol.Cyclotomic.CycRep".

__WARNING:__ as with all fixed-point arithmetic, the functions
associated with 'Cyc' may result in overflow (and thereby
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
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Cyclotomic.Cyc
(
-- * Data type
  Cyc
-- * Constructors/deconstructors
, UnCyc(..)
) where

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Module       as Module (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

import           Crypto.Lol.CRTrans
import           Crypto.Lol.Cyclotomic.CycRep   hiding (coeffsDec,
                                                 coeffsPow, crtSet,
                                                 powBasis)
import qualified Crypto.Lol.Cyclotomic.CycRep   as R
import           Crypto.Lol.Cyclotomic.Language hiding (Dec, Pow)
import qualified Crypto.Lol.Cyclotomic.Language as L
import           Crypto.Lol.Cyclotomic.Tensor   (TensorCRTSet,
                                                 TensorGSqNorm,
                                                 TensorGaussian,
                                                 TensorPowDec)
import           Crypto.Lol.Gadget
import           Crypto.Lol.Prelude             as LP
import           Crypto.Lol.Reflects
import           Crypto.Lol.Types               (RRq, ZqBasic)
import           Crypto.Lol.Types.FiniteField
import           Crypto.Lol.Types.IFunctor
import           Crypto.Lol.Types.Proto
import           Crypto.Lol.Types.ZPP

import           Control.Applicative  hiding ((*>))
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.Random hiding (lift)
import           Data.Constraint      ((:-), Dict (..), (\\))
import qualified Data.Constraint      as C
import           Data.Foldable        (Foldable)
import           Data.Traversable
import           Language.Haskell.TH

-- | Underlying GADT for a cyclotomic ring in one of several
-- representations.
data CycG t m r where
  Pow :: !(CycRep t P m r) -> CycG t m r
  Dec :: !(CycRep t D m r) -> CycG t m r
  CRT :: !(CycRepEC t m r) -> CycG t m r
  -- super-optimized storage of scalars
  Scalar :: !r -> CycG t m r
  -- optimized storage of subring elements
  Sub :: (l `Divides` m) => !(CycG t l r) -> CycG t m r
  -- CJP: someday try to merge the above two?

-- | A cyclotomic ring such as \( \Z[\zeta_m] \), \( \Z_q[\zeta_m] \),
-- or \( \Q[\zeta_m] \): @t@ is the 'Tensor' type for storing
-- coefficient tensors; @m@ is the cyclotomic index; @r@ is the base
-- ring of the coefficients (e.g., \(\ \Q \), \( \Z \), \( \Z_q \)).
data family Cyc (t :: Factored -> * -> *) (m :: Factored) r

type instance LiftOf (CycG t m r) = CycG t m (LiftOf r)
type instance LiftOf (Cyc  t m r) = Cyc  t m (LiftOf r)

-- could also do an Int instance
newtype instance Cyc t m Double        = CycDbl { unCycDbl :: CycG t m Double }
newtype instance Cyc t m Int64         = CycI64 { unCycI64 :: CycG t m Int64 }
newtype instance Cyc t m (ZqBasic q z) = CycZqB { unCycZqB :: CycG t m (ZqBasic q z) }

-- | cyclotomic over a product base ring, represented as a product of
-- cyclotomics over the individual rings
data    instance Cyc t m (a,b)         = CycPair !(Cyc t m a) !(Cyc t m b)

-- | cyclotomic ring of integers with unbounded precision, limited to
-- powerful- or decoding-basis representation.
data instance Cyc t m Integer
  = PowIgr !(CycRep t P m Integer)
  | DecIgr !(CycRep t D m Integer)

-- | additive group \( K/qR \), limited to powerful- or decoding-basis
-- representation
data instance Cyc t m (RRq q r)
  = PowRRq !(CycRep t P m (RRq q r))
  | DecRRq !(CycRep t D m (RRq q r))

---------- Constructors / destructors ----------

-- | Convenience wrapper.
cycPC :: Either (CycRep t P m r) (CycRep t C m r) -> CycG t m r
cycPC = either Pow (CRT . Right)
{-# INLINABLE cycPC #-}

-- | Convenience wrapper.
cycPE :: Either (CycRep t P m r) (CycRep t E m r) -> CycG t m r
cycPE = either Pow (CRT . Left)
{-# INLINABLE cycPE #-}

-- | Unwrap a 'CycG' as a 'CycRep' in powerful-basis representation.
unCycGPow :: (Fact m, CRTElt t r) => CycG t m r -> CycRep t P m r
{-# INLINABLE unCycGPow #-}
unCycGPow c = let (Pow u) = toPow' c in u

-- | Unwrap a 'CycG' as a 'CycRep' in decoding-basis representation.
unCycGDec :: (Fact m, CRTElt t r) => CycG t m r -> CycRep t D m r
{-# INLINABLE unCycGDec #-}
unCycGDec c = let (Dec u) = toDec' c in u

{-
-- | Unwrap a 'CycG' as a 'CycRep' in a CRT-basis representation.
uncycCRT :: (Fact m, CRTElt t r) => CycG t m r -> CycRepEC t m r
{-# INLINABLE uncycCRT #-}
uncycCRT c = let (CRT u) = toCRT' c in u
-}

-- | Go between 'Cyc' and 'CycRep', in a desired representation.
class UnCyc t r where
  cycPow   :: (Fact m) => CycRep t P m r -> Cyc t m r
  cycDec   :: (Fact m) => CycRep t D m r -> Cyc t m r
  unCycPow :: (Fact m) => Cyc t m r -> CycRep t P m r
  unCycDec :: (Fact m) => Cyc t m r -> CycRep t D m r

instance CRTElt t Double => UnCyc t Double where
  cycPow   = CycDbl . Pow
  cycDec   = CycDbl . Dec
  unCycPow = unCycGPow . unCycDbl
  unCycDec = unCycGDec . unCycDbl

instance CRTElt t Int64 => UnCyc t Int64 where
  cycPow   = CycI64 . Pow
  cycDec   = CycI64 . Dec
  unCycPow = unCycGPow . unCycI64
  unCycDec = unCycGDec . unCycI64

instance CRTElt t (ZqBasic q z) => UnCyc t (ZqBasic q z) where
  cycPow   = CycZqB . Pow
  cycDec   = CycZqB . Dec
  unCycPow = unCycGPow . unCycZqB
  unCycDec = unCycGDec . unCycZqB

-- CJP TODO: one for Integer?; would require converting between Pow and
-- Dec reps in pure Haskell

instance TensorPowDec t (RRq q r) => UnCyc t (RRq q r) where
  cycPow = PowRRq
  cycDec = DecRRq
  unCycPow (PowRRq v) = v
  unCycPow (DecRRq v) = toPow v
  unCycDec (DecRRq v) = v
  unCycDec (PowRRq v) = toDec v

instance (UnCyc t a, UnCyc t b, IFunctor t, IFElt t a, IFElt t b, IFElt t (a,b))
  => UnCyc t (a,b) where
  cycPow = uncurry CycPair . ((cycPow . fmapI fst) &&& (cycPow . fmapI snd))
  cycDec = uncurry CycPair . ((cycDec . fmapI fst) &&& (cycDec . fmapI snd))

  unCycPow (CycPair a b) = zipWithI (,) (unCycPow a) (unCycPow b)
  unCycDec (CycPair a b) = zipWithI (,) (unCycDec a) (unCycDec b)

---------- Category-theoretic instances ----------

instance (Fact m, CRTElt t r, ForallFact1 Foldable t,
          Traversable (CycRep t P m), Traversable (CycRep t D m))
    => FoldableCyc (CycG t m) r where
  foldrCyc (Just L.Pow)   f acc (Pow u) = foldr f acc u
  foldrCyc (Just L.Dec)   f acc (Dec u) = foldr f acc u
  foldrCyc Nothing        f acc (Pow u) = foldr f acc u
  foldrCyc Nothing        f acc (Dec u) = foldr f acc u

  foldrCyc b@(Just L.Pow) f acc c       = foldrCyc b f acc $ toPow' c
  foldrCyc b@(Just L.Dec) f acc c       = foldrCyc b f acc $ toDec' c
  foldrCyc Nothing        f acc c       = foldrCyc Nothing f acc $ toPow' c

instance (FoldableCyc (CycG t m) Double) => FoldableCyc (Cyc t m) Double where
  foldrCyc bas f acc = foldrCyc bas f acc . unCycDbl

instance (FoldableCyc (CycG t m) Int64) => FoldableCyc (Cyc t m) Int64 where
  foldrCyc bas f acc = foldrCyc bas f acc . unCycI64

instance (FoldableCyc (CycG t m) (ZqBasic q z)) => FoldableCyc (Cyc t m) (ZqBasic q z) where
  foldrCyc bas f acc = foldrCyc bas f acc . unCycZqB

-- No instance for CycPair -- is one possible?

-- No instance for Cyc over Integer without TensorPowDec CT Integer,
-- because we need toDec and toPow.

instance (Fact m, TensorPowDec t (RRq q r),
          ForallFact1 Traversable t, ForallFact1 Applicative t)
    => FoldableCyc (Cyc t m) (RRq q r) where
  foldrCyc (Just L.Pow) f acc = foldr f acc . unCycPow
  foldrCyc (Just L.Dec) f acc = foldr f acc . unCycDec
  foldrCyc Nothing      f acc = foldrCyc (Just L.Pow) f acc

--- FunctorCyc

instance (Fact m, CRTElt t a, IFunctor t, IFElt t a, IFElt t b)
  => FunctorCyc (CycG t m) a b where
  fmapCyc (Just L.Pow) f = Pow . fmapI f . unCycGPow
  fmapCyc (Just L.Dec) f = Dec . fmapI f . unCycGDec
  fmapCyc Nothing      f = fmapCyc (Just L.Pow) f

-- CJP: the rest of the instances are autogen'd at end of file, to
-- avoid scoping problems

---------- Algebraic instances ----------

instance (Fact m, ZeroTestable r, CRTElt t r, ForallFact2 ZeroTestable.C t r)
  => ZeroTestable.C (CycG t m r) where
  isZero x = case x of
    (Pow u)         -> isZero u
    (Dec u)         -> isZero u
    (CRT (Right u)) -> isZero u
    c@(CRT _)       -> isZero $ toPow' c
    (Scalar c)      -> isZero c
    (Sub c)         -> isZero c
    \\ (entailFact2 :: Fact m :- ZeroTestable.C (t m r))

deriving instance ZeroTestable (CycG t m Double) => ZeroTestable.C (Cyc t m Double)
deriving instance ZeroTestable (CycG t m Int64) => ZeroTestable.C (Cyc t m Int64)
deriving instance ZeroTestable (CycG t m (ZqBasic q z)) => ZeroTestable.C (Cyc t m (ZqBasic q z))

instance (ZeroTestable (Cyc t m a), ZeroTestable (Cyc t m b))
  => ZeroTestable.C (Cyc t m (a,b)) where
  isZero (CycPair a b) = isZero a && isZero b

instance ZeroTestable (t m Integer) => ZeroTestable.C (Cyc t m Integer) where
  isZero (PowIgr v) = isZero v
  isZero (DecIgr v) = isZero v

instance ZeroTestable (t m (RRq q r)) => ZeroTestable.C (Cyc t m (RRq q r)) where
  isZero (PowRRq c) = isZero c
  isZero (DecRRq c) = isZero c

-----

instance (Eq r, Fact m, CRTElt t r, ForallFact2 Eq t r) => Eq (CycG t m r) where
  {-# INLINABLE (==) #-}
  -- same representations
  (Scalar c1) == (Scalar c2) = c1 == c2
  (Pow u1) == (Pow u2) = u1 == u2 \\ (entailFact2 :: Fact m :- Eq (t m r))
  (Dec u1) == (Dec u2) = u1 == u2 \\ (entailFact2 :: Fact m :- Eq (t m r))
  (CRT (Right u1)) == (CRT (Right u2)) =
    u1 == u2 \\ (entailFact2 :: Fact m :- Eq (t m r))

  -- compare Subs in compositum
  -- EAC: would like to convert c2 to basis of c1 *before* embedding
  (Sub (c1 :: CycG t l1 r)) == (Sub (c2 :: CycG t l2 r)) =
    (embed' c1 :: CycG t (FLCM l1 l2) r) == embed' c2
    \\ lcmDivides @l1 @l2

  -- some other relatively efficient comparisons
  (Scalar c1) == (Pow u2) = scalarPow c1 == u2
                            \\ (entailFact2 :: Fact m :- Eq (t m r))
  (Pow u1) == (Scalar c2) = u1 == scalarPow c2
                            \\ (entailFact2 :: Fact m :- Eq (t m r))

  -- otherwise: compare in powerful basis
  c1 == c2 = toPow' c1 == toPow' c2

deriving instance Eq (CycG t m Int64)         => Eq (Cyc t m Int64)
deriving instance Eq (CycG t m (ZqBasic q z)) => Eq (Cyc t m (ZqBasic q z))

instance (Eq (Cyc t m a), Eq (Cyc t m b)) => Eq (Cyc t m (a,b)) where
  (CycPair a b) == (CycPair a' b') = a == a' && b == b'

-- no Eq for Double or RRq due to precision, nor for Integer because
-- we can't change representations

instance (CRTElt t Int64, ForallFact2 Eq t Int64)
  => ForallFact2 Eq (Cyc t) Int64 where
  entailFact2 = C.Sub Dict

instance (Eq (ZqBasic q z), CRTElt t (ZqBasic q z),
          ForallFact2 Eq t (ZqBasic q z))
  => ForallFact2 Eq (Cyc t) (ZqBasic q z) where
  entailFact2 = C.Sub Dict

instance (ForallFact2 Eq (Cyc t) a, ForallFact2 Eq (Cyc t) b)
  => ForallFact2 Eq (Cyc t) (a,b) where
  entailFact2 :: forall m . Fact m :- Eq (Cyc t m (a,b))
  entailFact2 = C.Sub (Dict
                       \\ (entailFact2 :: Fact m :- Eq (Cyc t m a))
                       \\ (entailFact2 :: Fact m :- Eq (Cyc t m b)))

-----

instance (Fact m, CRTElt t r, ZeroTestable r) => Additive.C (CycG t m r) where
  {-# INLINABLE zero #-}
  zero = Scalar zero

  {-# INLINABLE (+) #-}
  -- optimized addition of zero
  (Scalar c1) + c2 | isZero c1 = c2
  c1 + (Scalar c2) | isZero c2 = c1

  -- SAME CONSTRUCTORS
  (Scalar c1) + (Scalar c2) = Scalar (c1+c2)
  (Pow u1) + (Pow u2) = Pow $ u1 + u2
  (Dec u1) + (Dec u2) = Dec $ u1 + u2
  (CRT u1) + (CRT u2) = CRT $ u1 + u2
  -- Sub plus Sub: work in compositum
  -- EAC: would like to convert c2 to basis of c1 before embedding
  (Sub (c1 :: CycG t m1 r)) + (Sub (c2 :: CycG t m2 r)) =
    (Sub $ (embed' c1 :: CycG t (FLCM m1 m2) r) + embed' c2)
    \\ lcm2Divides @m1 @m2 @m

  -- SCALAR PLUS SOMETHING ELSE

  (Scalar c)  + (Pow u)  = Pow $ scalarPow c + u
  (Scalar c)  + (Dec u)  = Pow $ scalarPow c + toPow u -- workaround scalarDec
  (Scalar c)  + (CRT u)  = CRT $ scalarCRT c + u
  (Scalar c1) + (Sub c2) = Sub $ Scalar c1 + c2 -- must re-wrap Scalar!

  (Pow u)  + (Scalar c)  = Pow $ u + scalarPow c
  (Dec u)  + (Scalar c)  = Pow $ toPow u + scalarPow c -- workaround scalarDec
  (CRT u)  + (Scalar c)  = CRT $ u + scalarCRT c
  (Sub c1) + (Scalar c2) = Sub $ c1 + Scalar c2

  -- SUB PLUS NON-SUB, NON-SCALAR: work in full ring
  -- EAC: would like to convert sub to basis of other before embedding
  (Sub c1) + c2 = embed' c1 + c2
  c1 + (Sub c2) = c1 + embed' c2

  -- mixed Dec and Pow: use linear-time conversions
  (Dec u1) + (Pow u2) = Pow $ toPow u1 + u2
  (Pow u1) + (Dec u2) = Pow $ u1 + toPow u2

  -- one CRT: convert other to CRT
  (CRT u1) + (Pow u2) = CRT $ u1 + toCRT u2
  (CRT u1) + (Dec u2) = CRT $ u1 + toCRT u2
  (Pow u1) + (CRT u2) = CRT $ toCRT u1 + u2
  (Dec u1) + (CRT u2) = CRT $ toCRT u1 + u2

  {-# INLINABLE negate #-}
  negate (Pow u)    = Pow $ negate u
  negate (Dec u)    = Dec $ negate u
  negate (CRT u)    = CRT $ negate u
  negate (Scalar c) = Scalar (negate c)
  negate (Sub c)    = Sub $ negate c

deriving instance Additive (CycG t m Double) => Additive.C (Cyc t m Double)
deriving instance Additive (CycG t m Int64) => Additive.C (Cyc t m Int64)
deriving instance Additive (CycG t m (ZqBasic q z)) => Additive.C (Cyc t m (ZqBasic q z))

instance (Additive (Cyc t m a), Additive (Cyc t m b))
  => Additive.C (Cyc t m (a,b)) where
  zero = CycPair zero zero
  (CycPair a b) + (CycPair a' b') = CycPair (a+a') (b+b')
  negate (CycPair a b) = CycPair (negate a) (negate b)

instance (Additive (RRq q r), TensorPowDec t (RRq q r), IFunctor t, Fact m)
  => Additive.C (Cyc t m (RRq q r)) where
  zero = PowRRq zero

  (PowRRq u1) + (PowRRq u2) = PowRRq $ u1 + u2
  (DecRRq u1) + (DecRRq u2) = DecRRq $ u1 + u2
  (PowRRq u1) + (DecRRq u2) = PowRRq $ u1 + toPow u2
  (DecRRq u1) + (PowRRq u2) = PowRRq $ toPow u1 + u2

  negate (PowRRq u) = PowRRq $ negate u
  negate (DecRRq u) = DecRRq $ negate u

-- ForallFact2 instances needed for RescaleCyc instance

instance (CRTElt t Int64) => ForallFact2 Additive.C (Cyc t) Int64 where
  entailFact2 = C.Sub Dict

instance (CRTElt t Double) => ForallFact2 Additive.C (Cyc t) Double where
  entailFact2 = C.Sub Dict

instance (CRTElt t (ZqBasic q z), ZeroTestable z)
  => ForallFact2 Additive.C (Cyc t) (ZqBasic q z) where
  entailFact2 = C.Sub Dict

instance (ForallFact2 Additive.C (Cyc t) a,
          ForallFact2 Additive.C (Cyc t) b)
  => ForallFact2 Additive.C (Cyc t) (a,b) where
  entailFact2 :: forall m . Fact m :- Additive.C (Cyc t m (a,b))
  entailFact2 = C.Sub (Dict
                       \\ (entailFact2 :: Fact m :- Additive.C (Cyc t m a))
                       \\ (entailFact2 :: Fact m :- Additive.C (Cyc t m b)))

-----

instance (Fact m, CRTElt t r, ZeroTestable r) => Ring.C (CycG t m r) where
  {-# INLINABLE one #-}
  one = Scalar one

  {-# INLINABLE fromInteger #-}
  fromInteger = Scalar . fromInteger

  {-# INLINABLE (*) #-}

  -- optimized mul-by-zero
  v1@(Scalar c1) * _ | isZero c1 = v1
  _ * v2@(Scalar c2) | isZero c2 = v2

  -- both CRT: if over C, then convert result to pow for precision reasons
  (CRT u1) * (CRT u2) = either (Pow . toPow) (CRT . Right) $ u1*u2

  -- at least one Scalar
  (Scalar c1) * (Scalar c2) = Scalar $ c1*c2
  (Scalar c) * (Pow u) = Pow $ c *> u
  (Scalar c) * (Dec u) = Dec $ c *> u
  (Scalar c) * (CRT u) = CRT $ c *> u
  (Scalar c1) * (Sub c2) = Sub $ Scalar c1 * c2

  (Pow u) * (Scalar c) = Pow $ c *> u
  (Dec u) * (Scalar c) = Dec $ c *> u
  (CRT u) * (Scalar c) = CRT $ c *> u
  (Sub c1) * (Scalar c2) = Sub $ c1 * Scalar c2

  -- TWO SUBS: work in a CRT rep for compositum
  (Sub (c1 :: CycG t m1 r)) * (Sub (c2 :: CycG t m2 r)) =
    -- re-wrap c1, c2 as Subs of the composition, and force them to CRT
    (Sub $ (toCRT' $ Sub c1 :: CycG t (FLCM m1 m2) r) * toCRT' (Sub c2))
    \\ lcm2Divides @m1 @m2 @m

  -- ELSE: work in appropriate CRT rep
  c1 * c2 = toCRT' c1 * toCRT' c2

deriving instance Ring (CycG t m Double)        => Ring.C (Cyc t m Double)
deriving instance Ring (CycG t m Int64)         => Ring.C (Cyc t m Int64)
deriving instance Ring (CycG t m (ZqBasic q z)) => Ring.C (Cyc t m (ZqBasic q z))

instance (Ring (Cyc t m a), Ring (Cyc t m b)) => Ring.C (Cyc t m (a,b)) where
  one = CycPair one one
  fromInteger z = CycPair (fromInteger z) (fromInteger z)
  (CycPair a b) * (CycPair a' b') = CycPair (a*a') (b*b')

-- no instance for RRq because it's not a ring

-- ForallFact2 instances in case they're useful

instance (CRTElt t Int64) => ForallFact2 Ring.C (Cyc t) Int64 where
  entailFact2 = C.Sub Dict

instance (CRTElt t Double) => ForallFact2 Ring.C (Cyc t) Double where
  entailFact2 = C.Sub Dict

instance (CRTElt t (ZqBasic q z), ZeroTestable z)
  => ForallFact2 Ring.C (Cyc t) (ZqBasic q z) where
  entailFact2 = C.Sub Dict

instance (ForallFact2 Ring.C (Cyc t) a,
          ForallFact2 Ring.C (Cyc t) b)
  => ForallFact2 Ring.C (Cyc t) (a,b) where
  entailFact2 :: forall m . Fact m :- Ring.C (Cyc t m (a,b))
  entailFact2 = C.Sub (Dict
                       \\ (entailFact2 :: Fact m :- Ring.C (Cyc t m a))
                       \\ (entailFact2 :: Fact m :- Ring.C (Cyc t m b)))

-----

instance (Fact m, CRTElt t r, ZeroTestable r) => Module.C r (CycG t m r) where
  r *> (Scalar c) = Scalar $ r * c
  r *> (Pow v)    = Pow $ r *> v
  r *> (Dec v)    = Dec $ r *> v
  r *> (Sub c)    = Sub $ r *> c
  r *> x          = r *> toPow' x

deriving instance Module Int64 (CycG t m Int64) => Module.C Int64 (Cyc t m Int64)
deriving instance Module Double (CycG t m Double) => Module.C Double (Cyc t m Double)
deriving instance (Module (ZqBasic q z) (CycG t m (ZqBasic q z)),
                   Ring (ZqBasic q z)) -- satisfy superclass
  => Module.C (ZqBasic q z) (Cyc t m (ZqBasic q z))

instance (Module a (Cyc t m a), Module b (Cyc t m b))
  => Module.C (a,b) (Cyc t m (a,b)) where
  (a,b) *> (CycPair ca cb) = CycPair (a *> ca) (b *> cb)

-- no instance for RRq because it's not, mathematically

-- ForallFact2 instances needed for special RescaleCyc instance

instance (CRTElt t Int64) => ForallFact2 (Module.C Int64) (Cyc t) Int64 where
  entailFact2 = C.Sub Dict

instance (CRTElt t Double) => ForallFact2 (Module.C Double) (Cyc t) Double where
  entailFact2 = C.Sub Dict

instance (CRTElt t (ZqBasic q z), ZeroTestable z)
  => ForallFact2 (Module.C (ZqBasic q z)) (Cyc t) (ZqBasic q z) where
  entailFact2 = C.Sub Dict

instance (ForallFact2 (Module.C a) (Cyc t) a,
          ForallFact2 (Module.C b) (Cyc t) b)
  => ForallFact2 (Module.C (a,b)) (Cyc t) (a,b) where
  entailFact2 :: forall m . Fact m :- Module.C (a,b) (Cyc t m (a,b))
  entailFact2 = C.Sub (Dict
                       \\ (entailFact2 :: Fact m :- Module.C a (Cyc t m a))
                       \\ (entailFact2 :: Fact m :- Module.C b (Cyc t m b)))

-- Module over finite field

-- | \(R_p\) is an \(\F_{p^d}\)-module when \(d\) divides
-- \(\varphi(m)\), by applying \(d\)-dimensional \(\F_p\)-linear
-- transform on \(d\)-dim chunks of powerful basis coeffs.
instance (GFCtx fp d, Fact m, CRTElt t fp, Module (GF fp d) (t m fp))
  => Module.C (GF fp d) (CycG t m fp) where
  -- CJP: optimize for Scalar if we can: r *> (Scalar c) is the tensor
  -- that has the coeffs of (r*c), followed by zeros.  (This assumes
  -- that the powerful basis has 1 as its first element, and that
  -- we're using pow to define the module mult.)

  -- Can use any r-basis to define module mult, but must be
  -- consistent. We use powerful basis.
  r *> (Pow v) = Pow $ r *> v
  r *> x = r *> toPow' x

deriving instance (Ring (GF (ZqBasic q z) d),
                   Module (GF (ZqBasic q z) d) (CycG t m (ZqBasic q z)))
  => Module.C (GF (ZqBasic q z) d) (Cyc t m (ZqBasic q z))

---------- Cyclotomic classes ----------

instance (Fact m, CRTElt t r, ZeroTestable r, IntegralDomain r)
  => Cyclotomic (CycG t m r) where
  mulG (Pow u)         = Pow $ R.mulGPow u
  mulG (Dec u)         = Dec $ R.mulGDec u
  mulG (CRT (Left u))  = Pow $ R.mulGPow $ toPow u -- go to Pow for precision
  mulG (CRT (Right u)) = CRT $ Right $ R.mulGCRTC u
  mulG c@(Scalar _)    = mulG $ toCRT' c
  mulG (Sub c)         = mulG $ embed' c   -- must go to full ring

  divG (Pow u)         = Pow <$> R.divGPow u
  divG (Dec u)         = Dec <$> R.divGDec u
  divG (CRT (Left u))  = Pow <$> R.divGPow (toPow u) -- go to Pow for precision
  divG (CRT (Right u)) = Just $ (CRT . Right) $ R.divGCRTC u
  divG c@(Scalar _)    = divG $ toCRT' c
  divG (Sub c)         = divG $ embed' c  -- must go to full ring

  advisePow = toPow'
  adviseDec = toDec'
  adviseCRT = toCRT'

-- CJP: can't derive instances here because Cyc isn't the *last*
-- argument of the class

instance Cyclotomic (CycG t m Double) => Cyclotomic (Cyc t m Double) where
  mulG      = CycDbl . mulG      . unCycDbl
  divG      = fmap CycDbl . divG . unCycDbl
  advisePow = CycDbl . advisePow . unCycDbl
  adviseDec = CycDbl . adviseDec . unCycDbl
  adviseCRT = CycDbl . adviseCRT . unCycDbl

instance Cyclotomic (CycG t m Int64) => Cyclotomic (Cyc t m Int64) where
  mulG      = CycI64 . mulG      . unCycI64
  divG      = fmap CycI64 . divG . unCycI64
  advisePow = CycI64 . advisePow . unCycI64
  adviseDec = CycI64 . adviseDec . unCycI64
  adviseCRT = CycI64 . adviseCRT . unCycI64

instance Cyclotomic (CycG t m (ZqBasic q z)) => Cyclotomic (Cyc t m (ZqBasic q z)) where
  mulG      = CycZqB . mulG      . unCycZqB
  divG      = fmap CycZqB . divG . unCycZqB
  advisePow = CycZqB . advisePow . unCycZqB
  adviseDec = CycZqB . adviseDec . unCycZqB
  adviseCRT = CycZqB . adviseCRT . unCycZqB

instance (Cyclotomic (Cyc t m a), Cyclotomic (Cyc t m b))
  => Cyclotomic (Cyc t m (a,b)) where
  mulG (CycPair a b) = CycPair (mulG a) (mulG b)
  divG (CycPair a b) = CycPair <$> divG a <*> divG b
  advisePow (CycPair a b) = CycPair (advisePow a) (advisePow b)
  adviseDec (CycPair a b) = CycPair (adviseDec a) (adviseDec b)
  adviseCRT (CycPair a b) = CycPair (adviseCRT a) (adviseCRT b)

-----

instance (Fact m, TensorGSqNorm t r, CRTElt t r)
  => GSqNormCyc (CycG t m) r where
  gSqNorm (Dec c) = gSqNormDec c
  gSqNorm c       = gSqNorm $ toDec' c

instance (Fact m, TensorGSqNorm t Double, CRTElt t Double) -- copied
  => GSqNormCyc (Cyc t m) Double where
  gSqNorm = gSqNorm . unCycDbl

instance (Fact m, TensorGSqNorm t Int64, CRTElt t Int64) -- copied
  => GSqNormCyc (Cyc t m) Int64 where
  gSqNorm = gSqNorm . unCycI64

-----

instance (Fact m, TensorGaussian t q) => GaussianCyc (CycG t m q) where
  tweakedGaussian = fmap Dec . R.tweakedGaussian

instance (Fact m, TensorGaussian t Double) => GaussianCyc (Cyc t m Double) where
  tweakedGaussian = fmap CycDbl . L.tweakedGaussian

-- CJP: no GaussianCyc for Int64, Integer, ZqBasic, pairs, or RRq

-- | uses 'Double' precision for the intermediate Gaussian samples
instance (TensorGaussian t Double, IFElt t Double, IFunctor t, Fact m, Mod zp,
          Lift zp (ModRep zp), CRTElt t zp, IFElt t (LiftOf zp))
  => CosetGaussianCyc (CycG t m zp) where
  {-# INLINABLE cosetGaussian #-}
  cosetGaussian v = (Dec <$>) . R.cosetGaussian v . unCycGDec

-- | uses 'Double' precision for the intermediate Gaussian samples
instance (CosetGaussianCyc (CycG t m (ZqBasic q Int64)))
  => CosetGaussianCyc (Cyc t m (ZqBasic q Int64)) where
  cosetGaussian v = fmap CycI64 . L.cosetGaussian v . unCycZqB

-- CJP: no CosetGaussianCyc for Double, Int64, Integer, or pairs

-----

instance (CRTElt t r, ZeroTestable r, IntegralDomain r) -- ZT, ID for superclass
  => ExtensionCyc (CycG t) r where

  -- lazily embed
  embed :: forall t m m' r . (m `Divides` m')
            => CycG t m r -> CycG t m' r
  embed (Scalar c) = Scalar c           -- keep as scalar
  embed (Sub (c :: CycG t l r)) = Sub c -- keep as subring element
    \\ transDivides @l @m @m'
  embed c = Sub c

  twace :: forall t m m' r .
          (CRTElt t r, ZeroTestable r, IntegralDomain r, m `Divides` m')
       => CycG t m' r -> CycG t m r
  twace (Pow u) = Pow $ twacePow u
  twace (Dec u) = Dec $ twaceDec u
  twace (CRT u) = either (cycPE . twaceCRTE) (cycPC . twaceCRTC) u
  twace (Scalar u) = Scalar u
  twace (Sub (c :: CycG t l r)) = Sub (twace c :: CycG t (FGCD l m) r)
                                  \\ gcdDivides @l @m

  powBasis :: forall m m' . (m `Divides` m') => Tagged m [CycG t m' r]
  powBasis = tag $ Pow <$> R.powBasis @m

  coeffsCyc L.Pow c' = Pow <$> R.coeffsPow (unCycGPow c')
  coeffsCyc L.Dec c' = Dec <$> R.coeffsDec (unCycGDec c')

instance ExtensionCyc (CycG t) Double => ExtensionCyc (Cyc t) Double where
  embed = CycDbl . embed . unCycDbl
  twace = CycDbl . twace . unCycDbl
  powBasis = (CycDbl <$>) <$> powBasis
  coeffsCyc b = fmap CycDbl . coeffsCyc b . unCycDbl

instance ExtensionCyc (CycG t) Int64 => ExtensionCyc (Cyc t) Int64 where
  embed = CycI64 . embed . unCycI64
  twace = CycI64 . twace . unCycI64
  powBasis = (CycI64 <$>) <$> powBasis
  coeffsCyc b = fmap CycI64 . coeffsCyc b . unCycI64

instance ExtensionCyc (CycG t) (ZqBasic q z) => ExtensionCyc (Cyc t) (ZqBasic q z) where
  embed = CycZqB . embed . unCycZqB
  twace = CycZqB . twace . unCycZqB
  powBasis = (CycZqB <$>) <$> powBasis
  coeffsCyc b = fmap CycZqB . coeffsCyc b . unCycZqB

instance (ExtensionCyc (Cyc t) a, ExtensionCyc (Cyc t) b)
  => ExtensionCyc (Cyc t) (a,b) where
  embed (CycPair a b) = CycPair (embed a) (embed b)
  twace (CycPair a b) = CycPair (twace a) (twace b)
  powBasis = zipWith CycPair <$> powBasis <*> powBasis
  coeffsCyc bas (CycPair a b) =
    zipWith CycPair (coeffsCyc bas a) (coeffsCyc bas b)

instance (TensorPowDec t (RRq q r)) => ExtensionCyc (Cyc t) (RRq q r) where
  embed (PowRRq u) = PowRRq $ embedPow u
  embed (DecRRq u) = PowRRq $ embedPow $ toPow u
  twace (PowRRq u) = PowRRq $ twacePow u
  twace (DecRRq u) = DecRRq $ twaceDec u
  powBasis :: forall m m' . (m `Divides` m') => Tagged m [Cyc t m' (RRq q r)]
  powBasis = tag $ PowRRq <$> R.powBasis @m
  coeffsCyc L.Pow (PowRRq c) = PowRRq <$> R.coeffsPow c
  coeffsCyc L.Dec (DecRRq c) = DecRRq <$> R.coeffsDec c
  coeffsCyc L.Pow (DecRRq c) = PowRRq <$> R.coeffsPow (toPow c)
  coeffsCyc L.Dec (PowRRq c) = DecRRq <$> R.coeffsDec (toDec c)

-- | Force to a non-'Sub' constructor (for internal use only).
embed' :: forall t r l m . (l `Divides` m, CRTElt t r)
       => CycG t l r -> CycG t m r
{-# INLINE embed' #-}
embed' (Pow u) = Pow $ embedPow u
embed' (Dec u) = Pow $ embedPow $ toPow u
embed' (CRT u) = either (cycPE . embedCRTE) (cycPC . embedCRTC) u
embed' (Scalar c) = Scalar c
embed' (Sub (c :: CycG t k r)) = embed' c \\ transDivides @k @l @m

-----

instance (ZPP r, CRTElt t r, TensorCRTSet t (ZpOf r), ExtensionCyc (CycG t) r)
  => CRTSetCyc (CycG t) r where
  crtSet :: forall m m' . (m `Divides` m') => Tagged m [CycG t m' r]
  crtSet = tag $ Pow <$> R.crtSet @m
  {-# INLINABLE crtSet #-}

instance (CRTSetCyc (CycG t) (ZqBasic q z))
  => CRTSetCyc (Cyc t) (ZqBasic q z) where
  crtSet = (CycZqB <$>) <$> crtSet

-- CJP TODO?: instance CRTSetCyc (Cyc t) (a,b)

---------- Promoted lattice operations ----------

-- | Rescales relative to the powerful basis. This instance is
-- provided for convenience, but usage of 'RescaleCyc' is preferred to
-- explicitly specify which basis by which to rescale.
instance (RescaleCyc (Cyc t m) a b, Fact m,
          Additive (Cyc t m a), Additive (Cyc t m b)) -- superclasses
 => Rescale (Cyc t m a) (Cyc t m b) where
  rescale = rescaleCyc L.Pow

-- CJP: can we avoid incoherent instances by changing instance heads
-- and using overlapping instances with isomorphism constraints?

instance {-# INCOHERENT #-} (Fact m, Rescale a b, CRTElt t a, TensorPowDec t b)
  => RescaleCyc (CycG t m) a b where
  -- Optimized for subring constructors, for powerful basis.
  -- Analogs for decoding basis are not quite correct, because (* -1)
  -- doesn't commute with 'rescale' due to tiebreakers!
  rescaleCyc L.Pow (Scalar c) = Scalar $ rescale c
  rescaleCyc L.Pow (Sub c)    = Sub $ rescalePow c

  rescaleCyc L.Pow c          = Pow $ fmapI rescale $ unCycGPow c
  rescaleCyc L.Dec c          = Dec $ fmapI rescale $ unCycGDec c
  {-# INLINABLE rescaleCyc #-}

-- | identity rescale
instance RescaleCyc (CycG t m) a a where
  -- No-op rescale
  rescaleCyc _ = id
  {-# INLINABLE rescaleCyc #-}

-- | rescale from one modulus to another
instance (RescaleCyc (CycG t m) (ZqBasic q z) (ZqBasic p z))
  => RescaleCyc (Cyc t m) (ZqBasic q z) (ZqBasic p z) where
  rescaleCyc b = CycZqB . rescaleCyc b . unCycZqB

-- | rescale from one modulus to another
instance (Fact m, Rescale (RRq q r) (RRq p r), TensorPowDec t (RRq q r), TensorPowDec t (RRq p r))
  => RescaleCyc (Cyc t m) (RRq q r) (RRq p r) where
  rescaleCyc L.Pow (PowRRq u) = PowRRq $ rescale u
  rescaleCyc L.Pow (DecRRq u) = PowRRq $ rescale $ toPow u
  rescaleCyc L.Dec (DecRRq u) = DecRRq $ rescale u
  rescaleCyc L.Dec (PowRRq u) = DecRRq $ rescale $ toDec u

-- | rescale up by one additional modulus
instance (Fact m, Reflects q z, Reduce z b, CRTElt t (ZqBasic q z), ZeroTestable z,
          Module.C b (Cyc t m b))
  => RescaleCyc (Cyc t m) b (ZqBasic q z, b) where

  rescaleCyc = let q :: z = value @q
               -- same method works for any basis
               in \_ b -> CycPair zero $ (reduce q :: b) *> b

-- | specialized (faster) rescale-down by a single \(\Z_q\)
instance (ToInteger z, Reflects q z, Reduce z b, Field b,
          FunctorCyc (Cyc t m) (ZqBasic q z) z,
          FunctorCyc (Cyc t m) z b,
          Additive (Cyc t m b), Module b (Cyc t m b))
  => RescaleCyc (Cyc t m) (ZqBasic q z, b) b where

  rescaleCyc bas (CycPair a b) =
    let q :: z = value @q
        x      = liftCyc (Just bas) a
    in recip (reduce q :: b) *> (b - reduceCyc x)

-- CJP: do we really need these? Just have client call rescaleCyc
-- multiple times?

-- | convenient rescale-down by multiple components at once
instance (RescaleCyc (Cyc t m) (b,c) c, RescaleCyc (Cyc t m) (a,(b,c)) (b,c))
         => RescaleCyc (Cyc t m) (a,(b,c)) c where

  rescaleCyc bas a = rescaleCyc bas (rescaleCyc bas a :: Cyc t m (b,c))
  {-# INLINABLE rescaleCyc #-}

-- | convenient rescale-down by multiple components at once
instance (RescaleCyc (Cyc t m) (b,(c,d)) d,
          RescaleCyc (Cyc t m) (a,(b,(c,d))) (b,(c,d)))
         => RescaleCyc (Cyc t m) (a,(b,(c,d))) d where

  rescaleCyc bas a = rescaleCyc bas (rescaleCyc bas a :: Cyc t m (b,(c,d)))
  {-# INLINABLE rescaleCyc #-}

-- | convenient rescale-down by multiple components at once
instance (RescaleCyc (Cyc t m) (b,(c,(d,e))) e,
          RescaleCyc (Cyc t m) (a,(b,(c,(d,e)))) (b,(c,(d,e))))
         => RescaleCyc (Cyc t m) (a,(b,(c,(d,e)))) e where

  rescaleCyc bas a = rescaleCyc bas (rescaleCyc bas a :: Cyc t m (b,(c,(d,e))))
  {-# INLINABLE rescaleCyc #-}

-----

-- | promoted from base ring
instance (Gadget gad (ZqBasic q z),
          -- satisfy Gadget's Ring superclass; remove if it goes away
          Fact m, CRTElt t (ZqBasic q z),
          ZeroTestable (ZqBasic q z), IntegralDomain (ZqBasic q z))
  => Gadget gad (CycG t m (ZqBasic q z)) where
  gadget = Scalar <$> gadget @gad
  {-# INLINABLE gadget #-}
  -- CJP: default 'encode' works because mul-by-Scalar is fast

-- can't auto-derive because of ambiguity of gadget
instance Gadget gad (CycG t m (ZqBasic q z))
  => Gadget gad (Cyc t m (ZqBasic q z)) where
  gadget = CycZqB <$> gadget @gad

instance (Gadget gad (Cyc t m a), Gadget gad (Cyc t m b))
  => Gadget gad (Cyc t m (a,b)) where
  gadget = (++) (flip CycPair zero <$> gadget @gad)
                (     CycPair zero <$> gadget @gad)

-- ForallFact2 in case they're useful

instance (Gadget gad (ZqBasic q z),
          -- remove these if they go away from above
          CRTElt t (ZqBasic q z), ZeroTestable (ZqBasic q z),
          IntegralDomain (ZqBasic q z))
  => ForallFact2 (Gadget gad) (Cyc t) (ZqBasic q z) where
  entailFact2 = C.Sub Dict

instance (ForallFact2 (Gadget gad) (Cyc t) a,
          ForallFact2 (Gadget gad) (Cyc t) b)
  => ForallFact2 (Gadget gad) (Cyc t) (a,b) where
  -- bring m into scope
  entailFact2 :: forall m . Fact m :- Gadget gad (Cyc t m (a,b))
  entailFact2 = C.Sub (Dict
                       \\ (entailFact2 :: Fact m :- Gadget gad (Cyc t m a))
                       \\ (entailFact2 :: Fact m :- Gadget gad (Cyc t m b)))

-----

instance (Fact m, Reduce a b, CRTElt t a, TensorPowDec t b)
  => Reduce (CycG t m a) (CycG t m b) where
  reduce (Pow u)                 = Pow    $ reduce u
  reduce (Dec u)                 = Dec    $ reduce u
  reduce (CRT u)                 = Pow    $ reduce $ either toPow toPow u
  reduce (Scalar c)              = Scalar $ reduce c
  reduce (Sub (c :: CycG t l a)) = Sub (reduce c :: CycG t l b)

instance (Reduce (CycG t m Int64) (CycG t m (ZqBasic q Int64)))
  => Reduce (Cyc t m Int64) (Cyc t m (ZqBasic q Int64)) where
  reduce = CycZqB . reduce . unCycI64

instance (Fact m, Reflects q Int64, ForallFact1 Applicative t)
  => Reduce (Cyc t m Integer) (Cyc t m (ZqBasic q Int64)) where
  reduce (PowIgr u) = CycZqB $ Pow $ fmap reduce u
  reduce (DecIgr u) = CycZqB $ Dec $ fmap reduce u

instance (Reflects q Double, FunctorCyc (Cyc t m) Double (RRq q Double))
  => Reduce (Cyc t m Double) (Cyc t m (RRq q Double)) where
  reduce = fmapCyc Nothing reduce

instance (Reduce (Cyc t m z) (Cyc t m a), Reduce (Cyc t m z) (Cyc t m b))
  => Reduce (Cyc t m z) (Cyc t m (a,b)) where
  reduce z = CycPair (reduce z) (reduce z)

-- | promoted from base ring, using the powerful basis for best geometry
instance (Decompose gad (ZqBasic q z), CRTElt t (ZqBasic q z), Fact m,
          -- for satisfying Decompose's Gadget superclass
          ZeroTestable (ZqBasic q z), IntegralDomain (ZqBasic q z),
          -- for satisfying Decompose's Reduce superclass w/o using m
          CRTElt t z, ZeroTestable z)
         => Decompose gad (CycG t m (ZqBasic q z)) where

  type DecompOf (CycG t m (ZqBasic q z)) = CycG t m z

  -- faster implementations: decompose directly in subring, which is
  -- correct because we decompose in powerful basis
  decompose (Scalar c) = Scalar <$> decompose @gad c
  decompose (Sub c) = Sub <$> decompose @gad c

  -- traverse: Traversable (CycRep t P m) and Applicative ZipList
  decompose (Pow u) = getZipList $ Pow <$> traverse (ZipList . decompose @gad) u
  decompose c = decompose @gad $ toPow' c

  {-# INLINABLE decompose #-}

-- specific to Int64 because we need to know constructor for lift type
instance (Decompose gad (CycG t m (ZqBasic q Int64)),
          -- for satisfying Decompose's Reduce superclass
          Reduce (Cyc t m Int64) (Cyc t m (ZqBasic q Int64)))
  => Decompose gad (Cyc t m (ZqBasic q Int64)) where

  type DecompOf (Cyc t m (ZqBasic q Int64)) = Cyc t m Int64
  decompose (CycZqB c) = CycI64 <$> decompose @gad c

instance (Decompose gad (Cyc t m a), Decompose gad (Cyc t m b),
         DecompOf (Cyc t m a) ~ DecompOf (Cyc t m b),
         -- for satisfying Decompose's Reduce superclass
         Reduce (DecompOf (Cyc t m a)) (Cyc t m (a, b)))
  => Decompose gad (Cyc t m (a,b)) where
  type DecompOf (Cyc t m (a,b)) = DecompOf (Cyc t m a)
  decompose (CycPair a b) = (++) (decompose @gad a) (decompose @gad b)

-----

-- | promoted from base ring, using the decoding basis for best geometry
instance (Correct gad (ZqBasic q z), CRTElt t (ZqBasic q z), Fact m,
          -- satisfy Gadget superclass
          ZeroTestable (ZqBasic q z), IntegralDomain (ZqBasic q z),
          Traversable (CycRep t D m))
  => Correct gad (CycG t m (ZqBasic q z)) where
  -- sequence: Monad [] and Traversable (CycRep t D m)
  -- sequenceA: Applicative (CycRep t D m) and Traversable (TaggedT gad [])
  correct bs = Dec *** (Dec <$>) $
               second sequence $ fmap fst &&& fmap snd $ correct @gad <$>
               sequenceA (unCycGDec <$> bs)
  {-# INLINABLE correct #-}

-- specific to Int64 due to LiftOf. Can't auto-derive because of
-- ambiguity of 'correct'
instance Correct gad (CycG t m (ZqBasic q Int64))
  => Correct gad (Cyc t m (ZqBasic q Int64)) where

  correct = (CycZqB *** fmap CycI64) . correct @gad . fmap unCycZqB
  -- correct = coerce $
  --   (correct @gad :: [CycG t m (ZqBasic q Int64)]
  --                 -> (CycG t m (ZqBasic q Int64), [CycG t m Int64]))

-- TODO: instance Correct gad (Cyc t m (a,b)) where
-- seems hard; see Correct instance for pairs in Gadget.hs


-- no ForallFact2 instance due to Traversable (CycRep t D m)
-- constraint in Correct instance, but maybe that can be replaced

---------- Change of representation (internal use only) ----------

toPow', toDec', toCRT' :: (Fact m, CRTElt t r) => CycG t m r -> CycG t m r
{-# INLINABLE toPow' #-}
{-# INLINABLE toDec' #-}
{-# INLINABLE toCRT' #-}

-- | Force to powerful-basis representation (for internal use only).
toPow' c@(Pow _)  = c
toPow' (Dec u)    = Pow $ toPow u
toPow' (CRT u)    = Pow $ either toPow toPow u
toPow' (Scalar c) = Pow $ scalarPow c
toPow' (Sub c)    = toPow' $ embed' c

-- | Force to decoding-basis representation (for internal use only).
toDec' (Pow u)    = Dec $ toDec u
toDec' c@(Dec _)  = c
toDec' (CRT u)    = Dec $ either toDec toDec u
toDec' (Scalar c) = Dec $ toDec $ scalarPow c
toDec' (Sub c)    = toDec' $ embed' c

-- | Force to a CRT representation (for internal use only).
toCRT' (Pow u)    = CRT $ toCRT u
toCRT' (Dec u)    = CRT $ toCRT u
toCRT' c@(CRT _)  = c
toCRT' (Scalar c) = CRT $ scalarCRT c
-- CJP: the following is the fastest algorithm for when both source
-- and target have the same CRTr/CRTe choice.  It is not the fastest
-- when the choices are different (it will do an unnecessary CRT if
-- input is non-CRT), but this is an unusual case.  Note: both calls
-- to toCRT' are necessary in general, because embed' may not preserve
-- CRT representation!
toCRT' (Sub c)    = toCRT' $ embed' $ toCRT' c

---------- Utility instances ----------

instance (Fact m, ForallFact2 Random t r, CRTElt t r) => Random (CycG t m r) where
  random g = let (u,g') = random g
             in (either Pow (CRT . Right) u, g')
  {-# INLINABLE random #-}

  randomR _ = error "randomR non-sensical for CycG"

deriving instance Random (CycG t m Double)        => Random (Cyc t m Double)
deriving instance Random (CycG t m Int64)         => Random (Cyc t m Int64)
deriving instance Random (CycG t m (ZqBasic q z)) => Random (Cyc t m (ZqBasic q z))

instance (Random (Cyc t m a), Random (Cyc t m b)) => Random (Cyc t m (a,b)) where
  random g = let (a,g') = random g
                 (b,g'') = random g'
                 in (CycPair a b, g'')
  randomR _ = error "randomR non-sensical for Cyc"

instance (Fact m, ForallFact2 Random t Integer) => Random (Cyc t m Integer) where
  random g = let (u,g') = random g in (PowIgr u, g')
  randomR = error "randomR nonsensical for Cyc over Integer"

instance (Fact m, ForallFact2 Random t (RRq q r))
  => Random (Cyc t m (RRq q r)) where
  random g = let (u,g') = random g in (PowRRq u, g')
  randomR = error "randomR nonsensical for Cyc over (RRq q r)"

-----

instance (Fact m, ForallFact2 Show t r, ForallFact2 Show t (CRTExt r), Show r)
  => Show (CycG t m r) where
  show (Pow x)         = "Cyc.Pow " ++ show x
  show (Dec x)         = "Cyc.Dec " ++ show x
  show (CRT (Left x))  = "Cyc.CRT " ++ show x
  show (CRT (Right x)) = "Cyc.CRT " ++ show x
  show (Scalar x)      = "Cyc.Scalar " ++ show x
  show (Sub x)         = "Cyc.Sub " ++ show x

deriving instance Show (CycG t m Double)        => Show (Cyc t m Double)
deriving instance Show (CycG t m Int64)         => Show (Cyc t m Int64)
deriving instance Show (CycG t m (ZqBasic q z)) => Show (Cyc t m (ZqBasic q z))
deriving instance (Show (Cyc t m a), Show (Cyc t m b))
                  => Show (Cyc t m (a,b))

deriving instance (Fact m, ForallFact2 Show t Integer)   => Show (Cyc t m Integer)
deriving instance (Fact m, ForallFact2 Show t (RRq q r)) => Show (Cyc t m (RRq q r))

-----

instance (Fact m, NFData r,
          ForallFact2 NFData t r, ForallFact2 NFData t (CRTExt r))
         => NFData (CycG t m r) where
  rnf (Pow u)    = rnf u
  rnf (Dec u)    = rnf u
  rnf (CRT u)    = rnf u
  rnf (Scalar u) = rnf u
  rnf (Sub c)    = rnf c

deriving instance NFData (CycG t m Double)        => NFData (Cyc t m Double)
deriving instance NFData (CycG t m Int64)         => NFData (Cyc t m Int64)
deriving instance NFData (CycG t m (ZqBasic q z)) => NFData (Cyc t m (ZqBasic q z))

instance (NFData (Cyc t m a), NFData (Cyc t m b)) => NFData (Cyc t m (a,b)) where
  rnf (CycPair a b) = rnf a `seq` rnf b

instance (Fact m, ForallFact2 NFData t Integer) => NFData (Cyc t m Integer) where
  rnf (PowIgr u) = rnf u
  rnf (DecIgr u) = rnf u

instance (Fact m, ForallFact2 NFData t (RRq q r))
  => NFData (Cyc t m (RRq q r)) where
  rnf (PowRRq u) = rnf u
  rnf (DecRRq u) = rnf u

---------- Protoable instances of Cyc/CycG ----------

instance (Fact m, CRTElt t r, Protoable (CycRep t D m r))
    => Protoable (CycG t m r) where
  type ProtoType (CycG t m r) = ProtoType (CycRep t D m r)
  toProto (Dec uc) = toProto uc
  toProto x        = toProto $ toDec' x
  fromProto x = Dec <$> fromProto x

instance (Fact m, CRTElt t Double, Protoable (CycG t m Double))
    => Protoable (Cyc t m Double) where
  type ProtoType (Cyc t m Double) = ProtoType (CycG t m Double)
  toProto = toProto . unCycDbl
  fromProto x = CycDbl <$> fromProto x

instance (Fact m, CRTElt t Int64, Protoable (CycG t m Int64))
    => Protoable (Cyc t m Int64) where
  type ProtoType (Cyc t m Int64) = ProtoType (CycG t m Int64)
  toProto = toProto . unCycI64
  fromProto x = CycI64 <$> fromProto x

instance (Fact m, CRTElt t Double, Protoable (CycG t m (ZqBasic q z)))
    => Protoable (Cyc t m (ZqBasic q z)) where
  type ProtoType (Cyc t m (ZqBasic q z)) = ProtoType (CycG t m (ZqBasic q z))
  toProto = toProto . unCycZqB
  fromProto x = CycZqB <$> fromProto x

instance (Fact m, CRTElt t Double, TensorPowDec t (RRq q Double),
          Protoable (CycRep t D m (RRq q Double)))
    => Protoable (Cyc t m (RRq q Double)) where
  type ProtoType (Cyc t m (RRq q Double)) = ProtoType (CycG t m (RRq q Double))
  toProto (PowRRq x) = toProto $ toDec x
  toProto (DecRRq x) = toProto x
  fromProto x = DecRRq <$> fromProto x

---------- TH instances of FunctorCyc ----------

-- CJP: the TH needs to appear before/after everything in the module
-- so as not to screw up scoping

let types = [ [t| Int64 |]
            , [t| Double |]
            , [t| ZqBasic $(varT (mkName "q")) $(varT (mkName "z")) |]
            , [t| RRq $(varT (mkName "q")) $(varT (mkName "r")) |]
            , [t| ( $(varT (mkName "a")) , $(varT (mkName "b"))) |] -- pair
            ]
    -- Instances that rely on IFunctor (in practice, Storable base
    -- types), and go between any two IFElt types.
    mkIFunctorCyc y z =
      [d|
       instance (Fact m, UnCyc t $y, UnCyc t $z,
                 IFunctor t, IFElt t $y, IFElt t $z)
         => FunctorCyc (Cyc t m) $y $z where
         fmapCyc (Just L.Pow) f = cycPow . fmapI f . unCycPow
         fmapCyc (Just L.Dec) f = cycDec . fmapI f . unCycDec
         fmapCyc Nothing      f = fmapCyc (Just L.Pow) f
       |]
    -- Instances that map to Integer, hence need to use fmap.
    mkFunctorCyc y =
      [d|
       instance (Fact m, ForallFact1 Applicative t, UnCyc t $y)
         => FunctorCyc (Cyc t m) $y Integer where
         fmapCyc (Just L.Pow) f = PowIgr . fmap f . unCycPow
         fmapCyc (Just L.Dec) f = DecIgr . fmap f . unCycDec
         fmapCyc Nothing      f = fmapCyc (Just L.Pow) f
       |]
    -- CJP TODO: if/when we get a way to convert Integer between Pow
    -- and Dec, we can also have instances that go *from* Integer
  in liftA concat $ sequence $
     (mkIFunctorCyc <$> types <*> types) ++ (mkFunctorCyc <$> types)
