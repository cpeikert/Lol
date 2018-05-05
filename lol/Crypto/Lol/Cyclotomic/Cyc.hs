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

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Cyclotomic.Cyc
(
-- * Data type
  Cyc
-- * Constructors/deconstructors
, scalarCyc
, cycPow, cycDec, cycCRT, cycCRTC, cycCRTE, cycPC, cycPE
, uncycPow, uncycDec, uncycCRT
) where

import qualified Algebra.Additive     as Additive (C)
import qualified Algebra.Module       as Module (C)
import qualified Algebra.Ring         as Ring (C)
import qualified Algebra.ZeroTestable as ZeroTestable (C)

-- hide these due to name collisions
import Crypto.Lol.Cyclotomic.CycRep hiding (coeffsDec, coeffsPow, crtSet,
                                     gSqNorm, mulG, powBasis)

import           Crypto.Lol.CRTrans
import qualified Crypto.Lol.Cyclotomic.CycRep   as R
import           Crypto.Lol.Cyclotomic.Language hiding (Dec, Pow)
import qualified Crypto.Lol.Cyclotomic.Language as L
import           Crypto.Lol.Cyclotomic.Tensor   (Tensor, TensorCRTSet,
                                                 TensorGaussian)
import           Crypto.Lol.Gadget
import           Crypto.Lol.Prelude             as LP
import           Crypto.Lol.Types               (ZqBasic)
import           Crypto.Lol.Types.FiniteField
import           Crypto.Lol.Types.IFunctor
import           Crypto.Lol.Types.Proto
import           Crypto.Lol.Types.ZPP

import Control.Applicative    hiding ((*>))
import Control.Arrow
import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.Random   hiding (lift)
import Data.Coerce
import Data.Constraint        ((:-), (\\))
import Data.Foldable
import Data.Traversable

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
  -- CJP: someday try to merge the above two

-- | A cyclotomic ring such as \( \Z[\zeta_m] \), \( \Z_q[\zeta_m] \),
-- or \( \Q[\zeta_m] \): @t@ is the 'Tensor' type for storing
-- coefficient tensors; @m@ is the cyclotomic index; @r@ is the base
-- ring of the coefficients (e.g., \(\ \Q \), \( \Z \), \( \Z_q \)).
data family Cyc (t :: Factored -> * -> *) (m :: Factored) r

newtype instance Cyc t m Double        = CycDbl { unCycDbl :: CycG t m Double }
newtype instance Cyc t m Int64         = CycI64 { unCycI64 :: CycG t m Int64 }
-- could also do an Int instance
newtype instance Cyc t m (ZqBasic q z) = CycZqB { unCycZqB :: CycG t m (ZqBasic q z) }
data    instance Cyc t m (a,b)         = CycPair !(Cyc t m a) !(Cyc t m b)

---------- Constructors / deconstructors ----------

-- | Wrap a 'CycRep' as a 'Cyc'.
cycPow :: CycRep t P m r -> CycG t m r
cycPow = Pow
{-# INLINABLE cycPow #-}

-- | Wrap a 'CycRep' as a 'Cyc'.
cycDec :: CycRep t D m r -> CycG t m r
cycDec = Dec
{-# INLINABLE cycDec #-}

-- | Wrap a 'CycRepEC' as a 'Cyc'.
cycCRT :: CycRepEC t m r -> CycG t m r
cycCRT = CRT
{-# INLINABLE cycCRT #-}

-- | Wrap a 'CycRep' as a 'Cyc'.
cycCRTC :: CycRep t C m r -> CycG t m r
cycCRTC = CRT . Right
{-# INLINABLE cycCRTC #-}

-- | Wrap a 'CycRep' as a 'Cyc'.
cycCRTE :: CycRep t E m r -> CycG t m r
cycCRTE = CRT . Left
{-# INLINABLE cycCRTE #-}

-- | Convenience wrapper.
cycPC :: Either (CycRep t P m r) (CycRep t C m r) -> CycG t m r
cycPC = either Pow (CRT . Right)
{-# INLINABLE cycPC #-}

-- | Convenience wrapper.
cycPE :: Either (CycRep t P m r) (CycRep t E m r) -> CycG t m r
cycPE = either Pow (CRT . Left)
{-# INLINABLE cycPE #-}

-- | Unwrap a 'Cyc' as a 'CycRep' in powerful-basis representation.
uncycPow :: (Fact m, CRTElt t r) => CycG t m r -> CycRep t P m r
{-# INLINABLE uncycPow #-}
uncycPow c = let (Pow u) = toPow' c in u

-- | Unwrap a 'Cyc' as a 'CycRep' in decoding-basis representation.
uncycDec :: (Fact m, CRTElt t r) => CycG t m r -> CycRep t D m r
{-# INLINABLE uncycDec #-}
uncycDec c = let (Dec u) = toDec' c in u

-- | Unwrap a 'Cyc' as a 'CycRep' in a CRT-basis representation.
uncycCRT :: (Fact m, CRTElt t r) => CycG t m r -> CycRepEC t m r
{-# INLINABLE uncycCRT #-}
uncycCRT c = let (CRT u) = toCRT' c in u

---------- Algebraic instances ----------

instance (Fact m, ZeroTestable r, CRTElt t r, ForallFact2 ZeroTestable.C t r)
  => ZeroTestable.C (CycG t m r) where
  isZero = \x -> case x of
                   (Pow u) -> isZero u
                   (Dec u) -> isZero u
                   (CRT (Right u)) -> isZero u
                   c@(CRT _) -> isZero $ toPow' c
                   (Scalar c) -> isZero c
                   (Sub c) -> isZero c
                 \\ (entailFact2 :: Fact m :- ZeroTestable.C (t m r))
  {-# INLINABLE isZero #-}

deriving instance ZeroTestable (CycG t m Double) => ZeroTestable.C (Cyc t m Double)
deriving instance ZeroTestable (CycG t m Int64) => ZeroTestable.C (Cyc t m Int64)
deriving instance ZeroTestable (CycG t m (ZqBasic q z)) => ZeroTestable.C (Cyc t m (ZqBasic q z))

instance (ZeroTestable (Cyc t m a), ZeroTestable (Cyc t m b))
  => ZeroTestable.C (Cyc t m (a,b)) where
  isZero (CycPair a b) = isZero a && isZero b

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
    \\ lcmDivides (Proxy::Proxy l1) (Proxy::Proxy l2)

  -- some other relatively efficient comparisons
  (Scalar c1) == (Pow u2) = scalarPow c1 == u2
                            \\ (entailFact2 :: Fact m :- Eq (t m r))
  (Pow u1) == (Scalar c2) = u1 == scalarPow c2
                            \\ (entailFact2 :: Fact m :- Eq (t m r))

  -- otherwise: compare in powerful basis
  c1 == c2 = toPow' c1 == toPow' c2

deriving instance Eq (CycG t m Double) => Eq (Cyc t m Double)
deriving instance Eq (CycG t m Int64) => Eq (Cyc t m Int64)
deriving instance Eq (CycG t m (ZqBasic q z)) => Eq (Cyc t m (ZqBasic q z))

instance (Eq (Cyc t m a), Eq (Cyc t m b)) => Eq (Cyc t m (a,b)) where
  (CycPair a b) == (CycPair a' b') = a == a' && b == b'

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
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- SCALAR PLUS SOMETHING ELSE

  (Scalar c) + (Pow u) = Pow $ scalarPow c + u
  (Scalar c) + (Dec u) = Pow $ scalarPow c + toPow u -- workaround scalarDec
  (Scalar c) + (CRT u) = CRT $ scalarCRT c + u
  (Scalar c1) + (Sub c2) = Sub $ Scalar c1 + c2 -- must re-wrap Scalar!

  (Pow u) + (Scalar c) = Pow $ u + scalarPow c
  (Dec u) + (Scalar c) = Pow $ toPow u + scalarPow c -- workaround scalarDec
  (CRT u) + (Scalar c) = CRT $ u + scalarCRT c
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
  negate (Pow u) = Pow $ negate u
  negate (Dec u) = Dec $ negate u
  negate (CRT u) = CRT $ negate u
  negate (Scalar c) = Scalar (negate c)
  negate (Sub c) = Sub $ negate c

deriving instance Additive (CycG t m Double) => Additive.C (Cyc t m Double)
deriving instance Additive (CycG t m Int64) => Additive.C (Cyc t m Int64)
deriving instance Additive (CycG t m (ZqBasic q z)) => Additive.C (Cyc t m (ZqBasic q z))

instance (Additive (Cyc t m a), Additive (Cyc t m b))
  => Additive.C (Cyc t m (a,b)) where
  zero = CycPair zero zero
  (CycPair a b) + (CycPair a' b') = CycPair (a+a') (b+b')
  negate (CycPair a b) = CycPair (negate a) (negate b)

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
    \\ lcm2Divides (Proxy::Proxy m1) (Proxy::Proxy m2) (Proxy::Proxy m)

  -- ELSE: work in appropriate CRT rep
  c1 * c2 = toCRT' c1 * toCRT' c2

deriving instance Ring (CycG t m Double) => Ring.C (Cyc t m Double)
deriving instance Ring (CycG t m Int64) => Ring.C (Cyc t m Int64)
deriving instance Ring (CycG t m (ZqBasic q z)) => Ring.C (Cyc t m (ZqBasic q z))

instance (Ring (Cyc t m a), Ring (Cyc t m b)) => Ring.C (Cyc t m (a,b)) where
  one = CycPair one one
  fromInteger z = CycPair (fromInteger z) (fromInteger z)
  (CycPair a b) * (CycPair a' b') = CycPair (a*a') (b*b')

-----

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
  r *> (Pow v) = Pow $ r LP.*> v
  r *> x = r *> toPow' x

deriving instance (Ring (GF (ZqBasic q z) d),
                   Module (GF (ZqBasic q z) d) (CycG t m (ZqBasic q z)))
  => Module.C (GF (ZqBasic q z) d) (Cyc t m (ZqBasic q z))

---------- Cyclotomic classes ----------

instance (CRTElt t r, ZeroTestable r, IntegralDomain r)
  => Cyclotomic (CycG t) r where
  scalarCyc = Scalar

  mulG (Pow u) = Pow $ R.mulGPow u
  mulG (Dec u) = Dec $ R.mulGDec u
  mulG (CRT (Left u)) = Pow $ R.mulGPow $ R.toPow u -- go to Pow for precision
  mulG (CRT (Right u)) = CRT $ Right $ R.mulGCRTC u
  mulG c@(Scalar _) = mulG $ toCRT' c
  mulG (Sub c) = mulG $ embed' c   -- must go to full ring

  divG (Pow u) = Pow <$> R.divGPow u
  divG (Dec u) = Dec <$> R.divGDec u
  divG (CRT (Left u)) = Pow <$> R.divGPow (R.toPow u) -- go to Pow for precision
  divG (CRT (Right u)) = Just $ (CRT . Right) $ R.divGCRTC u
  divG c@(Scalar _) = divG $ toCRT' c
  divG (Sub c) = divG $ embed' c  -- must go to full ring

  advisePow = toPow'
  adviseDec = toDec'
  adviseCRT = toCRT'

-- CJP: can't derive instances here because Cyc isn't the *last*
-- argument of the class

instance Cyclotomic (CycG t) Double => Cyclotomic (Cyc t) Double where
  scalarCyc = CycDbl . scalarCyc
  mulG      = CycDbl . mulG      . unCycDbl
  divG      = fmap CycDbl . divG . unCycDbl
  advisePow = CycDbl . advisePow . unCycDbl
  adviseDec = CycDbl . adviseDec . unCycDbl
  adviseCRT = CycDbl . adviseCRT . unCycDbl

instance Cyclotomic (CycG t) Int64 => Cyclotomic (Cyc t) Int64 where
  scalarCyc = CycI64 . scalarCyc
  mulG      = CycI64 . mulG      . unCycI64
  divG      = fmap CycI64 . divG . unCycI64
  advisePow = CycI64 . advisePow . unCycI64
  adviseDec = CycI64 . adviseDec . unCycI64
  adviseCRT = CycI64 . adviseCRT . unCycI64

instance Cyclotomic (CycG t) (ZqBasic q z) => Cyclotomic (Cyc t) (ZqBasic q z) where
  scalarCyc = CycZqB . scalarCyc
  mulG      = CycZqB . mulG      . unCycZqB
  divG      = fmap CycZqB . divG . unCycZqB
  advisePow = CycZqB . advisePow . unCycZqB
  adviseDec = CycZqB . adviseDec . unCycZqB
  adviseCRT = CycZqB . adviseCRT . unCycZqB

instance (Cyclotomic (Cyc t) a, Cyclotomic (Cyc t) b) => Cyclotomic (Cyc t) (a,b) where
  scalarCyc (a,b) = CycPair (scalarCyc a) (scalarCyc b)
  mulG (CycPair a b) = CycPair (mulG a) (mulG b)
  divG (CycPair a b) = CycPair <$> divG a <*> divG b
  advisePow (CycPair a b) = CycPair (advisePow a) (advisePow b)
  adviseDec (CycPair a b) = CycPair (adviseDec a) (adviseDec b)
  adviseCRT (CycPair a b) = CycPair (adviseCRT a) (adviseCRT b)

-----

instance (GSqNorm (CycG t) Double) => GSqNorm (Cyc t) Double where
  gSqNorm = gSqNorm . unCycDbl

instance (GSqNorm (CycG t) Int64) => GSqNorm (Cyc t) Int64 where
  gSqNorm = gSqNorm . unCycI64

-----

instance TensorGaussian t q => GaussianCyc (CycG t) q where
  tweakedGaussian = fmap Dec . R.tweakedGaussian

instance GaussianCyc (CycG t) Double => GaussianCyc (Cyc t) Double where
  tweakedGaussian = fmap CycDbl . L.tweakedGaussian

-- CJP: no GaussianCyc for Int64, ZqBasic, or pairs

-- | uses 'Double' precision for the intermediate Gaussian samples
instance (TensorGaussian t Double, IFElt t Double, IFunctor t, ToInteger z, IFElt t z)
  => RoundedGaussianCyc (CycG t) z where
  {-# INLINABLE roundedGaussian #-}
  roundedGaussian = fmap Dec . R.roundedGaussian

instance RoundedGaussianCyc (CycG t) Int64 => RoundedGaussianCyc (Cyc t) Int64 where
  roundedGaussian = fmap CycI64 . L.roundedGaussian

-- CJP: no RoundedGaussianCyc for Double, ZqBasic, or pairs

-- | uses 'Double' precision for the intermediate Gaussian samples
instance (TensorGaussian t Double, IFElt t Double, IFunctor t, Mod zp,
          Lift zp (ModRep zp), CRTElt t zp, IFElt t (LiftOf zp))
  => CosetGaussianCyc (CycG t) zp where
  {-# INLINABLE cosetGaussian #-}
  cosetGaussian v = (Dec <$>) . R.cosetGaussian v . uncycDec

instance (CosetGaussianCyc (CycG t) (ZqBasic q Int64))
  => CosetGaussianCyc (Cyc t) (ZqBasic q Int64) where
  cosetGaussian v = fmap CycI64 . L.cosetGaussian v . unCycZqB

-- CJP: no CosetGaussianCyc for Double, Int64, or pairs

-----

instance (CRTElt t r, ZeroTestable r, IntegralDomain r) -- ZT, ID for superclass
  => ExtensionCyc (CycG t) r where
  -- use these because implementations need indices to be in scope
  embed = embedLazy
  twace = twace'

  powBasis = (Pow <$>) <$> R.powBasis

  coeffsCyc L.Pow c' = Pow <$> R.coeffsPow (uncycPow c')
  coeffsCyc L.Dec c' = Dec <$> R.coeffsDec (uncycDec c')

instance ExtensionCyc (CycG t) Double => ExtensionCyc (Cyc t) Double where
  embed = CycDbl . embed . unCycDbl
  twace = CycDbl . twace . unCycDbl
  powBasis = fmap CycDbl <$> powBasis
  coeffsCyc b = fmap CycDbl . coeffsCyc b . unCycDbl

instance ExtensionCyc (CycG t) Int64 => ExtensionCyc (Cyc t) Int64 where
  embed = CycI64 . embed . unCycI64
  twace = CycI64 . twace . unCycI64
  powBasis = fmap CycI64 <$> powBasis
  coeffsCyc b = fmap CycI64 . coeffsCyc b . unCycI64

instance ExtensionCyc (CycG t) (ZqBasic q z) => ExtensionCyc (Cyc t) (ZqBasic q z) where
  embed = CycZqB . embed . unCycZqB
  twace = CycZqB . twace . unCycZqB
  powBasis = fmap CycZqB <$> powBasis
  coeffsCyc b = fmap CycZqB . coeffsCyc b . unCycZqB

instance (ExtensionCyc (Cyc t) a, ExtensionCyc (Cyc t) b)
  => ExtensionCyc (Cyc t) (a,b) where
  embed (CycPair a b) = CycPair (embed a) (embed b)
  twace (CycPair a b) = CycPair (twace a) (twace b)
  powBasis = zipWith CycPair <$> powBasis <*> powBasis
  coeffsCyc bas (CycPair a b) =
    zipWith CycPair (coeffsCyc bas a) (coeffsCyc bas b)

twace' :: forall t m m' r .
          (CRTElt t r, ZeroTestable r, IntegralDomain r, m `Divides` m')
       => CycG t m' r -> CycG t m r
twace' (Pow u) = Pow $ R.twacePow u
twace' (Dec u) = Dec $ R.twaceDec u
twace' (CRT u) = either (cycPE . twaceCRTE) (cycPC . twaceCRTC) u
twace' (Scalar u) = Scalar u
twace' (Sub (c :: CycG t l r)) = Sub (twace c :: CycG t (FGCD l m) r)
                                 \\ gcdDivides (Proxy::Proxy l) (Proxy::Proxy m)

embedLazy :: forall t m m' r . (m `Divides` m')
          => CycG t m r -> CycG t m' r
embedLazy (Scalar c) = Scalar c           -- keep as scalar
embedLazy (Sub (c :: CycG t l r)) = Sub c  -- keep as subring element
  \\ transDivides (Proxy::Proxy l) (Proxy::Proxy m) (Proxy::Proxy m')
embedLazy c = Sub c

-- | Force to a non-'Sub' constructor (for internal use only).
embed' :: forall t r l m . (l `Divides` m, CRTElt t r) => CycG t l r -> CycG t m r
{-# INLINE embed' #-}
embed' (Pow u) = Pow $ embedPow u
embed' (Dec u) = Dec $ embedDec u
embed' (CRT u) = either (cycPE . embedCRTE) (cycPC . embedCRTC) u
embed' (Scalar c) = Scalar c
embed' (Sub (c :: CycG t k r)) = embed' c
  \\ transDivides (Proxy::Proxy k) (Proxy::Proxy l) (Proxy::Proxy m)

-----

type instance LiftOf (CycG t m r) = CycG t m (LiftOf r)
type instance LiftOf (Cyc  t m r) = Cyc  t m (LiftOf r)

instance (Lift b a, CRTElt t b, Tensor t a) => LiftCyc (CycG t) b where
  liftCyc L.Pow = cycLiftPow
  liftCyc L.Dec = cycLiftDec

instance (LiftCyc (CycG t) (ZqBasic q Int64))
  => LiftCyc (Cyc t) (ZqBasic q Int64) where
  liftCyc b = CycI64 . liftCyc b . unCycZqB

cycLiftPow, cycLiftDec :: (Lift b a, Fact m, CRTElt t b, Tensor t a)
  => CycG t m b -> CycG t m a

-- | Lift using the powerful basis.
cycLiftPow (Pow u) = Pow $ lift u
cycLiftPow (Dec u) = Pow $ lift $ toPow u
cycLiftPow (CRT u) = Pow $ lift $ either toPow toPow u
-- optimized for subrings; these are correct for powerful basis but
-- not for decoding
cycLiftPow (Scalar c) = Scalar $ lift c
cycLiftPow (Sub c) = Sub $ cycLiftPow c

-- | Lift using the decoding basis.
cycLiftDec c = Dec $ lift $ uncycDec c

-- | The relative mod-@r@ CRT set of the extension.
crtSet :: (m `Divides` m', ZPP r, CRTElt t r, TensorCRTSet t (ZpOf r))
       => Tagged m [CycG t m' r]
crtSet = (Pow <$>) <$> R.crtSet
{-# INLINABLE crtSet #-}


---------- Promoted lattice operations ----------

-- | promoted from base ring
instance (Reduce a b, Fact m, CRTElt t a, CRTElt t b,
          ZeroTestable a, ZeroTestable b) -- ZT just for Additive superclasses
         => Reduce (CycG t m a) (CycG t m b) where
  {-# INLINABLE reduce #-}
  reduce (Pow u) = Pow $ reduce u
  reduce (Dec u) = Dec $ reduce u
  reduce (CRT u) = Pow $ reduce $ either toPow toPow u
  reduce (Scalar c) = Scalar $ reduce c
  reduce (Sub (c :: CycG t l a)) = Sub (reduce c :: CycG t l b)

instance Reduce (CycG t m Int64) (CycG t m (ZqBasic q Int64))
  => Reduce (Cyc t m Int64) (Cyc t m (ZqBasic q Int64)) where

  reduce = CycZqB . reduce . unCycI64

-----

instance {-# INCOHERENT #-} (Rescale a b, CRTElt t a, Tensor t b)
    => RescaleCyc (CycG t) a b where

  -- Optimized for subring constructors, for powerful basis.
  -- Analogs for decoding basis are not quite correct, because (* -1)
  -- doesn't commute with 'rescale' due to tiebreakers!
  rescaleCyc L.Pow (Scalar c) = Scalar $ rescale c
  rescaleCyc L.Pow (Sub c) = Sub $ rescalePow c

  rescaleCyc L.Pow c = Pow $ fmapI rescale $ uncycPow c
  rescaleCyc L.Dec c = Dec $ fmapI rescale $ uncycDec c
  {-# INLINABLE rescaleCyc #-}

instance RescaleCyc (CycG t) a a where
  -- No-op rescale
  rescaleCyc _ = id
  {-# INLINABLE rescaleCyc #-}

-- TODO: appropriate RescaleCyc instance for Cyc

{- CJP: restore this when we have a data family instance for pairs

-- | specialized instance for product rings of \(\Z_q\)s: ~2x faster
-- algorithm; removes one ring from the product.
instance (Mod a, Field b, Lift a (ModRep a), Reduce (LiftOf a) b,
         CRTElt t (a,b), CRTElt t a, CRTElt t b, CRTElt t (LiftOf a))
         => RescaleCyc (CycG t) (a,b) b where

  -- optimized for subrings and powerful basis (see comments in other
  -- instance for why this doesn't work for decoding basis)
  rescaleCyc L.Pow (Scalar c) = Scalar $ rescale c
  rescaleCyc L.Pow (Sub c) = Sub $ rescalePow c

  rescaleCyc bas c = let aval = proxy modulus (Proxy::Proxy a)
                         (a,b) = unzipCyc c
                         z = liftCyc bas a
                     in Scalar (recip (reduce aval)) * (b - reduce z)
  {-# INLINABLE rescaleCyc #-}

-}

-- CJP: do we really need these? Just have client call rescaleCyc
-- multiple times?

instance (RescaleCyc (CycG t) (b,c) c, Rescale (a,(b,c)) c,
          RescaleCyc (CycG t) (a,(b,c)) (b,c))
         => RescaleCyc (CycG t) (a,(b,c)) c where

  rescaleCyc bas (a :: CycG t m (a,(b,c))) =
    rescaleCyc bas (rescaleCyc bas a :: CycG t m (b,c))
  {-# INLINABLE rescaleCyc #-}

instance (RescaleCyc (CycG t) (b,(c,d)) d, Rescale (a,(b,(c,d))) d,
          RescaleCyc (CycG t) (a,(b,(c,d))) (b,(c,d)))
         => RescaleCyc (CycG t) (a,(b,(c,d))) d where

  rescaleCyc bas (a :: CycG t m (a,(b,(c,d)))) =
    rescaleCyc bas (rescaleCyc bas a :: CycG t m (b,(c,d)))
  {-# INLINABLE rescaleCyc #-}

instance (RescaleCyc (CycG t) (b,(c,(d,e))) e, Rescale (a,(b,(c,(d,e)))) e,
          RescaleCyc (CycG t) (a,(b,(c,(d,e)))) (b,(c,(d,e))))
         => RescaleCyc (CycG t) (a,(b,(c,(d,e)))) e where

  rescaleCyc bas (a :: CycG t m (a,(b,(c,(d,e))))) =
    rescaleCyc bas (rescaleCyc bas a :: CycG t m (b,(c,(d,e))))
  {-# INLINABLE rescaleCyc #-}

instance (RescaleCyc (CycG t) (b,(c,(d,(e,f)))) f, Rescale (a,(b,(c,(d,(e,f))))) f,
          RescaleCyc (CycG t) (a,(b,(c,(d,(e,f))))) (b,(c,(d,(e,f)))))
         => RescaleCyc (CycG t) (a,(b,(c,(d,(e,f))))) f where

  rescaleCyc bas (a :: CycG t m (a,(b,(c,(d,(e,f)))))) =
    rescaleCyc bas (rescaleCyc bas a :: CycG t m (b,(c,(d,(e,f)))))
  {-# INLINABLE rescaleCyc #-}

-----

-- | promoted from base ring
instance (Gadget gad zq, Fact m, CRTElt t zq, ZeroTestable zq, IntegralDomain zq)
  => Gadget gad (CycG t m zq) where
  gadget = (scalarCyc <$>) <$> gadget
  {-# INLINABLE gadget #-}
  -- CJP: default 'encode' works because mul-by-Scalar is fast

deriving instance Gadget gad (CycG t m (ZqBasic q z))
  => Gadget gad (Cyc t m (ZqBasic q z))

-----

-- | promoted from base ring, using the powerful basis for best geometry
instance (Decompose gad zq, Fact m, CRTElt t zq, CRTElt t (DecompOf zq),
          ZeroTestable zq, IntegralDomain zq, ZeroTestable (DecompOf zq),
          -- copied from Traversable (CycRep t P m) instance;
          -- needed for Sub case of this instance
          ForallFact1 Traversable t, ForallFact1 Applicative t,
          ForallFact1 Foldable t)
         => Decompose gad (CycG t m zq) where

  type DecompOf (CycG t m zq) = CycG t m (DecompOf zq)

  -- faster implementations: decompose directly in subring, which is
  -- correct because we decompose in powerful basis
  decompose (Scalar c) = pasteT $ Scalar <$> peelT (decompose c)
  decompose (Sub c) = pasteT $ Sub <$> peelT (decompose c)

  -- traverse: Traversable (CycRep t P m) and Applicative (Tagged gad ZL)
  decompose (Pow u) = fromZL $ Pow <$> traverse (toZL . decompose) u
  decompose c = decompose $ toPow' c

  {-# INLINABLE decompose #-}

instance Decompose gad (CycG t m (ZqBasic q Int64))
  => Decompose gad (Cyc t m (ZqBasic q Int64)) where

  type DecompOf (Cyc t m (ZqBasic q Int64)) = Cyc t m Int64
  decompose = fmap (fmap CycI64) . decompose . unCycZqB

toZL :: Tagged s [a] -> TaggedT s ZipList a
toZL = coerce

fromZL :: TaggedT s ZipList a -> Tagged s [a]
fromZL = coerce

-----

-- | promoted from base ring, using the decoding basis for best geometry
instance (Correct gad zq, Fact m, CRTElt t zq,
          ZeroTestable zq, IntegralDomain zq, Traversable (CycRep t D m))
  => Correct gad (CycG t m zq) where
  -- sequence: Monad [] and Traversable (CycRep t D m)
  -- sequenceA: Applicative (CycRep t D m) and Traversable (TaggedT gad [])
  correct bs = Dec *** (Dec <$>) $
               second sequence $ fmap fst &&& fmap snd $ (correct . pasteT) <$>
               sequenceA (uncycDec <$> peelT bs)
  {-# INLINABLE correct #-}

deriving instance Correct gad (CycG t m (ZqBasic q Int64))
  => Correct gad (Cyc t m (ZqBasic q Int64))

---------- Change of representation (internal use only) ----------

toPow', toDec', toCRT' :: (Fact m, CRTElt t r) => CycG t m r -> CycG t m r
{-# INLINABLE toPow' #-}
{-# INLINABLE toDec' #-}
{-# INLINABLE toCRT' #-}

-- | Force to powerful-basis representation (for internal use only).
toPow' c@(Pow _) = c
toPow' (Dec u) = Pow $ toPow u
toPow' (CRT u) = Pow $ either toPow toPow u
toPow' (Scalar c) = Pow $ scalarPow c
toPow' (Sub c) = toPow' $ embed' c

-- | Force to decoding-basis representation (for internal use only).
toDec' (Pow u) = Dec $ toDec u
toDec' c@(Dec _) = c
toDec' (CRT u) = Dec $ either toDec toDec u
toDec' (Scalar c) = Dec $ toDec $ scalarPow c
toDec' (Sub c) = toDec' $ embed' c

-- | Force to a CRT representation (for internal use only).
toCRT' (Pow u) = CRT $ toCRT u
toCRT' (Dec u) = CRT $ toCRT u
toCRT' c@(CRT _) = c
toCRT' (Scalar c) = CRT $ scalarCRT c
-- CJP: the following is the fastest algorithm for when both source
-- and target have the same CRTr/CRTe choice.  It is not the fastest
-- when the choices are different (it will do an unnecessary CRT if
-- input is non-CRT), but this is an unusual case.  Note: both calls
-- to toCRT' are necessary in general, because embed' may not preserve
-- CRT representation!
toCRT' (Sub c) = toCRT' $ embed' $ toCRT' c

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

-----

instance (Fact m, ForallFact2 Show t r, ForallFact2 Show t (CRTExt r), Show r)
  => Show (CycG t m r) where
  show (Pow x) = "Cyc.Pow " ++ show x
  show (Dec x) = "Cyc.Dec " ++ show x
  show (CRT (Left x)) = "Cyc.CRT " ++ show x
  show (CRT (Right x)) = "Cyc.CRT " ++ show x
  show (Scalar x) = "Cyc.Scalar " ++ show x
  show (Sub x) = "Cyc.Sub " ++ show x

deriving instance Show (CycG t m Double)        => Show (Cyc t m Double)
deriving instance Show (CycG t m Int64)         => Show (Cyc t m Int64)
deriving instance Show (CycG t m (ZqBasic q z)) => Show (Cyc t m (ZqBasic q z))
deriving instance (Show (Cyc t m a), Show (Cyc t m b))
                  => Show (Cyc t m (a,b))

-----

instance (NFData r, Fact m,
          ForallFact2 NFData t r, ForallFact2 NFData t (CRTExt r))
         => NFData (CycG t m r) where
  rnf (Pow u) = rnf u
  rnf (Dec u) = rnf u
  rnf (CRT u) = rnf u
  rnf (Scalar u) = rnf u
  rnf (Sub c) = rnf c

deriving instance NFData (CycG t m Double)        => NFData (Cyc t m Double)
deriving instance NFData (CycG t m Int64)         => NFData (Cyc t m Int64)
deriving instance NFData (CycG t m (ZqBasic q z)) => NFData (Cyc t m (ZqBasic q z))

instance (NFData (Cyc t m a), NFData (Cyc t m b)) => NFData (Cyc t m (a,b)) where
  rnf (CycPair a b) = rnf a `seq` rnf b

-----

instance (Fact m, CRTElt t r, Protoable (CycRep t D m r))
         => Protoable (CycG t m r) where

  type ProtoType (CycG t m r) = ProtoType (CycRep t D m r)
  toProto (Dec uc) = toProto uc
  toProto x = toProto $ toDec' x
  fromProto x = Dec <$> fromProto x

-- TODO: define Protoable instances for Cyc
