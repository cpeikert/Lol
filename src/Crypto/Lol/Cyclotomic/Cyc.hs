{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | An implementation of cyclotomic rings with safe interface:
-- functions and instances involving 'Cyc' expose nothing about the
-- internal representations of ring elements (e.g., the basis they are
-- represented in).  For an experts-only, "unsafe" implementation that
-- offers limited exposure of internal representation, use
-- 'Crypto.Lol.Cyclotomic.UCyc.UCyc'.

module Crypto.Lol.Cyclotomic.Cyc
( 
-- * Data type
  Cyc, U.CElt, cyc, unsafeUnCyc
-- * Basic operations
, mulG, divG
, scalarCyc, liftCyc
, advisePow, adviseDec, adviseCRT
-- * Error sampling
, tGaussian, errorRounded, errorCoset
-- * Sub/extension rings
, embed, twace, powBasis, crtSet, coeffsCyc
, module Crypto.Lol.Cyclotomic.Utility
) where

import Algebra.Additive as Additive (C)
import Algebra.Ring     as Ring (C)

import           Crypto.Lol.Cyclotomic.UCyc    (CElt, UCyc)
import qualified Crypto.Lol.Cyclotomic.UCyc    as U
import           Crypto.Lol.Cyclotomic.Utility
import           Crypto.Lol.Gadget
import           Crypto.Lol.LatticePrelude     as LP
import           Crypto.Lol.Types.ZPP

import Control.Applicative    hiding ((*>))
import Control.DeepSeq
import Control.Monad.Random

import Data.Coerce

import Test.QuickCheck

-- | Wrapper around 'UCyc' that exposes a narrower, safe interface.
newtype Cyc t m r = Cyc { 
  -- | Unsafe deconstructor for 'Cyc'.
  unsafeUnCyc :: UCyc t m r }
                    deriving (Arbitrary, Random)

-- See: https://ghc.haskell.org/trac/ghc/ticket/11008
-- for why I have to use StandaloneDeriving here
deriving instance NFData (UCyc t m a) => NFData (Cyc t m a)
deriving instance Show (UCyc t m a) => Show (Cyc t m a)
deriving instance Eq (UCyc t m a) => Eq (Cyc t m a)
deriving instance Additive (UCyc t m a) => Additive.C (Cyc t m a)
deriving instance Ring (UCyc t m a) => Ring.C (Cyc t m a)
deriving instance Gadget gad (UCyc t m a) => Gadget gad (Cyc t m a)
deriving instance Correct gad (UCyc t m a) => Correct gad (Cyc t m a)

-- | Smart constructor (to prevent clients from pattern-matching).
cyc :: UCyc t m r -> Cyc t m r
cyc = Cyc

-- (try to) replace all occurrences of 'Cyc' with 'UCyc'
type family O a where
  O (Cyc t m a) = UCyc t m a
  O (a -> b) = O a -> O b
  O (m a) = m (O a)             -- works for concrete m, but not vars
  O a = a

-- specialized 'coerce', to aid type inference
coerceCyc :: (Coercible (O a) a) => O a -> a
coerceCyc = coerce

-- Can't seem to auto-derive these, due to constraints with GND and 
-- MPTCs.
instance (Reduce a b, Fact m, CElt t a, CElt t b)
         => Reduce (Cyc t m a) (Cyc t m b) where
  reduce = coerceCyc reduce

-- CJP: will this pick the right overlapping instance for UCyc?  I
-- think so...
instance (RescaleCyc (UCyc t) a b) => RescaleCyc (Cyc t) a b where
  rescaleCyc = coerceCyc rescaleCyc

instance (Decompose gad (UCyc t m zq),
          Reduce (Cyc t m (DecompOf zq)) (Cyc t m zq))
         => Decompose gad (Cyc t m zq) where

  type DecompOf (Cyc t m zq) = Cyc t m (DecompOf zq)
  decompose = coerceCyc decompose

---------- Core cyclotomic operations ----------

adviseCRT, advisePow, adviseDec :: (Fact m, CElt t r) => Cyc t m r -> Cyc t m r

-- | Yield an equivalent element that /may/ be in a CRT
-- representation.  This can serve as an optimization hint. E.g.,
-- call 'adviseCRT' prior to multiplying the same value by many
-- other values.
adviseCRT = coerceCyc U.adviseCRT

-- | Same as 'adviseCRT', but for the powerful-basis representation.
advisePow = coerceCyc U.forcePow -- do it, but not required by contract

-- | Same as 'adviseCRT', but for the powerful-basis representation.
adviseDec = coerceCyc U.forceDec


-- | Multiply by the special element @g@ of the @m@th cyclotomic.
mulG :: (Fact m, CElt t r) => Cyc t m r -> Cyc t m r
mulG = coerceCyc U.mulG

-- | Divide by @g@, returning 'Nothing' if not evenly divisible.
-- WARNING: this implementation is not a constant-time algorithm, so
-- information about the argument may be leaked through a timing
-- channel.
divG :: (Fact m, CElt t r) => Cyc t m r -> Maybe (Cyc t m r)
divG = coerceCyc U.divG

-- | Sample from the "tweaked" Gaussian error distribution @t*D@ in
-- the decoding basis, where @D@ has scaled variance @v@.  Note: This
-- implementation uses Double precision to generate the Gaussian
-- sample, which may not be sufficient for rigorous proof-based
-- security.
tGaussian :: (Fact m, OrdFloat q, Random q, CElt t q,
              ToRational v, MonadRandom rnd)
             => v -> rnd (Cyc t m q)
tGaussian = (Cyc <$>) . U.tGaussian

-- | Generate an LWE error term with given scaled variance,
-- deterministically rounded in the decoding basis.
errorRounded :: (ToInteger z, Fact m, CElt t z,
                 ToRational v, MonadRandom rnd)
                => v -> rnd (Cyc t m z)
errorRounded = (Cyc <$>) . U.errorRounded

-- | Generate an LWE error term with given scaled variance @* p^2@ over
-- the given coset, deterministically rounded in the decoding basis.
errorCoset ::
  (Mod zp, z ~ ModRep zp, Lift zp z, Fact m,
   CElt t zp, CElt t z, ToRational v, MonadRandom rnd)
  => v -> Cyc t m zp -> rnd (Cyc t m z)
errorCoset v = (Cyc <$>) . U.errorCoset v . unsafeUnCyc

-- | Embed into the extension ring.
embed :: (m `Divides` m', CElt t r) => Cyc t m r -> Cyc t m' r
embed = coerceCyc U.embed

-- | The "tweaked trace" (twace) function
-- @Tw(x) = (mhat \/ m'hat) * Tr(g' \/ g * x)@,
-- which fixes @R@ pointwise (i.e., @twace . embed == id@).
twace :: (m `Divides` m', CElt t r) => Cyc t m' r -> Cyc t m r
twace = coerceCyc U.twace

-- | Return the given element's coefficient vector with respect to
-- the (relative) powerful/decoding basis of the cyclotomic
-- extension @O_m' / O_m@.
coeffsCyc :: (m `Divides` m', CElt t r)
             => Basis -> Cyc t m' r -> [Cyc t m r]
coeffsCyc = coerceCyc U.coeffsCyc

-- | The relative powerful basis of @O_m' / O_m@.
powBasis :: (m `Divides` m', CElt t r) => Tagged m [Cyc t m' r]
powBasis = coerceCyc U.powBasis

-- | The relative mod-@r@ "CRT set" of the extension.
crtSet :: (m `Divides` m', ZPP r, CElt t r, CElt t (ZPOf r))
          => Tagged m [Cyc t m' r]
crtSet = coerceCyc U.crtSet

-- | Lift in the specified basis.
liftCyc :: (Lift b a, Fact m, CElt t a, CElt t b)
           => Basis -> Cyc t m b -> Cyc t m a
liftCyc = coerceCyc U.liftCyc

-- | Embed a scalar from the base ring as a cyclotomic element.
scalarCyc :: (Fact m, CElt t a) => a -> Cyc t m a
scalarCyc = Cyc . U.scalarCyc


