{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances, GADTs, InstanceSigs,
             MultiParamTypeClasses, NoImplicitPrelude, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A pure, repa-based implementation of the Tensor interface.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor
( RT ) where

import Crypto.Lol.Cyclotomic.Tensor                      as T
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.CRT
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Extension
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Gauss
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon  as RT
import Crypto.Lol.LatticePrelude                         as LP hiding
                                                                ((!!))
import Crypto.Lol.Types.IZipVector

import Algebra.Additive     as Additive (C)
import Algebra.Ring         as Ring (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
import Control.DeepSeq       (NFData (rnf))
import Control.Monad         (liftM)
import Control.Monad.Random
import Data.Coerce
import Data.Constraint
import Data.Foldable         as F
import Data.Maybe
import Data.Traversable      as T
import Data.Typeable
import Data.Vector.Unboxed   as U hiding (force)
import Test.QuickCheck

-- | An implementation of 'Tensor' backed by repa.
data RT (m :: Factored) r where
  RT :: Unbox r => !(Arr m r) -> RT m r
  ZV :: IZipVector m r -> RT m r
  deriving (Typeable)

deriving instance Show r => Show (RT m r)

instance Eq r => Eq (RT m r) where
  (ZV a) == (ZV b) = a == b
  (RT a) == (RT b) = a == b
  a@(RT _) == b = a == toRT b
  a == b@(RT _) = toRT a == b

zvToArr :: Unbox r => IZipVector m r -> Arr m r
zvToArr v = let vec = convert $ unIZipVector v
            in Arr $ fromUnboxed (Z :. U.length vec) vec

-- converts to RT constructor
toRT :: Unbox r => RT m r -> RT m r
toRT v@(RT _) = v
toRT (ZV v) = RT $ zvToArr v

toZV :: Fact m => RT m r -> RT m r
toZV (RT (Arr v)) = ZV $ fromMaybe (error "toZV: internal error") $
                    iZipVector $ convert $ toUnboxed v
toZV v@(ZV _) = v

wrap :: Unbox r => (Arr l r -> Arr m r) -> RT l r -> RT m r
wrap f (RT v) = RT $ f v
wrap f (ZV v) = RT $ f $ zvToArr v

wrapM :: (Unbox r, Monad mon) => (Arr l r -> mon (Arr m r))
         -> RT l r -> mon (RT m r)
wrapM f (RT v) = liftM RT $ f v
wrapM f (ZV v) = liftM RT $ f $ zvToArr v

instance Tensor RT where

  type TElt RT r = (Unbox r, Elt r)

  entailIndexT  = tag $ Sub Dict
  entailEqT = tag $ Sub Dict
  entailZTT = tag $ Sub Dict
  entailRingT = tag $ Sub Dict
  entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict

  scalarPow = RT . scalarPow'

  l = wrap fL
  lInv = wrap fLInv

  mulGPow = wrap fGPow
  mulGDec = wrap fGDec

  divGPow = wrapM  fGInvPow
  divGDec = wrapM  fGInvDec

  crtFuncs = (,,,,) <$>
             (liftM (RT .) scalarCRT') <*>
             (wrap <$> mulGCRT') <*>
             (wrap <$> divGCRT') <*>
             (wrap <$> fCRT) <*>
             (wrap <$> fCRTInv)

  -- instance sigs are the cleanest way to handle many weird types
  -- coming up

  tGaussianDec :: forall v rnd m q .
                  (Fact m, OrdFloat q, Random q, TElt RT q,
                   ToRational v, MonadRandom rnd) => v -> rnd (RT m q)
  tGaussianDec = liftM RT . tGaussianDec'

  twacePowDec = wrap twacePowDec'

  embedPow = wrap embedPow'
  embedDec = wrap embedDec'

  crtExtFuncs = (,) <$> (liftM wrap twaceCRT')
                    <*> (liftM wrap embedCRT')

  coeffs = wrapM coeffs'

  powBasisPow = (RT <$>) <$> powBasisPow'

  crtSetDec = (RT <$>) <$> crtSetDec'

  fmapT f (RT v) = RT $ (coerce $ force . RT.map f) v
  fmapT f v@(ZV _) = fmapT f $ toRT v

  -- Repa arrays don't have mapM, so apply to underlying Unboxed
  -- vector instead
  fmapTM f (RT (Arr arr)) = liftM (RT . Arr . fromUnboxed (extent arr)) $
                            U.mapM f $ toUnboxed arr
  fmapTM f v@(ZV _) = fmapTM f $ toRT v

---------- "Container" instances ----------

instance Fact m => Functor (RT m) where
  -- Functor instance is implied by Applicative
  fmap f x = pure f <*> x

instance Fact m => Applicative (RT m) where
  pure = ZV . pure

  -- RT can never hold an a -> b
  (ZV f) <*> (ZV a) = ZV (f <*> a)
  f@(ZV _) <*> v@(RT _) = f <*> toZV v

instance Fact m => Foldable (RT m) where
  -- Foldable instance is implied by Traversable
  foldMap = foldMapDefault

instance Fact m => Traversable (RT m) where
  traverse f r@(RT _) = T.traverse f $ toZV r
  traverse f (ZV v) = ZV <$> T.traverse f v


---------- Numeric Prelude instances ----------

-- CJP: should Elt, Unbox be constraints on these instances?  It's
-- possible to zipWith on IZipVector, so it's not *necessary* to
-- convert toRT.

instance (Fact m, Additive r, Unbox r, Elt r) => Additive.C (RT m r) where
  (RT a) + (RT b) = RT $ coerce (\x -> force . RT.zipWith (+) x) a b
  a + b = toRT a + toRT b

  negate (RT a) = RT $ (coerce $ force . RT.map negate) a
  negate a = negate $ toRT a

  zero = RT $ repl zero

instance (Fact m, Ring r, Unbox r, Elt r) => Ring.C (RT m r) where
  (RT a) * (RT b) = RT $ coerce (\x -> force . RT.zipWith (*) x) a b
  a * b = (toRT a) * (toRT b)

  fromInteger = RT . repl . fromInteger

instance (Fact m, ZeroTestable r, Unbox r, Elt r) => ZeroTestable.C (RT m r) where
  -- not using 'zero' to avoid Additive r constraint
  isZero (RT (Arr a)) = isZero $ foldAllS (\ x y -> if isZero x then y else x) (a RT.! (Z:.0)) a
  isZero (ZV v) = isZero v

---------- Miscellaneous instances ----------

-- CJP: shouldn't these instances be defined in RTCommon, where the
-- Arr data type is defined?  Here they are orphans.

instance (Unbox r, Random (Arr m r)) => Random (RT m r) where
  random = runRand $ liftM RT (liftRand random)

  randomR = error "randomR nonsensical for RT"

instance (Unbox r, Arbitrary (Arr m r)) => Arbitrary (RT m r) where
  arbitrary = RT <$> arbitrary

instance (NFData r) => NFData (RT m r) where
  rnf (RT v) = rnf v
  rnf (ZV v) = rnf v
