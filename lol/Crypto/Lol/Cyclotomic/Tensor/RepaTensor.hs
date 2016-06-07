{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

-- | A pure, repa-based implementation of the Tensor interface.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor
( RT ) where

import Crypto.Lol.Cyclotomic.Tensor                      as T
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.CRT
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Dec
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Extension
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon  as RT hiding
                                                                ((++))
import Crypto.Lol.Prelude                                as LP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.FiniteField                      as FF
import Crypto.Lol.Types.IZipVector
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.RRq
import Crypto.Lol.Types.ZqBasic

import Crypto.Proto.RLWE.Kq
import Crypto.Proto.RLWE.Rq

import Algebra.Additive     as Additive (C)
import Algebra.Module       as Module (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative  hiding ((*>))
import Control.Arrow        hiding (arr)
import Control.DeepSeq      (NFData (rnf))
import Control.Monad.Except (throwError)
import Control.Monad.Random
import Data.Coerce
import Data.Constraint      hiding ((***))
import Data.Foldable        as F
import Data.Maybe
import Data.Sequence        as S (fromList)
import Data.Traversable     as T
import Data.Vector          as V hiding (force, (++))
import Data.Vector.Unboxed  as U hiding (force, (++))

import Test.QuickCheck

-- | An implementation of 'Tensor' backed by repa.
data RT (m :: Factored) r where
  RT :: Unbox r => !(Arr m r) -> RT m r
  ZV :: IZipVector m r -> RT m r

deriving instance Show r => Show (RT m r)

instance (Fact m, Reflects q Int64) => Protoable (RT m (ZqBasic q Int64)) where
  type ProtoType (RT m (ZqBasic q Int64)) = Rq

  toProto (RT (Arr xs)) =
    let m = fromIntegral $ proxy valueFact (Proxy::Proxy m)
        q = proxy value (Proxy::Proxy q) :: Int64
    in Rq m (fromIntegral q) $ S.fromList $ RT.toList $ RT.map lift xs
  toProto x@(ZV _) = toProto $ toRT x

  fromProto (Rq m' q' xs) =
    let m = proxy valueFact (Proxy::Proxy m) :: Int
        q = proxy value (Proxy::Proxy q) :: Int64
        n = proxy totientFact (Proxy::Proxy m)
        xs' = RT.fromList (Z:.n) $ LP.map reduce $ F.toList xs
        len = F.length xs
    in if m == fromIntegral m' && len == n && fromIntegral q == q'
       then return $ RT $ Arr xs'
       else throwError $
            "An error occurred while reading the proto type for RT.\n\
            \Expected m=" ++ show m ++ ", got " ++ show m' ++ "\n\
            \Expected n=" ++ show n ++ ", got " ++ show len ++ "\n\
            \Expected q=" ++ show q ++ ", got " ++ show q' ++ "."

instance (Fact m, Reflects q Double) => Protoable (RT m (RRq q Double)) where
  type ProtoType (RT m (RRq q Double)) = Kq

  toProto (RT (Arr xs)) =
    let m = fromIntegral $ proxy valueFact (Proxy::Proxy m)
        q = proxy value (Proxy::Proxy q) :: Double
    in Kq m q $ S.fromList $ RT.toList $ RT.map lift xs
  toProto x@(ZV _) = toProto $ toRT x

  fromProto (Kq m' q' xs) =
    let m = proxy valueFact (Proxy::Proxy m) :: Int
        q = proxy value (Proxy::Proxy q) :: Double
        n = proxy totientFact (Proxy::Proxy m)
        xs' = RT.fromList (Z:.n) $ LP.map reduce $ F.toList xs
        len = F.length xs
    in if m == fromIntegral m' && len == n && q == q'
       then return $ RT $ Arr xs'
       else throwError $
            "An error occurred while reading the proto type for RT.\n\
            \Expected m=" ++ show m ++ ", got " ++ show m' ++ "\n\
            \Expected n=" ++ show n ++ ", got " ++ show len ++ "\n\
            \Expected q=" ++ show (round q :: Int64) ++ ", got " ++ show q' ++ "."

instance Eq r => Eq (RT m r) where
  (ZV a) == (ZV b) = a == b
  (RT a) == (RT b) = a == b
  a@(RT _) == b = a == toRT b
  a == b@(RT _) = toRT a == b
  {-# INLINABLE (==) #-}

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

{-# INLINABLE wrap #-}
wrap :: Unbox r => (Arr l r -> Arr m r) -> RT l r -> RT m r
wrap f (RT v) = RT $ f v
wrap f (ZV v) = RT $ f $ zvToArr v

{-# INLINABLE wrapM #-}
wrapM :: (Unbox r, Monad mon) => (Arr l r -> mon (Arr m r))
         -> RT l r -> mon (RT m r)
wrapM f (RT v) = RT <$> f v
wrapM f (ZV v) = RT <$> f (zvToArr v)

instance Tensor RT where

  type TElt RT r = (Unbox r, Elt r)

  entailIndexT  = tag $ Sub Dict
  entailEqT     = tag $ Sub Dict
  entailZTT     = tag $ Sub Dict
  entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict
  entailShowT   = tag $ Sub Dict
  entailModuleT = tag $ Sub Dict

  scalarPow = RT . scalarPow'

  l = wrap fL
  lInv = wrap fLInv

  mulGPow = wrap fGPow
  mulGDec = wrap fGDec

  divGPow = wrapM fGInvPow
  divGDec = wrapM fGInvDec

  crtFuncs = (,,,,) <$>
             ((RT .) <$> scalarCRT') <*>
             (wrap <$> mulGCRT') <*>
             (wrap <$> divGCRT') <*>
             (wrap <$> fCRT) <*>
             (wrap <$> fCRTInv)

  tGaussianDec = fmap RT . tGaussianDec'

  gSqNormDec (RT e) = gSqNormDec' e
  gSqNormDec e = gSqNormDec $ toRT e

  twacePowDec = wrap twacePowDec'

  embedPow = wrap embedPow'
  embedDec = wrap embedDec'

  crtExtFuncs = (,) <$> (wrap <$> twaceCRT') <*> (wrap <$> embedCRT')

  coeffs = wrapM coeffs'

  powBasisPow = (RT <$>) <$> powBasisPow'

  crtSetDec = (RT <$>) <$> crtSetDec'

  fmapT f (RT v) = RT $ (coerce $ force . RT.map f) v
  fmapT f v@(ZV _) = fmapT f $ toRT v

  -- Repa arrays don't have mapM, so apply to underlying Unboxed
  -- vector instead
  fmapTM f (RT (Arr arr)) = (RT . Arr . fromUnboxed (extent arr)) <$>
                            U.mapM f (toUnboxed arr)
  fmapTM f v = fmapTM f $ toRT v

  zipWithT f (RT (Arr a1)) (RT (Arr a2)) = RT $ Arr $ force $ RT.zipWith f a1 a2
  zipWithT f v1 v2 = zipWithT f (toRT v1) (toRT v2)

  unzipT v@(RT _) = unzipT $ toZV v
  unzipT (ZV v) = ZV *** ZV $ unzipIZV v

  {-# INLINABLE entailIndexT #-}
  {-# INLINABLE entailEqT #-}
  {-# INLINABLE entailZTT #-}
  {-# INLINABLE entailNFDataT #-}
  {-# INLINABLE entailRandomT #-}
  {-# INLINABLE entailShowT #-}
  {-# INLINABLE scalarPow #-}
  {-# INLINABLE l #-}
  {-# INLINABLE lInv #-}
  {-# INLINABLE mulGPow #-}
  {-# INLINABLE mulGDec #-}
  {-# INLINABLE divGPow #-}
  {-# INLINABLE divGDec #-}
  {-# INLINABLE crtFuncs #-}
  {-# INLINABLE twacePowDec #-}
  {-# INLINABLE embedPow #-}
  {-# INLINABLE embedDec #-}
  {-# INLINABLE tGaussianDec #-}
  {-# INLINABLE gSqNormDec #-}
  {-# INLINABLE crtExtFuncs #-}
  {-# INLINABLE coeffs #-}
  {-# INLINABLE powBasisPow #-}
  {-# INLINABLE crtSetDec #-}
  {-# INLINABLE fmapT #-}
  {-# INLINABLE fmapTM #-}
  {-# INLINABLE zipWithT #-}
  {-# INLINABLE unzipT #-}


---------- Category-theoretic instances ----------

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

--CJP: Additive, Ring are not necessary when we use zipWithT
--EAC: But we need an Additive instance for the Module instance

instance (Unbox r, Additive (Arr m r)) => Additive.C (RT m r) where
  zero = RT zero

  (RT a) + (RT b) = RT $ a + b
  a + b = toRT a + toRT b

  (RT a) - (RT b) = RT $ a - b
  a - b = toRT a - toRT b

  negate (RT a) = RT $ negate a
  negate a = negate $ toRT a

  {-# INLINABLE (+) #-}
  {-# INLINABLE (-) #-}
  {-# INLINABLE zero #-}
  {-# INLINABLE negate #-}

{-
instance (Unbox r, Ring (Arr m r)) => Ring.C (RT m r) where
  (RT a) * (RT b) = RT $ a * b
  a * b = toRT a * toRT b

  fromInteger = RT . fromInteger
  {-# INLINABLE (*) #-}
  {-# INLINABLE fromInteger #-}
-}

instance (ZeroTestable (Arr m r), ZeroTestable (IZipVector m r))
    => ZeroTestable.C (RT m r) where
  isZero (RT a) = isZero a
  isZero (ZV v) = isZero v
  {-# INLINABLE isZero #-}

instance (GFCtx fp d, Fact m, Additive (RT m fp))
    => Module.C (GF fp d) (RT m fp) where

  r *> v = case v of
    RT (Arr arr) -> RT $ Arr $ RT.fromList (extent arr)
                    $ unCoeffs $ r *> Coeffs $ RT.toList arr
    ZV zv -> ZV $ fromJust $ iZipVector $ V.fromList
             $ unCoeffs $ r *> Coeffs $ V.toList $ unIZipVector zv

---------- Miscellaneous instances ----------

instance (Unbox r, Random (Arr m r)) => Random (RT m r) where
  random = runRand $ RT <$> liftRand random

  randomR = error "randomR nonsensical for RT"

instance (Unbox r, Arbitrary (Arr m r)) => Arbitrary (RT m r) where
  arbitrary = RT <$> arbitrary

instance (NFData r) => NFData (RT m r) where
  rnf (RT v) = rnf v
  rnf (ZV v) = rnf v
