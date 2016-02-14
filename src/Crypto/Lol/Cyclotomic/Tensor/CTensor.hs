{-# LANGUAGE ConstraintKinds, DataKinds, GADTs,
             FlexibleContexts, FlexibleInstances, TypeOperators, PolyKinds,
             GeneralizedNewtypeDeriving, InstanceSigs, RoleAnnotations,
             MultiParamTypeClasses, NoImplicitPrelude, StandaloneDeriving,
             ScopedTypeVariables, TupleSections, TypeFamilies, RankNTypes,
             TypeSynonymInstances, UndecidableInstances,
             RebindableSyntax #-}

-- | Wrapper for a C implementation of the 'Tensor' interface.

module Crypto.Lol.Cyclotomic.Tensor.CTensor
( CT ) where

import Algebra.Additive as Additive (C)
import Algebra.Ring     as Ring (C)
import Algebra.ZeroTestable   as ZeroTestable (C)

import Control.Applicative
import Control.Arrow ((***))
import Control.DeepSeq
import Control.Monad (liftM)
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.Monad.Random
import Control.Monad.Trans (lift)

import Data.Coerce
import Data.Constraint  hiding ((***))
import Data.Foldable as F
import Data.Int
import Data.Maybe
import Data.Traversable as T
import Data.Vector.Generic           as V (zip, unzip)
import Data.Vector.Storable          as SV (Vector, (!), replicate, replicateM, thaw, convert, foldl',
                                            unsafeSlice, mapM, fromList,
                                            generate, foldl1',
                                            unsafeWith, zipWith, map, length, unsafeFreeze, thaw)
import Data.Vector.Storable.Internal (getPtr)
import Data.Vector.Storable.Mutable  as SM hiding (replicate)

import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr
import           Foreign.Storable        (Storable (..))
import           Test.QuickCheck         hiding (generate)

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Backend
import Crypto.Lol.Cyclotomic.Tensor.CTensor.Extension
import Crypto.Lol.GaussRandom
import Crypto.Lol.LatticePrelude as LP hiding (replicate, unzip, zip, lift)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.IZipVector
import Crypto.Lol.Types.ZqBasic

import System.IO.Unsafe (unsafePerformIO)

-- | Newtype wrapper around a Vector.
newtype CT' (m :: Factored) r = CT' { unCT :: Vector r } 
                              deriving (Show, Eq, NFData)

-- the first argument, though phantom, affects representation
type role CT' representational nominal

-- GADT wrapper that distinguishes between Unbox and unrestricted
-- element types

-- | An implementation of 'Tensor' backed by C code.
data CT (m :: Factored) r where 
  CT :: Storable r => CT' m r -> CT m r
  ZV :: IZipVector m r -> CT m r

instance Eq r => Eq (CT m r) where
  (ZV x) == (ZV y) = x == y
  (CT x) == (CT y) = x == y
  x@(CT _) == y = x == toCT y
  y == x@(CT _) = x == toCT y

deriving instance Show r => Show (CT m r)

toCT :: (Storable r) => CT m r -> CT m r
toCT v@(CT _) = v
toCT (ZV v) = CT $ zvToCT' v

toZV :: (Fact m) => CT m r -> CT m r
toZV (CT (CT' v)) = ZV $ fromMaybe (error "toZV: internal error") $
                    iZipVector $ convert v
toZV v@(ZV _) = v

zvToCT' :: forall m r . (Storable r) => IZipVector m r -> CT' m r
zvToCT' v = coerce (convert $ unIZipVector v :: Vector r)

wrap :: (Storable r) => (CT' l r -> CT' m r) -> (CT l r -> CT m r)
wrap f (CT v) = CT $ f v
wrap f (ZV v) = CT $ f $ zvToCT' v

wrapM :: (Storable r, Monad mon) => (CT' l r -> mon (CT' m r))
         -> (CT l r -> mon (CT m r))
wrapM f (CT v) = CT <$> f v
wrapM f (ZV v) = CT <$> f (zvToCT' v)

-- convert an CT' *twace* signature to Tagged one
type family Tw (r :: *) :: * where
  Tw (CT' m' r -> CT' m r) = Tagged '(m,m') (Vector r -> Vector r)
  Tw (Maybe (CT' m' r -> CT' m r)) = TaggedT '(m,m') Maybe (Vector r -> Vector r)

type family Em r where
  Em (CT' m r -> CT' m' r) = Tagged '(m,m') (Vector r -> Vector r)
  Em (Maybe (CT' m r -> CT' m' r)) = TaggedT '(m,m') Maybe (Vector r -> Vector r)


---------- NUMERIC PRELUDE INSTANCES ----------
instance (Additive r, Storable r, Fact m, Dispatch r)
  => Additive.C (CT m r) where
  (CT a@(CT' _)) + (CT b@(CT' _)) = CT $ (untag $ cZipDispatch dadd) a b
  a + b = (toCT a) + (toCT b)
  negate (CT (CT' a)) = CT $ CT' $ SV.map negate a -- EAC: This probably should be converted to C code
  negate a = negate $ toCT a

  zero = CT $ repl zero

instance (Fact m, Ring r, Storable r, Dispatch r)
  => Ring.C (CT m r) where
  (CT a@(CT' _)) * (CT b@(CT' _)) = CT $ (untag $ cZipDispatch dmul) a b

  fromInteger = CT . repl . fromInteger

instance (ZeroTestable r, Storable r, Fact m)
         => ZeroTestable.C (CT m r) where
  --{-# INLINABLE isZero #-} 
  isZero (CT (CT' a)) = SV.foldl' (\ b x -> b && isZero x) True a
  isZero (ZV v) = isZero v

---------- "Container" instances ----------

instance Fact m => Functor (CT m) where
  -- Functor instance is implied by Applicative laws
  fmap f x = pure f <*> x

instance Fact m => Applicative (CT m) where
  pure = ZV . pure

  (ZV f) <*> (ZV a) = ZV (f <*> a)
  f@(ZV _) <*> v@(CT _) = f <*> toZV v

instance Fact m => Foldable (CT m) where
  -- Foldable instance is implied by Traversable
  foldMap = foldMapDefault

instance Fact m => Traversable (CT m) where
  traverse f r@(CT _) = T.traverse f $ toZV r
  traverse f (ZV v) = ZV <$> T.traverse f v

instance Tensor CT where

  type TElt CT r = (Storable r, Dispatch r)

  entailIndexT = tag $ Sub Dict
  entailEqT = tag $ Sub Dict
  entailZTT = tag $ Sub Dict
  entailRingT = tag $ Sub Dict
  entailNFDataT = tag $ Sub Dict
  entailRandomT = tag $ Sub Dict

  scalarPow = CT . scalarPow' -- Vector code

  l = wrap $ untag $ basicDispatch dl
  lInv = wrap $ untag $ basicDispatch dlinv

  mulGPow = wrap mulGPow'
  mulGDec = wrap $ untag $ basicDispatch dmulgdec

  divGPow = wrapM divGPow'
  -- we divide by p in the C code (for divGDec only(?)), do NOT call checkDiv!
  divGDec = wrapM $ Just . untag (basicDispatch dginvdec)

  crtFuncs = (,,,,) <$>
    Just (CT . repl) <*>
    (wrap <$> untag (cZipDispatch dmul) <$> untagT gCoeffsCRT) <*>
    (wrap <$> untag (cZipDispatch dmul) <$> untagT gInvCoeffsCRT) <*>
    (wrap <$> untagT ctCRT) <*>
    (wrap <$> untagT ctCRTInv) 

  twacePowDec = wrap $ runIdentity $ coerceTw twacePowDec'
  embedPow = wrap $ runIdentity $ coerceEm embedPow'
  embedDec = wrap $ runIdentity $ coerceEm embedDec'

  tGaussianDec v = CT <$> cDispatchGaussian v
  --tGaussianDec v = CT <$> coerceT' (gaussianDec v)

  -- we do not wrap thsi function because (currently) it can only be called on lifted types
  gSqNormDec (CT v) = untag gSqNormDec' v
  gSqNormDec (ZV v) = gSqNormDec (CT $ zvToCT' v)

  crtExtFuncs = (,) <$> (wrap <$> coerceTw twaceCRT')
                    <*> (wrap <$> coerceEm embedCRT')

  coeffs = wrapM $ coerceCoeffs coeffs'

  powBasisPow = (CT <$>) <$> coerceBasis powBasisPow'

  crtSetDec = (CT <$>) <$> coerceBasis crtSetDec'

  fmapT f (CT v) = CT $ coerce (SV.map f) v
  fmapT f v@(ZV _) = fmapT f $ toCT v

  fmapTM f (CT (CT' v)) = (CT . CT') <$> SV.mapM f v
  fmapTM f v@(ZV _) = fmapTM f $ toCT v

  unzipTElt (CT (CT' v)) = (CT . CT') *** (CT . CT') $ unzip v
  unzipTElt v = unzipTElt $ toCT v

  unzipT v@(CT _) = unzipT $ toZV v
  unzipT (ZV v) = ZV *** ZV $ unzipIZV v


coerceTw :: (Functor mon) => TaggedT '(m, m') mon (Vector r -> Vector r) -> mon (CT' m' r -> CT' m r)
coerceTw = (coerce <$>) . untagT

coerceEm :: (Functor mon) => TaggedT '(m, m') mon (Vector r -> Vector r) -> mon (CT' m r -> CT' m' r)
coerceEm = (coerce <$>) . untagT

-- | Useful coersion for defining @coeffs@ in the @Tensor@
-- interface. Using 'coerce' alone is insufficient for type inference.
coerceCoeffs :: (Fact m, Fact m') 
  => Tagged '(m,m') (Vector r -> [Vector r]) -> CT' m' r -> [CT' m r]
coerceCoeffs = coerce

-- | Useful coersion for defining @powBasisPow@ and @crtSetDec@ in the @Tensor@
-- interface. Using 'coerce' alone is insufficient for type inference.
coerceBasis :: 
  (Fact m, Fact m')
  => Tagged '(m,m') [Vector r] -> Tagged m [CT' m' r]
coerceBasis = coerce

mulGPow' :: (TElt CT r, Fact m, Additive r) => CT' m r -> CT' m r
mulGPow' = untag $ basicDispatch dmulgpow

divGPow' :: forall m r . (TElt CT r, Fact m, IntegralDomain r, ZeroTestable r) => CT' m r -> Maybe (CT' m r)
divGPow' = untag $ checkDiv $ basicDispatch dginvpow

withBasicArgs :: forall m r . (Fact m, Storable r) 
  => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()) 
     -> CT' m r -> IO (CT' m r)
withBasicArgs f =
  let factors = proxy (marshalFactors <$> ppsFact) (Proxy::Proxy m)
      totm = proxy (fromIntegral <$> totientFact) (Proxy::Proxy m)
      numFacts = fromIntegral $ SV.length factors
  in \(CT' x) -> do
    yout <- SV.thaw x
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        f pout totm pfac numFacts))
    CT' <$> unsafeFreeze yout

basicDispatch :: forall m r .
     (Storable r, Fact m, Additive r)
      => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
         -> Tagged m (CT' m r -> CT' m r)
basicDispatch f = return $ unsafePerformIO . withBasicArgs f

gSqNormDec' :: forall m r .
     (Storable r, Fact m, Additive r, Dispatch r)
      => Tagged m (CT' m r -> r)
gSqNormDec' = return $ (!0) . unCT . unsafePerformIO . withBasicArgs dnorm

ctCRT :: forall m r . (Storable r, CRTrans r, Dispatch r,
          Fact m)
         => TaggedT m Maybe (CT' m r -> CT' m r)
ctCRT = do -- in TaggedT m Maybe
  ru' <- ru
  return $ \x -> unsafePerformIO $ 
    withPtrArray ru' (flip withBasicArgs x . dcrt)

-- CTensor CRT^(-1) functions take inverse rus
ctCRTInv :: (Storable r, CRTrans r, Dispatch r,
          Fact m)
         => TaggedT m Maybe (CT' m r -> CT' m r)
ctCRTInv = do -- in Maybe
  mhatInv <- snd <$> crtInfoFact
  ruinv' <- ruInv
  return $ \x -> unsafePerformIO $ 
    withPtrArray ruinv' (\ruptr -> with mhatInv (flip withBasicArgs x . dcrtinv ruptr))

checkDiv :: forall m r . 
  (IntegralDomain r, Storable r, ZeroTestable r, 
   Fact m)
    => Tagged m (CT' m r -> CT' m r) -> Tagged m (CT' m r -> Maybe (CT' m r))
checkDiv f = do
  f' <- f
  oddRad' <- fromIntegral <$> oddRadicalFact
  return $ \x -> 
    let (CT' y) = f' x
    in CT' <$> SV.mapM (`divIfDivis` oddRad') y

divIfDivis :: (IntegralDomain r, ZeroTestable r) => r -> r -> Maybe r
divIfDivis num den = let (q,r) = num `divMod` den
                     in if isZero r then Just q else Nothing

cZipDispatch :: (Storable r, Fact m, Additive r)
  => (Ptr r -> Ptr r -> Int64 -> IO ())
     -> Tagged m (CT' m r -> CT' m r -> CT' m r)
cZipDispatch f = do -- in Tagged m
  totm <- fromIntegral <$> totientFact
  return $ coerce $ \a b -> unsafePerformIO $ do
    yout <- SV.thaw a
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith b (\pin ->
        f pout pin totm))
    unsafeFreeze yout

cDispatchGaussian :: forall m r var rnd .
         (Storable r, Transcendental r, Dispatch r, Ord r,
          Fact m, ToRational var, Random r, MonadRandom rnd)
         => var -> rnd (CT' m r)
cDispatchGaussian var = flip proxyT (Proxy::Proxy m) $ do -- in TaggedT m rnd
  -- get rus for (Complex r)
  ruinv' <- mapTaggedT (return . fromMaybe (error "complexGaussianRoots")) ruInv
  totm <- pureT totientFact
  m <- pureT valueFact
  rad <- pureT radicalFact
  yin <- lift $ realGaussians (var * fromIntegral (m `div` rad)) totm
  return $ unsafePerformIO $ 
    withPtrArray ruinv' (\ruptr -> withBasicArgs (dgaussdec ruptr) (CT' yin))

instance (Arbitrary r, Fact m, Storable r) => Arbitrary (CT' m r) where
  arbitrary = replM arbitrary
  shrink = shrinkNothing

instance (Storable r, Arbitrary (CT' m r)) => Arbitrary (CT m r) where
  arbitrary = CT <$> arbitrary

instance (Storable r, Random r, Fact m) => Random (CT' m r) where
  --{-# INLINABLE random #-}
  random = runRand $ replM (liftRand random)

  randomR = error "randomR nonsensical for CT'"

instance (Storable r, Random (CT' m r)) => Random (CT m r) where
  --{-# INLINABLE random #-}
  random = runRand $ CT <$> liftRand random

  randomR = error "randomR nonsensical for CT"

instance (NFData r) => NFData (CT m r) where
  rnf (CT v) = rnf v
  rnf (ZV v) = rnf v

repl :: forall m r . (Fact m, Storable r) => r -> CT' m r
repl = let n = proxy totientFact (Proxy::Proxy m)
       in coerce . SV.replicate n

replM :: forall m r mon . (Fact m, Storable r, Monad mon) 
         => mon r -> mon (CT' m r)
replM = let n = proxy totientFact (Proxy::Proxy m)
        in fmap coerce . SV.replicateM n

--{-# INLINE scalarPow' #-}
scalarPow' :: forall m r . (Fact m, Additive r, Storable r) => r -> CT' m r
-- constant-term coefficient is first entry wrt powerful basis
scalarPow' = 
  let n = proxy totientFact (Proxy::Proxy m)
  in \r -> CT' $ generate n (\i -> if i == 0 then r else zero)

ru, ruInv :: forall r m . 
   (CRTrans r, Fact m, Storable r)
   => TaggedT m Maybe [Vector r]
--{-# INLINE ru #-}
ru = do
  mval <- pureT valueFact
  wPow <- fst <$> crtInfoFact
  LP.map
    (\(p,e) -> do
        let pp = p^e
            pow = mval `div` pp
        generate pp (wPow . (*pow))) <$>
      pureT ppsFact

--{-# INLINE ruInv #-}
ruInv = do
  mval <- pureT valueFact
  wPow <- fst <$> crtInfoFact
  LP.map
    (\(p,e) -> do
        let pp = p^e
            pow = mval `div` pp
        generate pp (\i -> wPow $ -i*pow)) <$>
      pureT ppsFact

gCoeffsCRT, gInvCoeffsCRT :: (TElt CT r, CRTrans r, Fact m, ZeroTestable r, IntegralDomain r)
  => TaggedT m Maybe (CT' m r)
gCoeffsCRT = ctCRT <*> return (mulGPow' $ scalarPow' LP.one)
-- It's necessary to call 'fromJust' here: otherwise 
-- sequencing functions in 'crtFuncs' relies on 'divGPow' having an
-- implementation in C, which is not true for all types which have a C
-- implementation of, e.g. 'crt'. In particular, 'Complex Double' has C support
-- for 'crt', but not for 'divGPow'.
-- This really breaks the contract of Tensor, so it's probably a bad idea.
--   Someone can get the "crt" and can even pull the function "divGCRT" from Tensor,
--   but it will fail when they try to apply it.
-- As an implementation note if I ever do fix this: the division by rad(m) can be
-- tricky for Double/Complex Doubles, so be careful! This is why we have a custom
-- Complex wrapper around NP.Complex.
gInvCoeffsCRT = ($ fromJust $ divGPow' $ scalarPow' LP.one) <$> ctCRT

-- we can't put this in Extension with the rest of the twace/embed fucntions because it needs access to 
-- the C backend
twaceCRT' :: forall m m' r .
             (TElt CT r, CRTrans r, m `Divides` m', ZeroTestable r, IntegralDomain r)
             => TaggedT '(m, m') Maybe (Vector r -> Vector r)
twaceCRT' = tagT $ do -- Maybe monad
  (CT' g') <- proxyT gCoeffsCRT (Proxy::Proxy m')
  (CT' gInv) <- proxyT gInvCoeffsCRT (Proxy::Proxy m)
  embed <- proxyT embedCRT' (Proxy::Proxy '(m,m'))
  indices <- pure $ proxy extIndicesCRT (Proxy::Proxy '(m,m'))
  (_, m'hatinv) <- proxyT crtInfoFact (Proxy::Proxy m')
  let phi = proxy totientFact (Proxy::Proxy m)
      phi' = proxy totientFact (Proxy::Proxy m')
      mhat = fromIntegral $ proxy valueHatFact (Proxy::Proxy m)
      hatRatioInv = m'hatinv * mhat
      reltot = phi' `div` phi
      -- tweak = mhat * g' / (m'hat * g)
      tweak = SV.map (* hatRatioInv) $ SV.zipWith (*) (embed gInv) g'
  return $ \ arr -> -- take true trace after mul-by-tweak
    let v = backpermute' indices (SV.zipWith (*) tweak arr)
    in generate phi $ \i -> foldl1' (+) $ SV.unsafeSlice (i*reltot) reltot v
