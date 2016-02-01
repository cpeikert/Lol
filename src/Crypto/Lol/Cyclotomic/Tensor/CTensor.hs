{-# LANGUAGE ConstraintKinds, DataKinds, GADTs,
             FlexibleContexts, FlexibleInstances, TypeOperators, PolyKinds,
             GeneralizedNewtypeDeriving, InstanceSigs, RoleAnnotations,
             MultiParamTypeClasses, NoImplicitPrelude, StandaloneDeriving,
             ScopedTypeVariables, TupleSections, TypeFamilies, RankNTypes,
             TypeSynonymInstances, UndecidableInstances,
             RebindableSyntax #-}

-- | Wrapper for a C implementation of the 'Tensor' interface.

module Crypto.Lol.Cyclotomic.Tensor.CTensor
( CT
-- Exports below here are due solely to ticket #10338. See CycTests for more details
, Dispatch
) where

import Algebra.Additive as Additive (C)
import Algebra.Ring     as Ring (C)
import Algebra.ZeroTestable   as ZeroTestable (C)

import Control.Applicative
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.Identity
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
                                            unsafeToForeignPtr0, unsafeSlice, mapM, fromList,
                                            generate, foldl1',
                                            unsafeWith, zipWith, map, length, unsafeFreeze, thaw)
import Data.Vector.Storable.Internal (getPtr)
import Data.Vector.Storable.Mutable  as SM hiding (replicate)

import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils (with)
import           Foreign.Ptr
import           Foreign.Storable        (Storable (..))
import qualified Foreign.Storable.Record as Store
import           Test.QuickCheck         hiding (generate)

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
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
zvToCT' v = coerce $ (convert $ unIZipVector v :: Vector r)

wrap :: (Storable r) => (CT' l r -> CT' m r) -> (CT l r -> CT m r)
wrap f (CT v) = CT $ f v
wrap f (ZV v) = CT $ f $ zvToCT' v

wrapM :: (Storable r, Monad mon) => (CT' l r -> mon (CT' m r))
         -> (CT l r -> mon (CT m r))
wrapM f (CT v) = liftM CT $ f v
wrapM f (ZV v) = liftM CT $ f $ zvToCT' v

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

  l = wrap $ untag $ lgDispatch dl
  lInv = wrap $ untag $ lgDispatch dlinv

  mulGPow = wrap mulGPow'
  mulGDec = wrap $ untag $ lgDispatch dmulgdec

  divGPow = wrapM $ divGPow'
  -- we divide by p in the C code (for divGDec only(?)), do NOT call checkDiv!
  divGDec = wrapM $ Just . (untag $ lgDispatch dginvdec)

  crtFuncs = (,,,,) <$>
    Just (CT . repl) <*>
    (liftM wrap $ (untag $ cZipDispatch dmul) <$> untagT gCoeffsCRT) <*>
    (liftM wrap $ (untag $ cZipDispatch dmul) <$> untagT gInvCoeffsCRT) <*>
    (liftM wrap $ untagT ctCRT) <*>
    (liftM wrap $ untagT ctCRTInv) 

  twacePowDec = wrap $ runIdentity $ coerceTw twacePowDec'
  embedPow = wrap $ runIdentity $ coerceEm embedPow'
  embedDec = wrap $ runIdentity $ coerceEm embedDec'

  tGaussianDec v = liftM CT $ cDispatchGaussian v
  --tGaussianDec v = liftM CT $ coerceT' $ gaussianDec v

  -- we do not wrap thsi function because (currently) it can only be called on lifted types
  gSqNormDec (CT v) = (untag gSqNormDec') v
  gSqNormDec (ZV v) = gSqNormDec (CT $ zvToCT' v)

  crtExtFuncs = (,) <$> (liftM wrap $ coerceTw twaceCRT')
                    <*> (liftM wrap $ coerceEm embedCRT')

  coeffs = wrapM $ coerceCoeffs $ coeffs'

  powBasisPow = (CT <$>) <$> coerceBasis powBasisPow'

  crtSetDec = (CT <$>) <$> coerceBasis crtSetDec'

  fmapT f (CT v) = CT $ coerce (SV.map f) v
  fmapT f v@(ZV _) = fmapT f $ toCT v

  fmapTM f (CT (CT' v)) = liftM (CT . CT') $ SV.mapM f v
  fmapTM f v@(ZV _) = fmapTM f $ toCT v

  unzipTElt (CT (CT' v)) = (CT . CT') *** (CT . CT') $ unzip v
  unzipTElt v = unzipTElt $ toCT v

  unzipT v@(CT _) = unzipT $ toZV v
  unzipT (ZV v) = ZV *** ZV $ unzipIZV v


coerceTw :: (Functor mon) => (TaggedT '(m, m') mon (Vector r -> Vector r)) -> mon (CT' m' r -> CT' m r)
coerceTw = (coerce <$>) . untagT

coerceEm :: (Functor mon) => (TaggedT '(m, m') mon (Vector r -> Vector r)) -> mon (CT' m r -> CT' m' r)
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
  => Tagged '(m,m') ([Vector r]) -> Tagged m [CT' m' r]
coerceBasis = coerce

mulGPow' :: (TElt CT r, Fact m, Additive r) => CT' m r -> CT' m r
mulGPow' = untag $ lgDispatch dmulgpow

divGPow' :: forall m r . (TElt CT r, Fact m, IntegralDomain r, ZeroTestable r) => CT' m r -> Maybe (CT' m r)
divGPow' = untag $ checkDiv $ lgDispatch dginvpow

--{-# INLINE lgDispatch #-}
lgDispatch :: forall m r .
     (Storable r, Fact m, Additive r)
      => (Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ())
         -> Tagged m (CT' m r -> CT' m r)
lgDispatch f = do
  factors <- liftM marshalFactors ppsFact
  totm <- liftM fromIntegral totientFact
  let numFacts = fromIntegral $ SV.length factors
  return $ coerce $ \yin -> unsafePerformIO $ do -- in IO
    yout <- SV.thaw yin
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        f pout totm pfac numFacts))
    unsafeFreeze yout

gSqNormDec' :: forall m r .
     (Storable r, Fact m, Additive r, Dispatch r)
      => Tagged m (CT' m r -> r)
gSqNormDec' = do
  factors <- liftM marshalFactors ppsFact
  totm <- liftM fromIntegral totientFact
  let numFacts = fromIntegral $ SV.length factors
  return $ (! 0) . (\(CT' yin) -> unsafePerformIO $ do -- in IO
    yout <- SV.thaw yin
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        dnorm pout totm pfac numFacts))
    unsafeFreeze yout)

--{-# INLINE ctCRT #-}
ctCRT :: (Storable r, CRTrans r, Dispatch r,
          Fact m)
         => TaggedT m Maybe (CT' m r -> CT' m r)
ctCRT = do -- in TaggedT m Maybe
  ru' <- ru
  factors <- pureT $ liftM marshalFactors ppsFact
  totm <- pureT $ liftM fromIntegral totientFact
  let numFacts = fromIntegral $ SV.length factors
  return $ \(CT' yin) -> unsafePerformIO $ do -- in IO
    yout <- SV.thaw yin
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        withPtrArray ru' (\ruptr ->
          dcrt pout totm pfac numFacts ruptr)))
    CT' <$> unsafeFreeze yout

-- CTensor CRT^(-1) functions take inverse rus
--{-# INLINE ctCRTInv #-}
ctCRTInv :: (Storable r, CRTrans r, Dispatch r,
          Fact m)
         => TaggedT m Maybe (CT' m r -> CT' m r)
ctCRTInv = do -- in Maybe
  mhatInv <- liftM snd $ crtInfoFact
  ruinv' <- ruInv
  factors <- pureT $ liftM marshalFactors ppsFact
  totm <- pureT $ liftM fromIntegral totientFact
  let numFacts = fromIntegral $ SV.length factors
  -- EAC: can't use coerce here?
  return $ \(CT' yin) -> unsafePerformIO $ do
    yout <- SV.thaw yin
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
        withPtrArray ruinv' (\ruptr ->
          with mhatInv (\minv -> 
            dcrtinv pout totm pfac numFacts ruptr minv))))
    CT' <$> unsafeFreeze yout

checkDiv :: forall m r . 
  (IntegralDomain r, Storable r, ZeroTestable r, 
   Fact m)
    => Tagged m (CT' m r -> CT' m r) -> Tagged m (CT' m r -> Maybe (CT' m r))
checkDiv f = do
  f' <- f
  oddRad' <- liftM fromIntegral oddRadicalFact
  return $ \x -> 
    let (CT' y) = f' x
    in CT' <$> (SV.mapM (`divIfDivis` oddRad')) y

divIfDivis :: (IntegralDomain r, ZeroTestable r) => r -> r -> Maybe r
divIfDivis num den = let (q,r) = num `divMod` den
                     in if isZero r then Just q else Nothing

cZipDispatch :: (Storable r, Fact m, Additive r)
  => (Ptr r -> Ptr r -> Int64 -> IO ())
     -> Tagged m (CT' m r -> CT' m r -> CT' m r)
cZipDispatch f = do -- in Tagged m
  totm <- liftM fromIntegral $ totientFact
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
cDispatchGaussian var = liftM CT' $ flip proxyT (Proxy::Proxy m) $ do -- in TaggedT m rnd
  -- get rus for (Complex r)
  ruinv' <- mapTaggedT (return . fromMaybe (error "complexGaussianRoots")) $ ruInv
  factors <- liftM marshalFactors $ pureT ppsFact
  totm <- pureT totientFact
  m <- pureT valueFact
  rad <- pureT radicalFact
  yin <- lift $ realGaussians (var * fromIntegral (m `div` rad)) totm
  let numFacts = fromIntegral $ SV.length factors
  return $ unsafePerformIO $ do -- in IO
    --let yin = create $ SM.new totm :: Vector r -- contents will be overwritten, so no need to initialize
    yout <- SV.thaw yin
    SM.unsafeWith yout (\pout ->
      SV.unsafeWith factors (\pfac ->
       withPtrArray ruinv' (\ruptr ->
        dgaussdec pout (fromIntegral totm) pfac numFacts ruptr)))
    unsafeFreeze yout

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
  random = runRand $ liftM CT (liftRand random)

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
        in liftM coerce . SV.replicateM n

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
  wPow <- liftM fst $ crtInfoFact
  liftM (LP.map
    (\(p,e) -> do
        let pp = p^e
            pow = mval `div` pp
        generate pp (wPow . (*pow)))) $
      pureT ppsFact

--{-# INLINE ruInv #-}
ruInv = do
  mval <- pureT valueFact
  wPow <- liftM fst $ crtInfoFact
  liftM (LP.map
    (\(p,e) -> do
        let pp = p^e
            pow = mval `div` pp
        generate pp (\i -> wPow $ (-i*pow)))) $
      pureT ppsFact

gCoeffsCRT, gInvCoeffsCRT :: (TElt CT r, CRTrans r, Fact m, ZeroTestable r, IntegralDomain r)
  => TaggedT m Maybe (CT' m r)
gCoeffsCRT = ctCRT <*> (return $ mulGPow' $ scalarPow' LP.one)
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












-- C-backend support

marshalFactors :: [PP] -> Vector CPP
marshalFactors = SV.fromList . LP.map (\(p,e) -> CPP (fromIntegral p) (fromIntegral e))

-- http://stackoverflow.com/questions/6517387/vector-vector-foo-ptr-ptr-foo-io-a-io-a
withPtrArray :: (Storable a) => [Vector a] -> (Ptr (Ptr a) -> IO b) -> IO b
withPtrArray v f = do
  let vs = LP.map SV.unsafeToForeignPtr0 v
      ptrV = LP.map (\(fp,_) -> getPtr fp) vs
  res <- withArray ptrV f
  LP.mapM_ (\(fp,_) -> touchForeignPtr fp) vs
  return res

data CPP = CPP {p' :: !Int32, e' :: !Int16}
-- stolen from http://hackage.haskell.org/packages/archive/numeric-prelude/0.4.0.3/doc/html/src/Number-Complex.html#T
-- the NumericPrelude Storable instance for complex numbers
instance Storable CPP where
   sizeOf    = Store.sizeOf store
   alignment = Store.alignment store
   peek      = Store.peek store
   poke      = Store.poke store

store :: Store.Dictionary CPP
store = Store.run $
   liftA2 CPP
      (Store.element p')
      (Store.element e')

instance Show CPP where
    show (CPP p e) = "(" LP.++ (show p) LP.++ "," LP.++ (show e) LP.++ ")"

foreign import ccall unsafe "tensorLR" tensorLR ::                  Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::            Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLRq" tensorLRq ::                Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorLInvRq" tensorLInvRq ::          Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLC" tensorLC ::       Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvC" tensorLInvC :: Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorNormSqR" tensorNormSqR ::     Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorGPowR" tensorGPowR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGPowRq" tensorGPowRq ::       Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorGPowC" tensorGPowC ::         Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecR" tensorGDecR ::         Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecRq" tensorGDecRq ::       Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorGInvPowR" tensorGInvPowR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvPowRq" tensorGInvPowRq :: Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorGInvPowC" tensorGInvPowC ::   Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvDecR" tensorGInvDecR ::   Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvDecRq" tensorGInvDecRq :: Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr Int64 -> IO ()

foreign import ccall unsafe "tensorCRTRq" tensorCRTRq ::         Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorCRTC" tensorCRTC ::           Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> IO ()
foreign import ccall unsafe "tensorCRTInvRq" tensorCRTInvRq ::   Int16 -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) -> Ptr Int64 -> IO ()
foreign import ccall unsafe "tensorCRTInvC" tensorCRTInvC ::     Int16 -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> IO ()

foreign import ccall unsafe "tensorGaussianDec" tensorGaussianDec :: Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) ->  IO ()

foreign import ccall unsafe "mulRq" mulRq :: Int16 -> Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "mulC" mulC :: Int16 -> Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()

foreign import ccall unsafe "addRq" addRq :: Int16 -> Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr Int64 -> IO ()
foreign import ccall unsafe "addR" addR :: Int16 -> Ptr Int64 -> Ptr Int64 -> Int64 -> IO ()
foreign import ccall unsafe "addC" addC :: Int16 -> Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()
foreign import ccall unsafe "addD" addD :: Int16 -> Ptr Double -> Ptr Double -> Int64 -> IO ()

type Dispatch r = (Dispatch' (CTypeOf r) r)

-- | Class to safely match Haskell types with the appropriate C function.
class (repr ~ CTypeOf r) => Dispatch' repr r where
  dcrt :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr r) -> IO ()
  dcrtinv :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr r) -> Ptr r -> IO ()
  dl :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dlinv :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dnorm :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dmulgpow :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dmulgdec :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dginvpow :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dginvdec :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  dadd :: Ptr r -> Ptr r -> Int64 -> IO ()
  dmul :: Ptr r -> Ptr r -> Int64 -> IO ()
  dgaussdec :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex r)) -> IO ()

instance (GetModPairs r, Storable (ModPairs r),
          RingProduct r, CTypeOf r ~ ZqB64D)
  => Dispatch' ZqB64D r where
  dcrt pout totm pfac numFacts ruptr = 
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorCRTRq numPairs (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr qsptr)
  dcrtinv pout totm pfac numFacts ruptr minv =
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorCRTInvRq numPairs (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv) (castPtr qsptr)
  dl pout totm pfac numFacts = 
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorLRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dlinv pout totm pfac numFacts = 
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorLRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dnorm = error "cannot call CT normSq on type ZqBasic"
  dmulgpow pout totm pfac numFacts =
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorGPowRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dmulgdec pout totm pfac numFacts =
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorGDecRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dginvpow pout totm pfac numFacts =
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorGInvPowRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dginvdec pout totm pfac numFacts =
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        tensorGInvDecRq numPairs (castPtr pout) totm pfac numFacts (castPtr qsptr)
  dadd aout bout totm = 
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        addRq numPairs (castPtr aout) (castPtr bout) totm (castPtr qsptr)
  dmul aout bout totm = 
    let qs = proxy getModuli (Proxy::Proxy r)
        numPairs = proxy numComponents (Proxy::Proxy r)
    in with qs $ \qsptr ->
        mulRq numPairs (castPtr aout) (castPtr bout) totm (castPtr qsptr)
  dgaussdec = error "cannot call CT gaussianDec on type ZqBasic"

instance (RingProduct r, CTypeOf r ~ ComplexD) => Dispatch' ComplexD r where
  dcrt pout totm pfac numFacts ruptr = 
    tensorCRTC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr)
  dcrtinv pout totm pfac numFacts ruptr minv = 
    tensorCRTInvC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv)
  dl pout totm pfac numFacts = 
    tensorLC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dlinv pout totm pfac numFacts = 
    tensorLInvC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dnorm = error "cannot call CT normSq on type Complex Double"
  dmulgpow pout totm pfac numFacts = 
    tensorGPowC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dmulgdec = error "cannot call CT mulGDec on type Complex Double"
  dginvpow pout totm pfac numFacts = 
    tensorGInvPowC (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dginvdec = error "cannot call CT divGDec on type Complex Double"
  dadd aout bout totm = 
    addC (proxy numComponents (Proxy::Proxy r)) (castPtr aout) (castPtr bout) totm
  dmul aout bout totm = 
    mulC (proxy numComponents (Proxy::Proxy r)) (castPtr aout) (castPtr bout) totm
  dgaussdec = error "cannot call CT gaussianDec on type Comple Double"

instance (RingProduct r, CTypeOf r ~ DoubleD) => Dispatch' DoubleD r where
  dcrt = error "cannot call CT Crt on type Double"
  dcrtinv = error "cannot call CT CrtInv on type Double"
  dl pout totm pfac numFacts = 
    tensorLDouble (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dlinv pout totm pfac numFacts = 
    tensorLInvDouble (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dnorm = error "cannot call CT normSq on type Double"
  dmulgpow = error "cannot call CT mulGPow on type Double"
  dmulgdec = error "cannot call CT mulGDec on type Double"
  dginvpow = error "cannot call CT divGPow on type Double"
  dginvdec = error "cannot call CT divGDec on type Double"
  dadd aout bout totm = 
    addD (proxy numComponents (Proxy::Proxy r)) (castPtr aout) (castPtr bout) totm
  dmul = error "cannot call CT (*) on type Double"
  dgaussdec pout totm pfac numFacts ruptr = 
    tensorGaussianDec (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts (castPtr ruptr)

instance (RingProduct r, CTypeOf r ~ Int64D) => Dispatch' Int64D r where
  dcrt = error "cannot call CT Crt on type Int64"
  dcrtinv = error "cannot call CT CrtInv on type Int64"
  dl pout totm pfac numFacts = 
    tensorLR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dlinv pout totm pfac numFacts = 
    tensorLInvR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dnorm pout totm pfac numFacts = 
    tensorNormSqR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dmulgpow pout totm pfac numFacts = 
    tensorGPowR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dmulgdec pout totm pfac numFacts = 
    tensorGDecR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dginvpow pout totm pfac numFacts = 
    tensorGInvPowR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dginvdec pout totm pfac numFacts = 
    tensorGInvDecR (proxy numComponents (Proxy::Proxy r)) (castPtr pout) totm pfac numFacts
  dadd aout bout totm = 
    addR (proxy numComponents (Proxy::Proxy r)) (castPtr aout) (castPtr bout) totm
  dmul = error "cannot call CT (*) on type Int64"
  dgaussdec = error "cannot call CT gaussianDec on type Int64"

class GetModPairs a where
  type ModPairs a
  getModuli :: Tagged a (ModPairs a)

instance (Reflects q Int64) => GetModPairs (ZqBasic q Int64) where
  type ModPairs (ZqBasic q Int64) = Int64
  getModuli = tag $ proxy value (Proxy::Proxy q)

instance (GetModPairs b, Reflects q Int64) => GetModPairs (ZqBasic q Int64, b) where
  type ModPairs (ZqBasic q Int64,b) = (Int64, ModPairs b)
  getModuli = 
    let q = proxy value (Proxy::Proxy q)
        qs = proxy getModuli (Proxy :: Proxy b)
    in tag $ (q,qs)

class RingProduct a where
  numComponents :: Tagged a Int16

instance {-# Overlappable #-} RingProduct a where
  numComponents = tag 1

instance (RingProduct a, RingProduct b) => RingProduct (a,b) where
  numComponents = tag $ (proxy numComponents (Proxy::Proxy a)) + (proxy numComponents (Proxy::Proxy b))

data ZqB64D -- for type safety purposes
data ComplexD
data DoubleD
data Int64D

type family BasicTypeOf x where
  BasicTypeOf (ZqBasic q Int64) = ZqB64D
  BasicTypeOf Double = DoubleD
  BasicTypeOf Int64 = Int64D
  BasicTypeOf (Complex Double) = ComplexD

type family CTypeOf x where
  CTypeOf (a,b) = BasicTypeOf a
  CTypeOf a = BasicTypeOf a


instance (Storable a, Storable b,
          BasicTypeOf a ~ CTypeOf b) 
  -- enforces right associativity and that each type of 
  -- the tuple has the same C repr, so using an array repr is safe
  => Storable (a,b) where
  sizeOf _ = (sizeOf (undefined :: a)) + (sizeOf (undefined :: b))
  alignment _ = max (alignment (undefined :: a)) (alignment (undefined :: b))
  peek p = do
    a <- peek (castPtr p :: Ptr a)
    b <- peek (castPtr (plusPtr p (sizeOf a)) :: Ptr b)
    return (a,b)
  poke p (a,b) = do
    poke (castPtr p :: Ptr a) a
    poke (castPtr (plusPtr p (sizeOf a)) :: Ptr b) b
