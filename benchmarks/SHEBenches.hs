{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NoImplicitPrelude, RankNTypes, RebindableSyntax, ScopedTypeVariables,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module SHEBenches (sheBenches) where

import Criterion
import Utils

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Crypto.Lol hiding (CT)
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Applications.SymmSHE

sheBenches :: (MonadRandom rnd) => rnd Benchmark
sheBenches = bgroupRnd "SHE"
  [bgroupRnd "genSK,v=0.1" $ groupGens $ wrapGenSK $ bench_gensk 0.1,
   bgroupRnd "genSK,v=1.0" $ groupGens $ wrapGenSK $ bench_gensk 1.0]

-- generate a rounded error term
bench_gensk :: forall t m gen . (GenSKCtx t m Int64 Double, CryptoRandomGen gen) 
  => Double -> Proxy gen -> Proxy t -> Proxy m -> Benchmarkable
bench_gensk v _ _ _ = nfIO $ do
  gen <- newGenIO -- uses system entropy
  return $ evalRand (genSK v :: Rand (CryptoRand gen) (SK (Cyc t m Int64))) gen

bench_enc :: forall t m m' z zp zq gen . (EncryptCtx t m m' z zp zq, CryptoRandomGen gen)
  => Proxy gen -> Proxy zq -> SK (Cyc t m' z) -> PT (Cyc t m zp) -> Benchmarkable
bench_enc _ _ sk pt = nfIO $ do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen

bench_dec :: (DecryptCtx t m m' z zp zq) => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> Benchmarkable
bench_dec sk ct = nf (decrypt sk) ct

bench_mul :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> Benchmarkable
bench_mul a = nf (*a)

bench_keySwQ :: KSHint m zp t m' zq' gad zq'' -> CT m zp (Cyc t m' zq') -> Benchmarkable
bench_keySwQ (KeySwitch kswq) x = nf kswq $ x*x

bench_rescaleCT :: (RescaleCyc (Cyc t) zq' zq, NFData (CT m zp (Cyc t m' zq))) 
  => Proxy zq -> CT m zp (Cyc t m' zq') -> Benchmarkable
bench_rescaleCT _ = nf rescaleLinearCT

newtype KSHint m zp t m' zq gad zq' = KeySwitch (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq))

-- sanity check: should take as long as the sum of its component tests
bench_mulCycle :: forall gad z zp zq zq' zq'' t m m' . (
                   Ring (CT m zp (Cyc t m' zq')), NFData (CT m zp (Cyc t m' zq)),
                   RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq', z~ModRep zp,
                   KeySwitchCtx gad t m' zp zq' zq'') 
  => Proxy zq
     -> KSHint m zp t m' zq' gad zq''
     -> CT m zp (Cyc t m' zq')
     -> CT m zp (Cyc t m' zq')
     -> Benchmarkable
bench_mulCycle _ (KeySwitch kswq) a b = 
  nf (rescaleLinearCT . kswq . (a*) :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq)) b













wrapGenSK :: forall t m gen rnd . (GenSKCtx t m Int64 Double, CryptoRandomGen gen, Monad rnd)
  => (Proxy gen -> Proxy t -> Proxy m -> Benchmarkable) 
     -> Proxy gen -> Proxy t -> Proxy m -> rnd Benchmark
wrapGenSK f _ _ _ = return $ bench (show (BT :: BenchType m)) $ f Proxy Proxy Proxy

























groupGens :: (Monad rnd) => 
  (forall t m gen .
      (GenSKCtx t m Int64 Double, CryptoRandomGen gen)
      => Proxy gen
         -> Proxy t
         -> Proxy m
         -> rnd Benchmark)
  -> [rnd Benchmark]
groupGens f = 
  [bgroupRnd "HashDRBG" $ groupCLift $ f (Proxy :: Proxy HashDRBG),
   bgroupRnd "SysRand" $ groupCLift $ f (Proxy :: Proxy SystemRandom)]

groupCLift :: (Monad rnd) =>
  (forall t m . 
       (GenSKCtx t m Int64 Double) 
       => Proxy t 
          -> Proxy m
          -> rnd Benchmark)
  -> [rnd Benchmark]
groupCLift f =
  [bgroupRnd "Cyc CT" $ groupMRLift $ f (Proxy::Proxy CT.CT),
   bgroupRnd "Cyc RT" $ groupMRLift $ f (Proxy::Proxy RT)]

groupMRLift :: (Monad rnd) =>
  (forall m . (GenSKCtx CT.CT m Int64 Double, GenSKCtx RT m Int64 Double) => Proxy m -> rnd Benchmark) 
  -> [rnd Benchmark]
groupMRLift f = 
  [f (Proxy::Proxy F128),
   f (Proxy::Proxy (PToF Prime281)),
   f (Proxy::Proxy (F32 * F9)),
   f (Proxy::Proxy (F32 * F9)),
   f (Proxy::Proxy (F32 * F9 * F25))]












-- generates a secrete key with svar=1, using non-cryptographic randomness
instance (GenSKCtx t m z Double, 
          MonadRandom rnd, 
          MonadState (Maybe (SK (Cyc t m z))) rnd)
  => GenArg rnd (SK (Cyc t m z)) where
  genArg = do
    msk <- get
    sk <- case msk of
      Just sk -> return sk
      Nothing -> do
        sk <- genSK (1 :: Double)
        put $ Just sk
        return sk
    return sk

instance (EncryptCtx t m m' z zp zq,
          z ~ LiftOf zp,
          MonadRandom rnd,
          GenArg rnd (SK (Cyc t m' z)),
          GenArg rnd (Cyc t m zp)) 
  => GenArg rnd (CT m zp (Cyc t m' zq)) where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    pt <- genArg
    encrypt sk pt

instance (GenArg rnd (SK (Cyc t m' z)),
          z ~ LiftOf zp,
          KeySwitchCtx gad t m' zp zq zq',
          KSHintCtx gad t m' z zq', 
          MonadRandom rnd)
  => GenArg rnd (KSHint m zp t m' zq gad zq') where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    KeySwitch <$> proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad,zq'))
