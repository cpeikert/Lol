{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             NoImplicitPrelude, RankNTypes, RebindableSyntax, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

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
  []

-- generate a rounded error term
bench_gensk :: forall t m r rnd . (GenSKCtx t m (LiftOf r) Double, CryptoRandomGen rnd) 
  => Double -> Proxy rnd -> Proxy (t m r) -> Benchmarkable
bench_gensk v _ _ = nfIO $ do
  gen <- newGenIO -- uses system entropy
  return $ evalRand (genSK v :: Rand (CryptoRand rnd) (SK (Cyc t m (LiftOf r)))) gen

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

bench_mulCycle :: forall gad z zp zq zq' zq'' t m m' rnd . (MonadRandom rnd,
                   Ring (CT m zp (Cyc t m' zq')), NFData (CT m zp (Cyc t m' zq)),
                   RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq',
                   KeySwitchCtx gad t m' zp zq' zq'', KSHintCtx gad t m' z zq'') 
  => Proxy zq -> Proxy (gad,zq'') -> SK (Cyc t m' z) -> CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq') -> rnd Benchmarkable
-- EAC: May need to change to output MBench rnd
bench_mulCycle _ psw sk a b = do
  kswq :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq') <- proxyT (keySwitchQuadCirc sk) psw
  return $ nf (rescaleLinearCT . kswq . (a*) :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq)) b

-- generates a secrete key with svar=1, using non-cryptographic randomness
instance (GenSKCtx t m z Double, MonadRandom rnd, MonadState (Maybe (SK (Cyc t m z))) rnd, GenArgs rnd b)
  => GenArgs rnd (SK (Cyc t m z) -> b) where
  genArgs f = do
    msk <- get
    sk <- case msk of
      Just sk -> return sk
      Nothing -> do
        sk <- genSK (1 :: Double)
        put $ Just sk
        return sk
    genArgs $ f sk

instance (GenSKCtx t m z Double, 
          EncryptCtx t m m' z zp zq, 
          MonadRandom rnd, 
          MonadState (Maybe (SK (Cyc t m' z))) rnd, 
          GenArgs rnd b) 
  => GenArgs rnd (CT m zp (Cyc t m' zq) -> b) where
  genArgs f = genArgs $ (\sk pt -> MBench $ (genArgs =<< (f <$> (encrypt sk pt))) :: MBench rnd)
