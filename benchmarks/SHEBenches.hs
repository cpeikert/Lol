{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, 
             MultiParamTypeClasses, NoImplicitPrelude, RankNTypes, RebindableSyntax, 
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module SHEBenches (sheBenches) where

import Utils
import Harness.SHE
import SHETests

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.State (StateT, evalStateT)
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Crypto.Lol hiding (CT)
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Applications.SymmSHE

import Data.Promotion.Prelude.List

sheBenches :: (MonadRandom rnd) => rnd Benchmark
sheBenches = bgroupRnd "SHE" [
   {-bgroupRnd "encrypt"   $ benchEnc    $ wrapEnc bench_enc,
   bgroupRnd "*"         $ benchCTFunc $ wrapMul bench_mul,
   bgroupRnd "addPublic" $ benchCTFunc $ wrapPublic bench_addPublic,
   bgroupRnd "mulPublic" $ benchCTFunc $ wrapPublic bench_mulPublic,-}
   bgroupRnd "rescaleCT" $ benchRescale (Proxy::Proxy RescaleParams) $ wrap' bench_rescaleCT,
   bgroupRnd "keySwitch" $ benchKSQ (Proxy::Proxy KSQParams) $ wrap' bench_keySwQ
   ]

bench_enc :: forall t m m' z zp zq gen . (EncryptCtx t m m' z zp zq, CryptoRandomGen gen)
  => Proxy gen -> Proxy zq -> SK (Cyc t m' z) -> PT (Cyc t m zp) -> NFValue
bench_enc _ _ sk pt = nfIO $ do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen

bench_mul :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> NFValue
bench_mul a = nf (*a)

bench_addPublic :: (AddPublicCtx t m m' zp zq) => Cyc t m zp -> CT m zp (Cyc t m' zq) -> NFValue
bench_addPublic a ct = nf (addPublic a) ct

bench_mulPublic :: (MulPublicCtx t m m' zp zq) => Cyc t m zp -> CT m zp (Cyc t m' zq) -> NFValue
bench_mulPublic a ct = nf (mulPublic a) ct

-- requires zq to be Liftable
bench_dec :: (DecryptCtx t m m' z zp zq) => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> NFValue
bench_dec sk ct = nf (decrypt sk) ct

bench_rescaleCT :: forall t m m' zp zq zq' . 
  (RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq', NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq') -> NFValue' '(t,m,m',zp,zq,zq')
bench_rescaleCT = nfv (rescaleLinearCT :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq))

bench_keySwQ :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq))) 
  => KSHint m zp t m' zq gad zq' -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq,zq',gad)
bench_keySwQ (KeySwitch kswq) x = nfv kswq $ x*x


data CTCtxD
-- union of compatible constraints in benchmarks
type CTCtx t m m' zp zq = 
  (EncryptCtx t m m' (LiftOf zp) zp zq,
   Ring (CT m zp (Cyc t m' zq)),
   NFData (CT m zp (Cyc t m' zq)),
   AddPublicCtx t m m' zp zq,
   MulPublicCtx t m m' zp zq,
   ShowArgs '(t,m,m',zp,zq))
instance (params `Satisfy` CTCtxD, CTCtx t m m' zp zq) 
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` CTCtxD where
  data ArgsCtx CTCtxD where
    CTD :: (CTCtx t m m' zp zq) 
      => Proxy '(t,m,m',zp,zq) -> ArgsCtx CTCtxD
  run _ f = (f $ CTD (Proxy::Proxy '(t,m,m',zp,zq))) : (run (Proxy::Proxy params) f)

hideCT :: (forall t m m' zp zq . (CTCtx t m m' zp zq) 
  => Proxy '(t,m,m',zp,zq) -> rnd Benchmark) -> ArgsCtx CTCtxD -> rnd Benchmark
hideCT f (CTD p) = f p

benchCTFunc :: (forall t m m' zp zq . (CTCtx t m m' zp zq) 
  => Proxy '(t,m,m',zp,zq) -> rnd Benchmark)
    -> [rnd Benchmark]
benchCTFunc g = run (Proxy::Proxy CTParams) $ hideCT g

benchEnc :: (Monad rnd)
  => (forall t m m' zp zq gen . (CTCtx t m m' zp zq, CryptoRandomGen gen) 
        => Proxy gen -> Proxy '(t,m,m',zp,zq) -> rnd Benchmark)
     -> [rnd Benchmark]
benchEnc g = [
  bgroupRnd "HashDRBG" $ benchCTFunc $ g (Proxy::Proxy HashDRBG),
  bgroupRnd "SysRand"  $ benchCTFunc $ g (Proxy::Proxy SystemRandom)]

















type Gadgets = '[TrivGad, BaseBGad 2]
type Tensors = '[CT.CT,RT]
type MM'PQCombos = 
  '[ '(F4, F128, Zq 64, Zq 257),
     '(F4, F128, Zq 64, Zq (257 ** 641)),
     '(F12, F32 * F9, Zq 64, Zq 577),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 64, Zq 14401),
     '(F12, F32 * F9 * F25, Zq 64, Zq (14401 ** 21601))
    ]

type CTParams  = ( '(,) <$> Tensors) <*> MM'PQCombos
type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable MM'PQCombos))
type RescaleParams = ( '(,) <$> Tensors) <*> (Map AddZq (Filter NonLiftable MM'PQCombos))
type KSQParams = ( '(,) <$> Gadgets) <*> RescaleParams
