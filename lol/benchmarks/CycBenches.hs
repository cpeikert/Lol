{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module CycBenches (cycBenches) where

import Benchmarks
import Harness.Cyc
import Utils

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar () -- required for singletons below

cycBenches :: (MonadRandom m) => m Benchmark
cycBenches = benchGroup "Cyc" [
  benchGroup "unzipCycPow" $ applyBasic  allParams    $ hideArgs bench_unzipCycPow,
  benchGroup "unzipCycCRT" $ applyBasic  allParams    $ hideArgs bench_unzipCycCRT,
  benchGroup "*"           $ applyBasic  allParams    $ hideArgs bench_mul,
  benchGroup "crt"         $ applyBasic  allParams    $ hideArgs bench_crt,
  benchGroup "crtInv"      $ applyBasic  allParams    $ hideArgs bench_crtInv,
  benchGroup "l"           $ applyBasic  allParams    $ hideArgs bench_l,
  benchGroup "*g Pow"      $ applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "*g CRT"      $ applyBasic  allParams    $ hideArgs bench_mulgCRT,
  benchGroup "lift"        $ applyLift   liftParams   $ hideArgs bench_liftPow,
  benchGroup "error"       $ applyError  errorParams  $ hideArgs $ bench_errRounded 0.1,
  benchGroup "twace"       $ applyTwoIdx twoIdxParams $ hideArgs bench_twacePow,
  benchGroup "embed"       $ applyTwoIdx twoIdxParams $ hideArgs bench_embedPow
  ]

bench_unzipCycPow :: (BasicCtx t m r, BasicCtx t m (r,r)) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycPow a =
  let a' = advisePow a
  in bench unzipCyc a'

bench_unzipCycCRT :: (BasicCtx t m r, BasicCtx t m (r,r)) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycCRT a =
  let a' = adviseCRT a
  in bench unzipCyc a'

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = adviseCRT a
      b' = adviseCRT b
  in bench (a' *) b'

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_crt x = let y = advisePow x in bench adviseCRT y

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_crtInv x = let y = adviseCRT x in bench advisePow y

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_l x = let y = adviseDec x in bench advisePow y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_liftPow x = let y = advisePow x in bench (liftCyc Pow :: Cyc t m r -> Cyc t m (LiftOf r)) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgPow x = let y = advisePow x in bench mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgCRT x = let y = adviseCRT x in bench mulG y

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twacePow x =
  let y = advisePow x
  in bench (twace :: Cyc t m' r -> Cyc t m r) y

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedPow x =
  let y = advisePow x
  in bench (embed :: Cyc t m r -> Cyc t m' r) y

type Tensors = '[CT,RT]
type MM'RCombos =
  '[ '(F4, F128, Zq 257),
     '(F1, PToF Prime281, Zq 563),
     '(F12, F32 * F9, Zq 512),
     '(F12, F32 * F9, Zq 577),
     '(F12, F32 * F9, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 14401)
    ]
-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> (Nub (Map RemoveM MM'RCombos))
allParams :: Proxy AllParams
allParams = Proxy

type LiftParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable (Map RemoveM MM'RCombos)))
liftParams :: Proxy LiftParams
liftParams = Proxy

type TwoIdxParams = ( '(,) <$> Tensors) <*> MM'RCombos
twoIdxParams :: Proxy TwoIdxParams
twoIdxParams = Proxy

type ErrorParams = ( '(,) <$> '[HashDRBG]) <*> LiftParams
errorParams :: Proxy ErrorParams
errorParams = Proxy

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m',r) = Int64 :== (LiftOf r)

data RemoveM :: TyFun (Factored, Factored, *) (Factored, *) -> *
type instance Apply RemoveM '(m,m',r) = '(m',r)
