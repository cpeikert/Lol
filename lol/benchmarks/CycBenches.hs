{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module CycBenches (cycBenches) where

import Apply.Cyc
import Benchmarks
import Utils

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar ()

cycBenches :: IO Benchmark
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
type MRCombos =
  '[ '(F1024, Zq 1051649),      -- 1024 / 512
     '(F2048, Zq 1054721),      -- 2048 / 1024
     '(F64 * F27, Zq 1048897),  -- 1728 / 576
     '(F64 * F81, Zq 1073089),  -- 5184 / 1728
     '(F64*F9*F25, Zq 1065601)  -- 14400 / 3840
    ]

type MM'RCombos =
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]

-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> MRCombos
allParams :: Proxy AllParams
allParams = Proxy

type LiftParams = ( '(,) <$> Tensors) <*> MRCombos
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
