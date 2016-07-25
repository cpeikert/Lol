{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorBenches (tensorBenches) where

import Apply.Cyc
import Benchmarks
import BenchParams

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random.DRBG

tensorBenches :: IO Benchmark
tensorBenches = benchGroup "Tensor" [
  benchGroup "unzipPow"    $ [hideArgs bench_unzip testParam],
  benchGroup "unzipDec"    $ [hideArgs bench_unzip testParam],
  benchGroup "unzipCRT"    $ [hideArgs bench_unzip testParam], --applyUnzip  allParams    $ hideArgs bench_unzip,
  benchGroup "zipWith (*)" $ [hideArgs bench_mul testParam], --applyBasic  allParams    $ hideArgs bench_mul,
  benchGroup "crt"         $ [hideArgs bench_crt testParam], --applyBasic  allParams    $ hideArgs bench_crt,
  benchGroup "crtInv"      $ [hideArgs bench_crtInv testParam], --applyBasic  allParams    $ hideArgs bench_crtInv,
  benchGroup "l"           $ [hideArgs bench_l testParam], --applyBasic  allParams    $ hideArgs bench_l,
  benchGroup "lInv"        $ [hideArgs bench_lInv testParam],
  benchGroup "*g Pow"      $ [hideArgs bench_mulgPow testParam], --applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "*g CRT"      $ [hideArgs bench_mulgCRT testParam], --applyBasic  allParams    $ hideArgs bench_mulgCRT,
  benchGroup "lift"        $ [hideArgs bench_liftPow testParam], --applyLift   liftParams   $ hideArgs bench_liftPow,
  benchGroup "error"       $ [hideArgs (bench_errRounded 0.1) testParam'], --applyError  errorParams  $ hideArgs $ bench_errRounded 0.1,
  benchGroup "twacePow"    $ [hideArgs bench_twacePow twoIdxParam], --applyTwoIdx twoIdxParams $ hideArgs bench_twacePow,
  benchGroup "twaceCRT"    $ [hideArgs bench_twaceCRT twoIdxParam],
  benchGroup "embedPow"    $ [hideArgs bench_embedPow twoIdxParam], --applyTwoIdx twoIdxParams $ hideArgs bench_embedPow-}
  benchGroup "embedDec"    $ [hideArgs bench_embedDec twoIdxParam]
  ]

bench_unzip :: (UnzipCtx t m r) => t m (r,r) -> Bench '(t,m,r)
bench_unzip = bench unzipT

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => t m r -> t m r -> Bench '(t,m,r)
bench_mul a = bench (zipWithT (*) a)

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_crt = bench (fromJust' "TensorBenches.bench_crt" crt)

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_crtInv = bench (fromJust' "TensorBenches.bench_crtInv" crtInv)

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_l = bench l

-- convert input from Dec basis to Pow basis
bench_lInv :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_lInv = bench lInv

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => t m r -> Bench '(t,m,r)
bench_liftPow = bench (fmapT lift)

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgPow = bench mulGPow

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgCRT = bench (fromJust' "TensorBenches.bench_mulgCRT" mulGCRT)

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand
    (fmapT (roundMult one) <$>
      (tGaussianDec v :: Rand (CryptoRand gen) (t m Double)) :: Rand (CryptoRand gen) (t m (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m' r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePowDec :: t m' r -> t m r)

bench_twaceCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m' r -> Bench '(t,m,m',r)
bench_twaceCRT = bench (fromJust' "TensorBenches.bench_twaceCRT" twaceCRT :: t m' r -> t m r)

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: t m r -> t m' r)

bench_embedDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedDec = bench (embedDec :: t m r -> t m' r)
