{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TensorBenches (tensorBenches1, tensorBenches2) where

import Apply.Cyc
import Benchmarks
import BenchConfig

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random.DRBG

{-# INLINE tensorBenches1 #-}
tensorBenches1 p = benchGroup "Tensor" $ ($ p) <$> [
  hideArgs "unzipPow" bench_unzip,
  hideArgs "unzipDec" bench_unzip,
  hideArgs "unzipCRT" bench_unzip,
  hideArgs "zipWith (*)" bench_mul,
  hideArgs "crt" bench_crt,
  hideArgs "crtInv" bench_crtInv,
  hideArgs "l" bench_l,
  hideArgs "lInv" bench_lInv,
  hideArgs "*g Pow" bench_mulgPow,
  hideArgs "*g Dec" bench_mulgDec,
  hideArgs "*g CRT" bench_mulgCRT,
  hideArgs "divg Pow" bench_divgPow,
  hideArgs "divg Dec" bench_divgDec,
  hideArgs "divg CRT" bench_divgCRT,
  hideArgs "lift" bench_liftPow,
  hideArgs "error" (bench_errRounded 0.1)
  ]
{-# INLINE tensorBenches2 #-}
tensorBenches2 p = benchGroup "Tensor" $ ($ p) <$> [
  hideArgs "twacePow" bench_twacePow,
  hideArgs "twaceDec" bench_twacePow, -- yes, twacePow is correct here. It's the same function!
  hideArgs "twaceCRT" bench_twaceCRT,
  hideArgs "embedPow" bench_embedPow,
  hideArgs "embedDec" bench_embedDec,
  hideArgs "embedCRT" bench_embedCRT
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

-- multiply by g when input is in Dec basis
bench_mulgDec :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgDec = bench mulGDec

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgCRT = bench (fromJust' "TensorBenches.bench_mulgCRT" mulGCRT)

-- divide by g when input is in Pow basis
bench_divgPow :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgPow x =
  let y = mulGPow x
  in bench divGPow y

-- divide by g when input is in Dec basis
bench_divgDec :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgDec x =
  let y = mulGDec x
  in bench divGDec y

-- divide by g when input is in CRT basis
bench_divgCRT :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgCRT = bench (fromJust' "TensorBenches.bench_divgCRT" divGCRT)

-- generate a rounded error term
bench_errRounded :: forall t m r . (ErrorCtx t m r Gen)
  => Double -> Bench '(t,m,r)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand
    (fmapT (roundMult one) <$>
      (tGaussianDec v :: Rand (CryptoRand Gen) (t m Double)) :: Rand (CryptoRand Gen) (t m (LiftOf r))) gen

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

bench_embedCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedCRT = bench (fromJust' "TensorBenches.bench_embedCRT" embedCRT :: t m r -> t m' r)
