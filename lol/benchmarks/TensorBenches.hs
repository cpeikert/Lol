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

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random.DRBG

{-# INLINABLE tensorBenches1 #-}
tensorBenches1 ptmr pgen = benchGroup "Tensor" $ ($ ptmr) <$> [
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
  hideArgs "error" (bench_errRounded 0.1) . addGen pgen
  ]
{-# INLINABLE tensorBenches2 #-}
tensorBenches2 p = benchGroup "Tensor" $ ($ p) <$> [
  hideArgs "twacePow" bench_twacePow,
  hideArgs "twaceDec" bench_twacePow, -- yes, twacePow is correct here. It's the same function!
  hideArgs "twaceCRT" bench_twaceCRT,
  hideArgs "embedPow" bench_embedPow,
  hideArgs "embedDec" bench_embedDec,
  hideArgs "embedCRT" bench_embedCRT
  ]

{-# INLINABLE bench_unzip #-}
bench_unzip :: (UnzipCtx t m r) => t m (r,r) -> Bench '(t,m,r)
bench_unzip = bench unzipT

{-# INLINABLE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => t m r -> t m r -> Bench '(t,m,r)
bench_mul a = bench (zipWithT (*) a)

{-# INLINABLE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_crt = bench (fromJust' "TensorBenches.bench_crt" crt)

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_crtInv = bench (fromJust' "TensorBenches.bench_crtInv" crtInv)

{-# INLINABLE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_l = bench l

{-# INLINABLE bench_lInv #-}
-- convert input from Dec basis to Pow basis
bench_lInv :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_lInv = bench lInv

{-# INLINABLE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => t m r -> Bench '(t,m,r)
bench_liftPow = bench (fmapT lift)

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgPow = bench mulGPow

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgDec = bench mulGDec

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_mulgCRT = bench (fromJust' "TensorBenches.bench_mulgCRT" mulGCRT)

{-# INLINABLE bench_divgPow #-}
-- divide by g when input is in Pow basis
bench_divgPow :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgPow x =
  let y = mulGPow x
  in bench divGPow y

{-# INLINABLE bench_divgDec #-}
-- divide by g when input is in Dec basis
bench_divgDec :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgDec x =
  let y = mulGDec x
  in bench divGDec y

{-# INLINABLE bench_divgCRT #-}
-- divide by g when input is in CRT basis
bench_divgCRT :: (BasicCtx t m r) => t m r -> Bench '(t,m,r)
bench_divgCRT = bench (fromJust' "TensorBenches.bench_divgCRT" divGCRT)

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand
    (fmapT (roundMult one) <$>
      (tGaussianDec v :: Rand (CryptoRand gen) (t m Double)) :: Rand (CryptoRand gen) (t m (LiftOf r))) gen

{-# INLINABLE bench_twacePow #-}
bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m' r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePowDec :: t m' r -> t m r)

{-# INLINABLE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m' r -> Bench '(t,m,m',r)
bench_twaceCRT = bench (fromJust' "TensorBenches.bench_twaceCRT" twaceCRT :: t m' r -> t m r)

{-# INLINABLE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: t m r -> t m' r)

{-# INLINABLE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedDec = bench (embedDec :: t m r -> t m' r)

{-# INLINABLE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => t m r -> Bench '(t,m,m',r)
bench_embedCRT = bench (fromJust' "TensorBenches.bench_embedCRT" embedCRT :: t m r -> t m' r)
