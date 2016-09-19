{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CycBenches (cycBenches1, cycBenches2) where

import Apply.Cyc
import Benchmarks

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

-- cycBenches1 param :: IO Benchmark
{-# INLINABLE cycBenches1 #-}
cycBenches1 ptmr pgen = benchGroup "Cyc" $ ($ ptmr) <$> [
  hideArgs "unzipPow" bench_unzipCycPow,
  hideArgs "unzipDec" bench_unzipCycDec,
  hideArgs "unzipCRT" bench_unzipCycCRT,
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
{-# INLINABLE cycBenches2 #-}
cycBenches2 p = benchGroup "Cyc" $ ($ p) <$> [
  hideArgs "twacePow" bench_twacePow,
  hideArgs "twaceDec" bench_twaceDec,
  hideArgs "twaceCRT" bench_twaceCRT,
  hideArgs "embedPow" bench_embedPow,
  hideArgs "embedDec" bench_embedDec,
  hideArgs "embedCRT" bench_embedCRT
  ]

{-# INLINABLE bench_unzipCycPow #-}
bench_unzipCycPow :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycPow = bench unzipCyc . advisePow

{-# INLINE bench_unzipCycDec #-}
bench_unzipCycDec :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycDec = bench unzipCyc . adviseDec

{-# INLINE bench_unzipCycCRT #-}
bench_unzipCycCRT :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycCRT = bench unzipCyc . adviseCRT

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

-- convert input from Pow basis to Dec basis
bench_lInv :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_lInv x = let y = advisePow x in bench adviseDec y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> Bench '(t,m,r)
{-# INLINE bench_liftPow #-}
bench_liftPow x = let y = advisePow x in bench (liftCyc Pow) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgPow x = let y = advisePow x in bench mulG y

-- multiply by g when input is in Dec basis
bench_mulgDec :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgDec x = let y = adviseDec x in bench mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgCRT x = let y = adviseCRT x in bench mulG y

-- divide by g when input is in Pow basis
bench_divgPow :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgPow x = let y = advisePow $ mulG x in bench divG y

-- divide by g when input is in Dec basis
bench_divgDec :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgDec x = let y = adviseDec $ mulG x in bench divG y

-- divide by g when input is in CRT basis
bench_divgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgCRT x = let y = adviseCRT x in bench divG y

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

bench_twaceDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twaceDec x =
  let y = adviseDec x
  in bench (twace :: Cyc t m' r -> Cyc t m r) y

bench_twaceCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twaceCRT x =
  let y = adviseCRT x
  in bench (twace :: Cyc t m' r -> Cyc t m r) y

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedPow x =
  let y = advisePow x
  in bench (advisePow . embed :: Cyc t m r -> Cyc t m' r) y

bench_embedDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedDec x =
  let y = adviseDec x
  in bench (adviseDec . embed :: Cyc t m r -> Cyc t m' r) y

bench_embedCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedCRT x =
  let y = adviseCRT x
  in bench (adviseCRT . embed :: Cyc t m r -> Cyc t m' r) y
