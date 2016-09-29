{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.CycBenches (cycBenches1, cycBenches2) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Types
import Crypto.Random

{-# INLINABLE cycBenches1 #-}
cycBenches1 :: (Monad rnd, _) => _ -> _ -> rnd Benchmark
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
cycBenches2 :: (Monad rnd, _) => _ -> rnd Benchmark
cycBenches2 p = benchGroup "Cyc" $ ($ p) <$> [
  hideArgs "twacePow" bench_twacePow,
  hideArgs "twaceDec" bench_twaceDec,
  hideArgs "twaceCRT" bench_twaceCRT,
  hideArgs "embedPow" bench_embedPow,
  hideArgs "embedDec" bench_embedDec,
  hideArgs "embedCRT" bench_embedCRT
  ]

{-# INLINE bench_unzipCycPow #-}
bench_unzipCycPow :: _ => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycPow = bench unzipCyc . advisePow

{-# INLINE bench_unzipCycDec #-}
bench_unzipCycDec :: _ => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycDec = bench unzipCyc . adviseDec

{-# INLINE bench_unzipCycCRT #-}
bench_unzipCycCRT :: _ => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycCRT = bench unzipCyc . adviseCRT

{-# INLINABLE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: _ => Cyc t m r -> Cyc t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = adviseCRT a
      b' = adviseCRT b
  in bench (a' *) b'

{-# INLINABLE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => Cyc t m r -> Bench '(t,m,r)
bench_crt = bench adviseCRT . advisePow

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
bench_crtInv :: _ => Cyc t m r -> Bench '(t,m,r)
bench_crtInv = bench advisePow . adviseCRT

{-# INLINABLE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: _ => Cyc t m r -> Bench '(t,m,r)
bench_l = bench advisePow . adviseDec

{-# INLINABLE bench_lInv #-}
-- convert input from Pow basis to Dec basis
bench_lInv :: _ => Cyc t m r -> Bench '(t,m,r)
bench_lInv = bench adviseDec  . advisePow

{-# INLINE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: _ => Cyc t m r -> Bench '(t,m,r)
bench_liftPow = bench (liftCyc Pow) . advisePow

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: _ => Cyc t m r -> Bench '(t,m,r)
bench_mulgPow = bench mulG . advisePow

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: _ => Cyc t m r -> Bench '(t,m,r)
bench_mulgDec = bench mulG . adviseDec

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: _ => Cyc t m r -> Bench '(t,m,r)
bench_mulgCRT = bench mulG . adviseCRT

{-# INLINABLE bench_divgPow #-}
-- divide by g when input is in Pow basis
bench_divgPow :: _ => Cyc t m r -> Bench '(t,m,r)
bench_divgPow = bench divG . advisePow . mulG

{-# INLINABLE bench_divgDec #-}
-- divide by g when input is in Dec basis
bench_divgDec :: _ => Cyc t m r -> Bench '(t,m,r)
bench_divgDec = bench divG . adviseDec . mulG

{-# INLINABLE bench_divgCRT #-}
-- divide by g when input is in CRT basis
bench_divgCRT :: _ => Cyc t m r -> Bench '(t,m,r)
bench_divgCRT = bench divG . adviseCRT

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (TElt t r, Fact m, CryptoRandomGen gen, _)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))) gen

-- These need a hint on the kind of the output index. Could use a kind annotation on the forall'd var.
{-# INLINE bench_twacePow #-}
bench_twacePow :: forall t m m' r . (Fact m, _)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twacePow = bench (twace :: Cyc t m' r -> Cyc t m r) . advisePow

{-# INLINE bench_twaceDec #-}
bench_twaceDec :: forall t m m' r . (Fact m, _)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twaceDec = bench (twace :: Cyc t m' r -> Cyc t m r) . adviseDec

{-# INLINE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (Fact m, _)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twaceCRT = bench (twace :: Cyc t m' r -> Cyc t m r) . adviseCRT

{-# INLINE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (Fact m', _)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedPow = bench (advisePow . embed :: Cyc t m r -> Cyc t m' r) . advisePow

{-# INLINE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (Fact m', _)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedDec = bench (adviseDec . embed :: Cyc t m r -> Cyc t m' r) . adviseDec

{-# INLINE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (Fact m', _)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedCRT = bench (adviseCRT . embed :: Cyc t m r -> Cyc t m' r) . adviseCRT
