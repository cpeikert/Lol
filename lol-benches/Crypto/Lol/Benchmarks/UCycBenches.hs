{-|
Module      : Crypto.Lol.Benchmarks.UCycBenches
Description : Benchmarks for the 'UCyc' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'UCyc' interface.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.UCycBenches (ucycBenches1, ucycBenches2) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random

-- | Benchmarks for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
{-# INLINABLE ucycBenches1 #-}
ucycBenches1 :: (Monad rnd, _) => Proxy '(t,m,r) -> Proxy gen -> rnd Benchmark
ucycBenches1 ptmr pgen = benchGroup "UCyc" $ ($ ptmr) <$> [
  genBenchArgs "unzipPow" bench_unzipUCycPow,
  genBenchArgs "unzipDec" bench_unzipUCycDec,
  genBenchArgs "unzipCRT" bench_unzipUCycCRT,
  genBenchArgs "zipWith (*)" bench_mul,
  genBenchArgs "crt" bench_crt,
  genBenchArgs "crtInv" bench_crtInv,
  genBenchArgs "l" bench_l,
  genBenchArgs "lInv" bench_lInv,
  genBenchArgs "*g Pow" bench_mulgPow,
  genBenchArgs "*g Dec" bench_mulgDec,
  genBenchArgs "*g CRT" bench_mulgCRT,
  genBenchArgs "divg Pow" bench_divgPow,
  genBenchArgs "divg Dec" bench_divgDec,
  genBenchArgs "divg CRT" bench_divgCRT,
  genBenchArgs "lift" bench_liftPow,
  genBenchArgs "error" (bench_errRounded 0.1) .  addGen pgen
  ]

-- | Benchmarks for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
{-# INLINE ucycBenches2 #-}
ucycBenches2 :: (Monad rnd, _) => Proxy '(t,m,m',r) -> rnd Benchmark
ucycBenches2 p = benchGroup "UCyc" $ ($ p) <$> [
  genBenchArgs "twacePow" bench_twacePow,
  genBenchArgs "twaceDec" bench_twaceDec,
  genBenchArgs "twaceCRT" bench_twaceCRT,
  genBenchArgs "embedPow" bench_embedPow,
  genBenchArgs "embedDec" bench_embedDec,
  genBenchArgs "embedCRT" bench_embedCRT
  ]

{-# INLINE bench_unzipUCycPow #-}
bench_unzipUCycPow :: _ => UCyc t m P (r,r) -> Bench '(t,m,r)
bench_unzipUCycPow = bench unzipPow

{-# INLINE bench_unzipUCycDec #-}
bench_unzipUCycDec :: _ => UCyc t m D (r,r) -> Bench '(t,m,r)
bench_unzipUCycDec = bench unzipDec

{-# INLINE bench_unzipUCycCRT #-}
bench_unzipUCycCRT :: _ => UCycPC t m (r,r) -> Bench '(t,m,r)
bench_unzipUCycCRT = either (const $ error "bench_unzipUCycCRT expected a CRTC") (bench unzipCRTC)

pcToEC :: UCycPC t m r -> UCycEC t m r
pcToEC (Right x) = (Right x)

{-# INLINE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: _ => UCycPC t m r -> UCycPC t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = pcToEC a
      b' = pcToEC b
  in bench (a' *) b'

{-# INLINE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => UCyc t m P r -> Bench '(t,m,r)
bench_crt = bench toCRT

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
-- EAC: This is slow without explicitly mentioning UCRTElt. Possibly related to constraint synonym issues?
bench_crtInv :: (UCRTElt t r, _) => UCycPC t m r -> Bench '(t,m,r)
bench_crtInv (Right a) = bench toPow a

{-# INLINE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: _ => UCyc t m D r -> Bench '(t,m,r)
bench_l = bench toPow

{-# INLINE bench_lInv #-}
-- convert input from Pow basis to Dec basis
bench_lInv :: _ => UCyc t m P r -> Bench '(t,m,r)
bench_lInv = bench toDec

{-# INLINABLE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: _ => UCyc t m P r -> Bench '(t,m,r)
bench_liftPow = bench lift

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: _ => UCyc t m P r -> Bench '(t,m,r)
bench_mulgPow = bench mulG

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: _ => UCyc t m D r -> Bench '(t,m,r)
bench_mulgDec = bench mulG

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: _ => UCycPC t m r -> Bench '(t,m,r)
bench_mulgCRT (Right a) = bench mulG a

{-# INLINABLE bench_divgPow #-}
-- divide by g when input is in Pow basis
bench_divgPow :: _ => UCyc t m P r -> Bench '(t,m,r)
bench_divgPow = bench divGPow . mulG

{-# INLINABLE bench_divgDec #-}
-- divide by g when input is in Dec basis
bench_divgDec :: _ => UCyc t m D r -> Bench '(t,m,r)
bench_divgDec = bench divGDec . mulG

{-# INLINABLE bench_divgCRT #-}
-- divide by g when input is in CRT basis
bench_divgCRT :: _ => UCycPC t m r -> Bench '(t,m,r)
bench_divgCRT = either (const $ error "bench_divgCRT expected a CRTC") (bench divGCRTC)

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (TElt t r, Fact m, CryptoRandomGen gen, _)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (UCyc t m D (LiftOf r))) gen

{-# INLINE bench_twacePow #-}
bench_twacePow :: forall t m m' r . (Fact m, _)
  => UCyc t m' P r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePow :: UCyc t m' P r -> UCyc t m P r)

{-# INLINE bench_twaceDec #-}
bench_twaceDec :: forall t m m' r . (Fact m, _)
  => UCyc t m' D r -> Bench '(t,m,m',r)
bench_twaceDec = bench (twaceDec :: UCyc t m' D r -> UCyc t m D r)

{-# INLINE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (Fact m, _)
  => UCycPC t m' r -> Bench '(t,m,m',r)
bench_twaceCRT (Right a) = bench (twaceCRTC :: UCyc t m' C r -> UCycPC t m r) a

{-# INLINE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (Fact m', _)
  => UCyc t m P r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: UCyc t m P r -> UCyc t m' P r)

{-# INLINE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (Fact m', _)
  => UCyc t m D r -> Bench '(t,m,m',r)
bench_embedDec = bench (embedDec :: UCyc t m D r -> UCyc t m' D r)

{-# INLINE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (Fact m', _)
  => UCycPC t m r -> Bench '(t,m,m',r)
bench_embedCRT (Right a) = bench (embedCRTC :: UCyc t m C r -> UCycPC t m' r) a
