{-|
Module      : Crypto.Lol.Benchmarks.CycRepBenches
Description : Benchmarks for 'CycRep'.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for 'CycRep'. In a perfect world, these benchmarks would
have the same performance as the 'Cyc' benchmarks. In practice, GHC
gets in the way at higher levels of the library, resulting in worse
performance for 'Cyc' in some cases.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.CycRepBenches (cycRepBenches1, cycRepBenches2) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol.Utils.Benchmarks
import Crypto.Lol.Cyclotomic.CycRep
import Crypto.Lol.Cyclotomic.Language (mulG)
import Crypto.Lol.Cyclotomic.Tensor (Tensor)
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random

-- | Benchmarks for single-index 'CycRep' operations.
-- There must be a CRT basis for \(O_m\) over @r@.
-- These cover the same functions as @cycBenches1@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINABLE cycRepBenches1 #-}
cycRepBenches1 :: (Monad rnd, _) => Proxy '(t,m,r) -> Proxy gen -> rnd Benchmark
cycRepBenches1 ptmr pgen = benchGroup "CycRep" $ ($ ptmr) <$> [
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

-- | Benchmarks for inter-ring 'CycRep' operations.
-- There must be a CRT basis for \(O_{m'}\) over @r@.
-- These cover the same functions as @cycBenches2@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINE cycRepBenches2 #-}
cycRepBenches2 :: (Monad rnd, _) => Proxy '(t,m,m',r) -> rnd Benchmark
cycRepBenches2 p = benchGroup "CycRep" $ ($ p) <$> [
  genBenchArgs "twacePow" bench_twacePow,
  genBenchArgs "twaceDec" bench_twaceDec,
  genBenchArgs "twaceCRT" bench_twaceCRT,
  genBenchArgs "embedPow" bench_embedPow,
  genBenchArgs "embedDec" bench_embedDec,
  genBenchArgs "embedCRT" bench_embedCRT
  ]

pcToEC :: CycRepPC t m r -> CycRepEC t m r
pcToEC (Right x) = (Right x)

{-# INLINE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: _ => CycRepPC t m r -> CycRepPC t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = pcToEC a
      b' = pcToEC b
  in bench (a' *) b'

{-# INLINE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => CycRep t P m r -> Bench '(t,m,r)
bench_crt = bench toCRT

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
-- EAC: This is slow without explicitly mentioning CRTElt. Possibly related to constraint synonym issues?
bench_crtInv :: (CRTElt t r, _) => CycRepPC t m r -> Bench '(t,m,r)
bench_crtInv (Right a) = bench toPow a

{-# INLINE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: _ => CycRep t D m r -> Bench '(t,m,r)
bench_l = bench toPow

{-# INLINE bench_lInv #-}
-- convert input from Pow basis to Dec basis
bench_lInv :: _ => CycRep t P m r -> Bench '(t,m,r)
bench_lInv = bench toDec

{-# INLINABLE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: _ => CycRep t P m r -> Bench '(t,m,r)
bench_liftPow = bench lift

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: _ => CycRep t P m r -> Bench '(t,m,r)
bench_mulgPow = bench mulG

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: _ => CycRep t D m r -> Bench '(t,m,r)
bench_mulgDec = bench mulG

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: _ => CycRepPC t m r -> Bench '(t,m,r)
bench_mulgCRT (Right a) = bench mulG a

{-# INLINABLE bench_divgPow #-}
-- divide by g when input is in Pow basis
bench_divgPow :: _ => CycRep t P m r -> Bench '(t,m,r)
bench_divgPow = bench divGPow . mulG

{-# INLINABLE bench_divgDec #-}
-- divide by g when input is in Dec basis
bench_divgDec :: _ => CycRep t D m r -> Bench '(t,m,r)
bench_divgDec = bench divGDec . mulG

{-# INLINABLE bench_divgCRT #-}
-- divide by g when input is in CRT basis
bench_divgCRT :: _ => CycRepPC t m r -> Bench '(t,m,r)
bench_divgCRT = either (const $ error "bench_divgCRT expected a CRTC") (bench divGCRTC)

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (Tensor t r, Fact m, CryptoRandomGen gen, _)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (roundedGaussian v :: Rand (CryptoRand gen) (CycRep t D m (LiftOf r))) gen

{-# INLINE bench_twacePow #-}
bench_twacePow :: forall t m m' r . (Fact m, _)
  => CycRep t P m' r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePow :: CycRep t P m' r -> CycRep t P m r)

{-# INLINE bench_twaceDec #-}
bench_twaceDec :: forall t m m' r . (Fact m, _)
  => CycRep t D m' r -> Bench '(t,m,m',r)
bench_twaceDec = bench (twaceDec :: CycRep t D m' r -> CycRep t D m r)

{-# INLINE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (Fact m, _)
  => CycRepPC t m' r -> Bench '(t,m,m',r)
bench_twaceCRT (Right a) = bench (twaceCRTC :: CycRep t C m' r -> CycRepPC t m r) a

{-# INLINE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (Fact m', _)
  => CycRep t P m r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: CycRep t P m r -> CycRep t P m' r)

{-# INLINE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (Fact m', _)
  => CycRep t D m r -> Bench '(t,m,m',r)
bench_embedDec = bench (embedDec :: CycRep t D m r -> CycRep t D m' r)

{-# INLINE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (Fact m', _)
  => CycRepPC t m r -> Bench '(t,m,m',r)
bench_embedCRT (Right a) = bench (embedCRTC :: CycRep t C m r -> CycRepPC t m' r) a
