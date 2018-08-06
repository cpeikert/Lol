{-|
Module      : Crypto.Lol.Benchmarks.CycBenches
Description : Benchmarks for the 'Cyc' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'Cyc' interface.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.CycBenches (cycBenches1, cycBenches2) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Lol.Utils.Benchmarks (Benchmark, bgroup, mkBench, mkBenchIO)
import Crypto.Random

-- | Benchmarks for single-index 'Cyc' operations.
-- There must be a CRT basis for \(O_m\) over @r@.
{-# INLINABLE cycBenches1 #-}
cycBenches1 :: forall (t :: Factored -> * -> *) (m :: Factored) (r :: *) gen . _
            => Proxy '(t,m,r) -> Proxy gen -> Benchmark
cycBenches1 ptmr pgen =
  let z = zero :: Cyc t m r
      errorBench = mkBenchIO "error" (bench_errRounded ptmr pgen 0.1)
      benches = ($ z) <$> [
        mkBench "zipWith (*)" (bench_mul z),
        mkBench "crt" bench_crt,
        mkBench "crtInv" bench_crtInv,
        mkBench "l" bench_l,
        mkBench "lInv" bench_lInv,
        mkBench "*g Pow" bench_mulgPow,
        mkBench "*g Dec" bench_mulgDec,
        mkBench "*g CRT" bench_mulgCRT,
        mkBench "divG Pow" bench_divGPow,
        mkBench "divG Dec" bench_divGDec,
        mkBench "divG CRT" bench_divGCRT,
        mkBench "lift" bench_liftPow] in
  bgroup "Cyc" (benches ++ [errorBench])

-- | Benchmarks for inter-ring 'Cyc' operations.
-- There must be a CRT basis for \(O_{m'}\) over @r@.
{-# INLINABLE cycBenches2 #-}
cycBenches2 :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (r :: *) .
               (m `Divides` m', _)
            => Proxy '(t,m,m',r) -> Benchmark
cycBenches2 ptmmr =
  let z' = zero :: Cyc t m' r
      z = zero :: Cyc t m r
      benches = [
        mkBench "twacePow" (bench_twacePow ptmmr) z',
        mkBench "twaceDec" (bench_twaceDec ptmmr) z',
        mkBench "twaceCRT" (bench_twaceCRT ptmmr) z',
        mkBench "embedPow" (bench_embedPow ptmmr) z,
        mkBench "embedDec" (bench_embedDec ptmmr) z,
        mkBench "embedCRT" (bench_embedCRT ptmmr) z] in
  bgroup "Cyc" benches

{-# INLINABLE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: _ => Cyc t m r -> Cyc t m r -> Cyc t m r
bench_mul a b = (adviseCRT a) * (adviseCRT b)

{-# INLINABLE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => Cyc t m r -> Cyc t m r
bench_crt = adviseCRT . advisePow

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
bench_crtInv :: _ => Cyc t m r -> Cyc t m r
bench_crtInv = advisePow . adviseCRT

{-# INLINABLE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: _ => Cyc t m r -> Cyc t m r
bench_l = advisePow . adviseDec

{-# INLINABLE bench_lInv #-}
-- convert input from Pow basis to Dec basis
bench_lInv :: _ => Cyc t m r -> Cyc t m r
bench_lInv = adviseDec  . advisePow

{-# INLINE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: _ => Cyc t m r -> Cyc t m r'
bench_liftPow = liftPow . advisePow

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: _ => Cyc t m r -> Cyc t m r
bench_mulgPow = mulG . advisePow

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: _ => Cyc t m r -> Cyc t m r
bench_mulgDec = mulG . adviseDec

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: _ => Cyc t m r -> Cyc t m r
bench_mulgCRT = mulG . adviseCRT

{-# INLINABLE bench_divGPow #-}
-- divide by g when input is in Pow basis
bench_divGPow :: _ => Cyc t m r -> Maybe (Cyc t m r)
bench_divGPow = divG . advisePow . mulG

{-# INLINABLE bench_divGDec #-}
-- divide by g when input is in Dec basis
bench_divGDec :: _ => Cyc t m r -> Maybe (Cyc t m r)
bench_divGDec = divG . adviseDec . mulG

{-# INLINABLE bench_divGCRT #-}
-- divide by g when input is in CRT basis
bench_divGCRT :: _ => Cyc t m r -> Maybe (Cyc t m r)
bench_divGCRT = divG . adviseCRT

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall (t :: Factored -> * -> *) m (r :: *) gen . (Fact m, CryptoRandomGen gen, _)
                 => Proxy '(t,m,r) -> Proxy gen -> Double -> IO (Cyc t m (LiftOf r))
bench_errRounded _ _ v = do
  gen <- newGenIO
  let e = roundedGaussian v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))
  return $ evalRand e gen

{-# INLINE bench_twacePow #-}
bench_twacePow :: forall t m m' r . _
  => Proxy '(t,m,m',r) -> Cyc t m' r -> Cyc t m r
bench_twacePow _ = (twace :: Cyc t m' r -> Cyc t m r) . advisePow

{-# INLINE bench_twaceDec #-}
bench_twaceDec :: forall t m m' r . (Fact m, _)
  => Proxy '(t,m,m',r) -> Cyc t m' r -> Cyc t m r
bench_twaceDec _ = (twace :: Cyc t m' r -> Cyc t m r) . adviseDec

{-# INLINE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (Fact m, _)
  => Proxy '(t,m,m',r) -> Cyc t m' r -> Cyc t m r
bench_twaceCRT _ = (twace :: Cyc t m' r -> Cyc t m r) . adviseCRT

{-# INLINE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> Cyc t m r -> Cyc t m' r
bench_embedPow _ = (advisePow . embed :: Cyc t m r -> Cyc t m' r) . advisePow

{-# INLINE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> Cyc t m r -> Cyc t m' r
bench_embedDec _ = (adviseDec . embed :: Cyc t m r -> Cyc t m' r) . adviseDec

{-# INLINE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> Cyc t m r -> Cyc t m' r
bench_embedCRT _ = (adviseCRT . embed :: Cyc t m r -> Cyc t m' r) . adviseCRT
