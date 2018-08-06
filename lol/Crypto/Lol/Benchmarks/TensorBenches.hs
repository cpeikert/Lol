{-|
Module      : Crypto.Lol.Benchmarks.TensorBenches
Description : Benchmarks for the 'Tensor' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'Tensor' interface. In a perfect world, these benchmarks would
have the same performance as the 'Cyc' benchmarks. In practice, GHC gets in the
way at higher levels of the library, resulting in worse performance for 'Cyc'
in some cases.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.TensorBenches (tensorBenches1, tensorBenches2) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol.Utils.Benchmarks (bgroup, mkBench, mkBenchIO, Benchmark)
import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Lol.Types.IFunctor
import Crypto.Random

-- | Benchmarks for single-index 'Tensor' operations.
-- There must be a CRT basis for \(O_m\) over @r@.
-- These cover the same functions as @cycBenches1@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINABLE tensorBenches1 #-}
tensorBenches1 :: forall (t :: Factored -> * -> *) (m :: Factored) (r :: *) gen . (Fact m, _)
               => Proxy '(t,m,r) -> Proxy gen -> Benchmark
tensorBenches1 ptmr pgen =
    let z = zero :: t m r
        errorBench = mkBenchIO "error" (bench_errRounded ptmr pgen 0.1)
        benches = ($ z) <$> [
          mkBench "zipWith (*)" (bench_mul z),
          mkBench "crt" bench_crt,
          mkBench "crtInv" bench_crtInv,
          mkBench "decToPow" bench_decToPow,
          mkBench "powToDec" bench_powToDec,
          mkBench "*g Pow" bench_mulGPow,
          mkBench "*g Dec" bench_mulGDec,
          mkBench "*g CRT" bench_mulGCRT,
          mkBench "divG Pow" bench_divGPow,
          mkBench "divG Dec" bench_divGDec,
          mkBench "divG CRT" bench_divGCRT,
          mkBench "lift" bench_lift] in
    bgroup "Tensor" (benches ++ [errorBench])

-- | Benchmarks for inter-ring 'Tensor' operations.
-- There must be a CRT basis for \(O_{m'}\) over @r@.
-- These cover the same functions as @cycBenches1@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINABLE tensorBenches2 #-}
tensorBenches2 :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (r :: *) . _
  => Proxy '(t,m,m',r) -> Benchmark
tensorBenches2 ptmmr =
  let z = zero :: t m r
      z' = zero :: t m' r
      benches = [
        mkBench "twacePow" (bench_twacePow ptmmr) z',
        mkBench "twaceDec" (bench_twacePow ptmmr) z', -- yes, twacePow is correct here. It's the same function!
        mkBench "twaceCRT" (bench_twaceCRT ptmmr) z',
        mkBench "embedPow" (bench_embedPow ptmmr) z,
        mkBench "embedCRT" (bench_embedCRT ptmmr) z] in
  bgroup "Tensor" benches

{-# INLINABLE bench_mul #-}
-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: _ => t m r -> t m r -> t m r
bench_mul = zipWithI (*)

{-# INLINABLE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => t m r -> t m r
bench_crt = fromJust' "TensorBenches.bench_crt" crt

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
bench_crtInv :: _ => t m r -> t m r
bench_crtInv = fromJust' "TensorBenches.bench_crtInv" crtInv

{-# INLINABLE bench_decToPow #-}
-- convert input from Dec basis to Pow basis
bench_decToPow :: _ => t m r -> t m r
bench_decToPow = decToPow

{-# INLINABLE bench_powToDec #-}
-- convert input from Dec basis to Pow basis
bench_powToDec :: _ => t m r -> t m r
bench_powToDec = powToDec

{-# INLINABLE bench_lift #-}
bench_lift :: _ => t m r -> t m r'
bench_lift = fmapI lift

{-# INLINABLE bench_mulGPow #-}
-- multiply by g when input is in Pow basis
bench_mulGPow :: _ => t m r -> t m r
bench_mulGPow = mulGPow

{-# INLINABLE bench_mulGDec #-}
-- multiply by g when input is in Dec basis
bench_mulGDec :: _ => t m r -> t m r
bench_mulGDec = mulGDec

{-# INLINABLE bench_mulGCRT #-}
-- multiply by g when input is in CRT basis
bench_mulGCRT :: _ => t m r -> t m r
bench_mulGCRT = fromJust' "TensorBenches.bench_mulGCRT" mulGCRT

{-# INLINABLE bench_divGPow #-}
-- divide by g when input is in Pow basis
bench_divGPow :: _ => t m r -> Maybe (t m r)
bench_divGPow = divGPow . mulGPow

{-# INLINABLE bench_divGDec #-}
-- divide by g when input is in Dec basis
bench_divGDec :: _ => t m r -> Maybe (t m r)
bench_divGDec = divGDec . mulGDec

{-# INLINABLE bench_divGCRT #-}
-- divide by g when input is in CRT basis
bench_divGCRT :: _ => t m r -> t m r
bench_divGCRT = fromJust' "TensorBenches.bench_divGCRT" divGCRT

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (Fact m, CryptoRandomGen gen, TensorPowDec t r, _)
  => Proxy '(t,m,r) -> Proxy gen -> Double -> IO (t m (LiftOf r))
bench_errRounded _ _ v = do
  gen <- newGenIO
  return $ evalRand
    (fmapI (roundMult one) <$>
      (tweakedGaussianDec v :: Rand (CryptoRand gen) (t m Double)) :: Rand (CryptoRand gen)
                                                                           (t m (LiftOf r))) gen

-- EAC: due to GHC bug #12634, I have to give these a little more help than the corresponding functions
-- in UCyc and Cyc benches. Not a huge deal.
{-# INLINABLE bench_twacePow #-}
bench_twacePow :: forall t (m :: Factored) (m' :: Factored) r . (TensorPowDec t r, Fact m, _)
  => Proxy '(t,m,m',r) -> t m' r -> t m r
bench_twacePow _ = twacePowDec

{-# INLINABLE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (TensorPowDec t r, Fact m, _)
  => Proxy '(t,m,m',r) -> t m' r -> t m r
bench_twaceCRT _ = fromJust' "TensorBenches.bench_twaceCRT" twaceCRT

{-# INLINABLE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (TensorPowDec t r, Fact m', _)
  => Proxy '(t,m,m',r) -> t m r -> t m' r
bench_embedPow _ = embedPow

{-# INLINABLE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (TensorPowDec t r, Fact m', _)
  => Proxy '(t,m,m',r) -> t m r -> t m' r
bench_embedCRT _ = fromJust' "TensorBenches.bench_embedCRT" embedCRT
