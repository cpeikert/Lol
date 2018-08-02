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
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.CycRepBenches (cycRepBenches1, cycRepBenches2) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol.Utils.Benchmarks (bgroup, mkBench, mkBenchIO, Benchmark)
import Crypto.Lol.Cyclotomic.CycRep
import Crypto.Lol.Cyclotomic.Language (mulG)
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random

-- | Benchmarks for single-index 'CycRep' operations.
-- There must be a CRT basis for \(O_m\) over @r@.
-- These cover the same functions as @cycBenches1@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINABLE cycRepBenches1 #-}
cycRepBenches1 :: forall (t :: Factored -> * -> *) (m :: Factored) (r :: *) gen . _
            => Proxy '(t,m,r) -> Proxy gen -> Benchmark
cycRepBenches1 ptmr pgen =
  let zDec = zero :: CycRep t D m r
      zPow = zero :: CycRep t P m r
      zEC = zero :: CycRepEC t m r
      zPC = ecToPC zEC
      errorBench = mkBenchIO "error" (bench_errRounded ptmr pgen 0.1)
      benches = [
        mkBench "zipWith (*)" (bench_mul zPC) zPC,
        mkBench "crt"         bench_crt       zPow,
        mkBench "crtInv"      bench_crtInv    zPC,
        mkBench "l"           bench_l         zDec,
        mkBench "lInv"        bench_lInv      zPow,
        mkBench "*g Pow"      bench_mulgPow   zPow,
        mkBench "*g Dec"      bench_mulgDec   zDec,
        mkBench "*g CRT"      bench_mulgCRT   zPC,
        mkBench "divG Pow"    bench_divGPow   zPow,
        mkBench "divG Dec"    bench_divGDec   zDec,
        mkBench "divG CRT"    bench_divGCRT   zPC,
        mkBench "lift"        bench_liftPow   zPow] in
      -- This is different because it lives in IO
  bgroup "CycRep" (benches ++ [errorBench])

-- | Benchmarks for inter-ring 'CycRep' operations.
-- There must be a CRT basis for \(O_{m'}\) over @r@.
-- These cover the same functions as @cycBenches2@, but may have different
-- performance due to how GHC interacts with Lol.
{-# INLINE cycRepBenches2 #-}
cycRepBenches2 :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (r :: *) .
                  (m `Divides` m', _)
               => Proxy '(t,m,m',r) -> Benchmark
cycRepBenches2 ptmmr =
  let zPow' = zero :: CycRep t P m' r
      zDec' = zero :: CycRep t D m' r
      zEC' = zero :: CycRepEC t m' r
      zPC' = ecToPC zEC'
      zPow = zero :: CycRep t P m r
      zDec = zero :: CycRep t D m r
      zEC = zero :: CycRepEC t m r
      zPC = ecToPC zEC
      benches = [
        mkBench "twacePow" (bench_twacePow ptmmr) zPow',
        mkBench "twaceDec" (bench_twaceDec ptmmr) zDec',
        mkBench "twaceCRT" (bench_twaceCRT ptmmr) zPC',
        mkBench "embedPow" (bench_embedPow ptmmr) zPow,
--        mkBench "embedDec" (bench_embedDec ptmmr) zDec,
        mkBench "embedCRT" (bench_embedCRT ptmmr) zPC] in
  bgroup "CycRep" benches

pcToEC :: CycRepPC t m r -> CycRepEC t m r
pcToEC (Right x) = (Right x)

ecToPC :: CycRepEC t m r -> CycRepPC t m r
ecToPC (Right x) = (Right x)

{-# INLINE bench_mul #-}
bench_mul :: _ => CycRepPC t m r -> CycRepPC t m r -> CycRepEC t m r
bench_mul a b = (pcToEC a) * (pcToEC b)

{-# INLINE bench_crt #-}
-- convert input from Pow basis to CRT basis
bench_crt :: _ => CycRep t P m r -> CycRepEC t m r
bench_crt = toCRT

{-# INLINABLE bench_crtInv #-}
-- convert input from CRT basis to Pow basis
-- EAC: This is slow without explicitly mentioning CRTElt. Possibly related to constraint synonym issues?
bench_crtInv :: (CRTElt t r, _) => CycRepPC t m r -> CycRep t P m r
bench_crtInv (Right a) = toPow a

{-# INLINE bench_l #-}
-- convert input from Dec basis to Pow basis
bench_l :: _ => CycRep t D m r -> CycRep t P m r
bench_l = toPow

{-# INLINE bench_lInv #-}
-- convert input from Pow basis to Dec basis
bench_lInv :: _ => CycRep t P m r -> CycRep t D m r
bench_lInv = toDec

{-# INLINABLE bench_liftPow #-}
-- lift an element in the Pow basis
bench_liftPow :: _ => CycRep t P m r -> CycRep t P m r'
bench_liftPow = lift

{-# INLINABLE bench_mulgPow #-}
-- multiply by g when input is in Pow basis
bench_mulgPow :: _ => CycRep t P m r -> CycRep t P m r
bench_mulgPow = mulGPow

{-# INLINABLE bench_mulgDec #-}
-- multiply by g when input is in Dec basis
bench_mulgDec :: _ => CycRep t D m r -> CycRep t D m r
bench_mulgDec = mulGDec

{-# INLINABLE bench_mulgCRT #-}
-- multiply by g when input is in CRT basis
bench_mulgCRT :: _ => CycRepPC t m r -> CycRep t C m r
bench_mulgCRT (Right a) = mulGCRTC a

{-# INLINABLE bench_divGPow #-}
-- divide by g when input is in Pow basis
bench_divGPow :: _ => CycRep t P m r -> Maybe (CycRep t P m r)
bench_divGPow = divGPow . mulGPow

{-# INLINABLE bench_divGDec #-}
-- divide by g when input is in Dec basis
bench_divGDec :: _ => CycRep t D m r -> Maybe (CycRep t D m r)
bench_divGDec = divGDec . mulGDec

{-# INLINABLE bench_divGCRT #-}
-- divide by g when input is in CRT basis
bench_divGCRT :: _ => CycRepPC t m r -> CycRep t C m r
bench_divGCRT = either (error "bench_divGCRT expected a CRTC") divGCRTC

{-# INLINABLE bench_errRounded #-}
-- generate a rounded error term
bench_errRounded :: forall t m r gen . (Fact m, CryptoRandomGen gen, _)
                 => Proxy '(t,m,r) -> Proxy gen -> Double -> IO (CycRep t D m (LiftOf r))
bench_errRounded _ _ v = do
  gen <- newGenIO
  let e = roundedGaussian v :: Rand (CryptoRand gen) (CycRep t D m (LiftOf r))
  return $ evalRand e gen

{-# INLINE bench_twacePow #-}
bench_twacePow :: forall t m m' r . (Fact m, _)
  => Proxy '(t,m,m',r) -> CycRep t P m' r -> CycRep t P m r
bench_twacePow _ = twacePow

{-# INLINE bench_twaceDec #-}
bench_twaceDec :: forall t m m' r . (Fact m, _)
  => Proxy '(t,m,m',r) -> CycRep t D m' r -> CycRep t D m r
bench_twaceDec _ = twaceDec

{-# INLINE bench_twaceCRT #-}
bench_twaceCRT :: forall t m m' r . (Fact m, _)
  => Proxy '(t,m,m',r) -> CycRepPC t m' r -> CycRepPC t m r
bench_twaceCRT _ (Right a) = twaceCRTC a

{-# INLINE bench_embedPow #-}
bench_embedPow :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> CycRep t P m r -> CycRep t P m' r
bench_embedPow _ = embedPow

{-
{-# INLINE bench_embedDec #-}
bench_embedDec :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> CycRep t D m r -> CycRep t D m' r
bench_embedDec _ = embedDec
-}

{-# INLINE bench_embedCRT #-}
bench_embedCRT :: forall t m m' r . (Fact m', _)
  => Proxy '(t,m,m',r) -> CycRepPC t m r -> CycRepPC t m' r
bench_embedCRT _ (Right a) = embedCRTC a
