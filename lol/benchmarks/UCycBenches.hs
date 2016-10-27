{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module UCycBenches (ucycBenches1, ucycBenches2) where

import Benchmarks

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random.DRBG

{-# INLINABLE ucycBenches1 #-}
ucycBenches1 :: (Monad rnd, _) => _ -> _ -> rnd Benchmark
ucycBenches1 ptmr pgen = benchGroup "UCyc" $ ($ ptmr) <$> [
  hideArgs "unzipPow" bench_unzipUCycPow,
  hideArgs "unzipDec" bench_unzipUCycDec,
  hideArgs "unzipCRT" bench_unzipUCycCRT,
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
  hideArgs "error" (bench_errRounded 0.1) .  addGen pgen
  ]

{-# INLINE ucycBenches2 #-}
ucycBenches2 :: (Monad rnd, _) => _ -> rnd Benchmark
ucycBenches2 p = benchGroup "UCyc" $ ($ p) <$> [
  hideArgs "twacePow" bench_twacePow,
  hideArgs "twaceDec" bench_twaceDec,
  hideArgs "twaceCRT" bench_twaceCRT,
  hideArgs "embedPow" bench_embedPow,
  hideArgs "embedDec" bench_embedDec,
  hideArgs "embedCRT" bench_embedCRT
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
