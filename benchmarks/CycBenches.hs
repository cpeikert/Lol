{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, NoImplicitPrelude, 
             RankNTypes, RebindableSyntax, ScopedTypeVariables, TypeOperators #-}


module CycBenches (cycBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol

import Criterion
import Utils

cycBenches :: (MonadRandom rnd) => rnd Benchmark
cycBenches = bgroupRnd "Cyc"
  [bgroupRnd "CRT + *" $ groupC $ wrap2Arg bench_mulPow,
   bgroupRnd "*" $ groupC $ wrap2Arg bench_mul,
   bgroupRnd "crt" $ groupC $ wrap1Arg bench_crt,
   bgroupRnd "crtInv" $ groupC $ wrap1Arg bench_crtInv,
   bgroupRnd "l" $ groupC $ wrap1Arg bench_l,
   bgroupRnd "*g Pow" $ groupC $ wrap1Arg bench_mulgPow,
   bgroupRnd "*g CRT" $ groupC $ wrap1Arg bench_mulgCRT,
   bgroupRnd "lift" $ groupCLift $ wrapLift bench_liftPow,
   bgroupRnd "error" $ groupCLift $ wrapError $ bench_errRounded 0.1]
   -- sanity checks
   --bgroupRnd "^2" $ groupC $ wrap1Arg bench_sq,             -- should take same as bench_mul
   --bgroupRnd "id2" $ groupC $ wrap1Arg bench_advisePowPow,] -- should take a few nanoseconds: this is a no-op

-- convert both arguments to CRT basis, then multiply them coefficient-wise
bench_mulPow :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> Benchmarkable
bench_mulPow a b = 
  let a' = advisePow a
      b' = advisePow b
  in nf (a' *) b'

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> Benchmarkable
bench_mul a b = 
  let a' = adviseCRT a
      b' = adviseCRT b
  in nf (a *) b

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => Cyc t m r -> Benchmarkable
bench_crt x = let y = advisePow x in nf adviseCRT y

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => Cyc t m r -> Benchmarkable
bench_crtInv x = let y = adviseCRT x in nf advisePow y

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => Cyc t m r -> Benchmarkable
bench_l x = let y = adviseDec x in nf advisePow y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> Benchmarkable
bench_liftPow x = let y = advisePow x in nf (liftCyc Pow :: Cyc t m r -> Cyc t m (LiftOf r)) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> Benchmarkable
bench_mulgPow x = let y = advisePow x in nf mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> Benchmarkable
bench_mulgCRT x = let y = adviseCRT x in nf mulG y

bench_errRounded :: forall t m r . (LiftCtx t m r) => Double -> Proxy (t m r) -> Benchmarkable
bench_errRounded v _ = nfIO (errorRounded v :: IO (Cyc t m (LiftOf r)))

{-
-- sanity check: this test should take the same amount of time as bench_mul
-- if it takes less, then random element generation is being counted!
bench_sq :: (CElt t r, Fact m) => Cyc t m r -> Benchmarkable
bench_sq a = nf (a *) a

-- sanity check: this should be a no-op
bench_advisePowPow :: (CElt t r, Fact m) => Cyc t m r -> Benchmarkable
bench_advisePowPow x = let y = advisePow x in nf advisePow y
-}

type BasicCtx t m r = (CElt t r, Fact m)

wrap1Arg :: (BasicCtx t m r, MonadRandom rnd) 
  => (Cyc t m r -> Benchmarkable) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrap1Arg f _ _ str = bench str <$> genArgs f

--wrap2 :: (BasicCtx t m r, MonadRandom rnd) 
--  => (Cyc t m r -> Cyc t m r -> Benchmarkable) -> [rnd Benchmark]
--wrap2 f = groupC $ wrap2Arg f

wrap2Arg :: (BasicCtx t m r, MonadRandom rnd) 
  => (Cyc t m r -> Cyc t m r -> Benchmarkable) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrap2Arg f _ _ str = bench str <$> genArgs f

groupC :: (MonadRandom rnd) =>
  (forall t m r . 
       (BasicCtx t m r) 
       => Proxy t 
          -> Proxy '(m,r)
          -> String
          -> rnd Benchmark)
  -> [rnd Benchmark]
groupC f =
  [--bgroupRnd "Cyc CT" $ groupMR (f (Proxy::Proxy CT))]
   bgroupRnd "Cyc RT" $ groupMR (f (Proxy::Proxy RT))]

groupMR :: (MonadRandom rnd) =>
  (forall m r . (BasicCtx CT m r, BasicCtx RT m r) => Proxy '(m, r) -> String -> rnd Benchmark) 
  -> [rnd Benchmark]
groupMR f = 
  [f (Proxy::Proxy '(F128, Zq 257)) "F128/Q257",
   f (Proxy::Proxy '(PToF Prime281, Zq 563)) "F281/Q563",
   f (Proxy::Proxy '(F32 * F9, Zq 512)) "F288/Q512",
   f (Proxy::Proxy '(F32 * F9, Zq 577)) "F288/Q577",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153))) "F288/Q577*1153",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017))) "F288/Q577*1153*2017",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593))) "F288/Q577*1153*2017*2593",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169))) "F288/Q577*1153*2017*2593*3619",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457))) "F288/Q577*1153*2017*2593*3619*3457",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337))) "F288/Q577*1153*2017*2593*3619*3457*6337",
   f (Proxy::Proxy '(F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489))) "F288/Q577*1153*2017*2593*3619*3457*6337*7489",
   f (Proxy::Proxy '(F32 * F9 * F25, Zq 14401)) "F7200/Q14401"]




type LiftCtx t m r = (CElt t r, CElt t (LiftOf r), Lift' r, Fact m, ToInteger (LiftOf r))

wrapLift :: (LiftCtx t m r, MonadRandom rnd)
  => (Cyc t m r -> Benchmarkable) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrapLift f _ _ str = bench str <$> genArgs f

wrapError :: (LiftCtx t m r, Monad rnd)
  => (Proxy (t m r) -> Benchmarkable) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrapError f _ _ str = return $ bench str $ f Proxy

groupCLift :: (MonadRandom rnd) =>
  (forall t m r . 
       (LiftCtx t m r) 
       => Proxy t 
          -> Proxy '(m,r)
          -> String
          -> rnd Benchmark)
  -> [rnd Benchmark]
groupCLift f =
  [--bgroupRnd "Cyc CT" $ groupMRLift (f (Proxy::Proxy CT))]
   bgroupRnd "Cyc RT" $ groupMRLift (f (Proxy::Proxy RT))]

groupMRLift :: (MonadRandom rnd) =>
  (forall m r . (LiftCtx CT m r, LiftCtx RT m r) => Proxy '(m, r) -> String -> rnd Benchmark) 
  -> [rnd Benchmark]
groupMRLift f = 
  [f (Proxy::Proxy '(F128, Zq 257)) "F128/Q257",
   f (Proxy::Proxy '(PToF Prime281, Zq 563)) "F281/Q563",
   f (Proxy::Proxy '(F32 * F9, Zq 512)) "F288/Q512",
   f (Proxy::Proxy '(F32 * F9, Zq 577)) "F288/Q577",
   f (Proxy::Proxy '(F32 * F9 * F25, Zq 14401)) "F7200/Q14401"]