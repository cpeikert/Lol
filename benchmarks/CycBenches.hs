{-# LANGUAGE ConstraintKinds, NoImplicitPrelude, RebindableSyntax, FlexibleContexts, RankNTypes,
             DataKinds #-}


module CycBenches (cycBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol

import Criterion
import Utils

cycBenches :: (MonadRandom rnd) => rnd Benchmark
cycBenches = bgroupRnd "Cyc"
  [bgroupRnd "CRT + *" $ groupC $ wrap2Arg bench_mulPow,
   bgroupRnd "*" $ groupC $ wrap2Arg bench_mul]

bench_mulPow :: (CElt t r, Fact m) => Cyc t m r -> Cyc t m r -> Benchmarkable
bench_mulPow a b = 
  let a' = advisePow a
      b' = advisePow b
  in nf (a' *) b'

bench_mul :: (CElt t r, Fact m) => Cyc t m r -> Cyc t m r -> Benchmarkable
bench_mul a b = nf (a *) b

type BasicCtx t m r = (CElt t r, Fact m)

wrap2Arg :: (BasicCtx t m r, MonadRandom rnd) 
  => (Cyc t m r -> Cyc t m r -> Benchmarkable) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrap2Arg f _ _ str = (bench str) <$> (genArgs f)

groupC :: (MonadRandom rnd) =>
  (forall t m m' r . 
       (BasicCtx t m r) 
       => Proxy t 
          -> Proxy '(m,r)
          -> String
          -> rnd Benchmark)
  -> [rnd Benchmark]
groupC f =
  [bgroupRnd "Cyc CT" $ groupMR (f (Proxy::Proxy CT)),
   bgroupRnd "Cyc RT" $ groupMR (f (Proxy::Proxy RT))]

groupMR :: (MonadRandom rnd) =>
  (forall m r . (CElt CT r, CElt RT r, Fact m) => Proxy '(m, r) -> String -> rnd Benchmark) 
  -> [rnd Benchmark]
groupMR f = 
  [f (Proxy::Proxy '(F128, ZqBasic 257 Int64)) "F128/Q257", 
   f (Proxy::Proxy '(PToF Prime281, ZqBasic 563 Int64)) "F281/Q563"]
