{-# LANGUAGE ConstraintKinds, NoImplicitPrelude, RebindableSyntax, FlexibleContexts, RankNTypes,
             DataKinds #-}


module CycBenches (cycBenches) where

import Control.Monad.Random

import Crypto.Lol

import Criterion
import Utils

cycBenches :: (MonadRandom rnd) => rnd Benchmark
cycBenches = bgroupRnd "Cyc"
  [bgroupRnd "CRT + *" $ groupC $ wrap2Arg bench_mulPow,
   bgroupRnd "*" $ groupC $ wrap2Arg bench_mul]

bench_mulPow :: (CElt t r, Fact m) => Cyc t m r -> Cyc t m r -> String -> Benchmark
bench_mulPow a b str = 
  let a' = advisePow a
      b' = advisePow b
  in bench str $ nf (a' *) b'

bench_mul :: (CElt t r, Fact m) => Cyc t m r -> Cyc t m r -> String -> Benchmark
bench_mul a b str = bench str $ nf (a *) b

type BasicCtx t m r = (CElt t r, Fact m)

wrap2Arg :: (BasicCtx t m r, MonadRandom rnd) 
  => (Cyc t m r -> Cyc t m r -> String -> Benchmark) -> Proxy t -> Proxy '(m,r) -> String -> rnd Benchmark
wrap2Arg f _ _ str = genArgs str f

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
   f (Proxy::Proxy '(PToF Prime281, ZqBasic 2 Int64)) "F281/Q2"]
