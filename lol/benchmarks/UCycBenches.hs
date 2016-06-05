{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Benchmarks
import Harness.Cyc
import Utils

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Cyclotomic.UCyc

ucycBenches :: (MonadRandom m) => m Benchmark
ucycBenches = benchGroup "UCyc" [
  benchGroup "l"      $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_l,
  benchGroup "twace" $ applyTwoIdx twoIdxParams $ hideArgs bench_twacePow,
  benchGroup "embed" $ applyTwoIdx twoIdxParams $ hideArgs bench_embedPow
  ]

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => UCyc t m D r -> Bench '(t,m,r)
bench_l = bench toPow

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCyc t m' P r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePow :: UCyc t m' P r -> UCyc t m P r)

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCyc t m P r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: UCyc t m P r -> UCyc t m' P r)

type QuickTest = '[ '(F128, Zq 257),
                    '(F32 * F9, Zq 577),
                    '(F32 * F9, Int64) ]
type Tensors = '[CT,RT]
type QuickParams = ( '(,) <$> Tensors) <*> QuickTest

type MM'RCombos =
  '[ '(F4, F128, Zq 257),
     '(F1, PToF Prime281, Zq 563),
     '(F12, F32 * F9, Zq 512),
     '(F12, F32 * F9, Zq 577),
     '(F12, F32 * F9, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 14401)
    ]

type TwoIdxParams = ( '(,) <$> Tensors) <*> MM'RCombos
twoIdxParams :: Proxy TwoIdxParams
twoIdxParams = Proxy
