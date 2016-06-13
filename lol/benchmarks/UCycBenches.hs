{-# LANGUAGE DataKinds, FlexibleContexts,
             NoImplicitPrelude, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies,
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Apply.Cyc
import Benchmarks
import Utils

import Crypto.Lol
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor

ucycBenches :: IO Benchmark
ucycBenches = benchGroup "UCyc" [
  benchGroup "l"     $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_l,
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
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]
type TwoIdxParams = ( '(,) <$> Tensors) <*> MM'RCombos
twoIdxParams :: Proxy TwoIdxParams
twoIdxParams = Proxy
