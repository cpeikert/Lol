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
  benchGroup "l"      $ applyBasic (Proxy::Proxy QuickParams) $ hideArgs bench_l
  ]

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => UCyc t m D r -> Bench '(t,m,r)
bench_l = bench toPow

type QuickTest = '[ '(F128, Zq 257),
                    '(F32 * F9, Zq 577),
                    '(F32 * F9, Int64) ]
type Tensors = '[CT,RT]
type QuickParams = ( '(,) <$> Tensors) <*> QuickTest