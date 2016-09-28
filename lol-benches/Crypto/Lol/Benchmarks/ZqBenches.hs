{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             NoImplicitPrelude, RankNTypes, TypeFamilies #-}

module Crypto.Lol.Benchmarks.ZqBenches (zqBenches) where

import Crypto.Lol

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa as R

import Utils
import GenArgs
import Benchmarks

type Arr = R.Array R.U R.DIM1

zqBenches :: IO Benchmark
zqBenches = benchGroup "ZqBasic" [
  hideArgs bench_mul_unb (Proxy::Proxy (Zq 577)),
  hideArgs bench_mul_repa (Proxy::Proxy (Zq 577))
  ]

bench_mul_repa :: (Ring zq, U.Unbox zq) => Arr zq -> Arr zq -> Bench zq
bench_mul_repa a = bench (R.computeUnboxedS . R.zipWith (*) a)

bench_mul_unb :: (Ring zq, U.Unbox zq) => U.Vector zq -> U.Vector zq -> Bench zq
bench_mul_unb a = bench (U.zipWith (*) a)

vecLen :: Int
vecLen = 100

instance (U.Unbox zq, Random zq, MonadRandom rnd) => Generatable rnd (Arr zq) where
  genArg = R.fromListUnboxed (R.Z R.:. vecLen) <$> replicateM vecLen getRandom

instance (U.Unbox zq, Random zq, MonadRandom rnd) => Generatable rnd (U.Vector zq) where
  genArg = U.fromList <$> replicateM vecLen getRandom