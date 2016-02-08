{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude, PolyKinds, RankNTypes, TypeFamilies #-}

module ZqBenches (zqBenches) where

import Utils

import Crypto.Lol

import Control.Applicative
import Control.Monad.Random
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa as R
import GHC.TypeLits

type Arr = R.Array R.U R.DIM1
type ZqB q = ZqBasic q Int64

zqBenches :: MonadRandom rnd => rnd Benchmark
zqBenches = bgroupRnd "ZqBasic" $
            [ wrapZq bench_mul_unb $ (Proxy::Proxy 577),
              wrapZq bench_mul_repa $ (Proxy::Proxy 577)
            ]

bench_mul_repa :: (Ring (ZqB q)) => Arr (ZqB q) -> Arr (ZqB q) -> NFValue
bench_mul_repa a b = nf (R.computeUnboxedS . R.zipWith (*) a) b

bench_mul_unb :: (Ring (ZqB q)) => U.Vector (ZqB q) -> U.Vector (ZqB q) -> NFValue
bench_mul_unb a b = nf (U.zipWith (*) a) b


wrapZq :: (MonadRandom rnd, Benchmarkable rnd (v (ZqB q) -> a), ShowArgs q)
          => (v (ZqB q) -> a) -> Proxy q -> rnd Benchmark
wrapZq f pq = bench (showArgs pq) <$> genArgs f

monomorphize :: (forall q . KnownNat q => Proxy q -> rnd Benchmark)
             -> rnd Benchmark
monomorphize f = f (Proxy::Proxy 577)
