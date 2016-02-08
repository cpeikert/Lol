{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses,NoImplicitPrelude, RankNTypes, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, 
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Control.Applicative
import Control.Monad.Random
import Criterion

import Crypto.Lol hiding (Cyc, adviseCRT)
import Crypto.Lol.Cyclotomic.UCyc

proxies = Proxy::Proxy '(RT,F128*F3,ZqBasic 769 Int64)

ucycBenches :: MonadRandom rnd => rnd Benchmark
ucycBenches = bgroup "UCyc" <$> sequence
              [bench "(*)" <$> wrap2 bench_mul proxies,
               bench "crtInv" <$> wrap1 bench_crtInv proxies,
               bench "l" <$> wrap1 bench_l proxies]

bench_mul :: (CElt t r, Fact m)
             => UCyc t m r -> UCyc t m r -> Benchmarkable
bench_mul a b = nf (a *) b

bench_crtInv :: (CElt t r, Fact m)
                   => UCyc t m r -> Benchmarkable
bench_crtInv = nf forcePow

bench_l :: (CElt t r, Fact m) => UCyc t m r -> Benchmarkable
bench_l a = let a' = forcePow a in nf forceDec a'

wrap1 :: forall t m r rnd . (CElt t r, Fact m, MonadRandom rnd)
         => (UCyc t m r -> Benchmarkable) -> Proxy '(t,m,r) -> rnd Benchmarkable
wrap1 f _ = f <$> (getRandom :: rnd (UCyc t m r))

wrap2 :: forall t m r rnd .
         (CElt t r, Fact m, MonadRandom rnd)
         => (UCyc t m r -> UCyc t m r -> Benchmarkable)
             -> Proxy '(t,m,r) -> rnd Benchmarkable
wrap2 f _ = do a :: UCyc t m r <- getRandom
               b <- getRandom
               return $! f a b

