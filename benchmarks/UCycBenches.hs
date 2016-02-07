{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses,NoImplicitPrelude, RankNTypes, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, 
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Criterion

import Crypto.Lol hiding (Cyc, adviseCRT)
import Crypto.Lol.Cyclotomic.UCyc

ucycBenches :: MonadRandom rnd => rnd Benchmark
ucycBenches = bench "UCyc *" <$> 
              wrap_mul (Proxy::Proxy '(RT,F288,ZqBasic 577 Int64))

bench_mul :: (Ring (UCyc t m r), NFData (UCyc t m r))
             => UCyc t m r -> UCyc t m r -> Benchmarkable
bench_mul a b = nf (a *) b

wrap_mul :: forall t m r rnd .
            (Random (UCyc t m r), Ring (UCyc t m r), NFData (UCyc t m r), 
             MonadRandom rnd)
            => Proxy '(t,m,r) -> rnd Benchmarkable
wrap_mul _ = do a :: UCyc t m r <- getRandom
                b <- getRandom
                return $! bench_mul a b

