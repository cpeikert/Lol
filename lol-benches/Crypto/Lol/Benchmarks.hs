{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Benchmarks
(Crypto.Lol.Benchmarks.bench
,benchIO
,benchGroup
,hideArgs
,Bench(..)
,Benchmark
,NFData
,addGen) where

import Criterion as C

import Control.DeepSeq

import Data.Proxy

import Crypto.Lol.Utils.GenArgs

addGen :: Proxy gen -> Proxy '(t,m,r) -> Proxy '(t,m,r,gen)
addGen _ _ = Proxy

{-# INLINABLE bench #-}
-- wrapper for Criterion's `nf`
bench :: NFData b => (a -> b) -> a -> Bench params
bench f = Bench . nf f

-- wrapper for Criterion's `nfIO`
benchIO :: NFData b => IO b -> Bench params
benchIO = Bench . nfIO

{-# INLINABLE benchGroup #-}
-- wrapper for Criterion's
benchGroup :: (Monad rnd) => String -> [rnd Benchmark] -> rnd Benchmark
benchGroup str = (bgroup str <$>) . sequence

-- normalizes any function resulting in a Benchmark to
-- one that takes a proxy for its arguments
hideArgs :: (GenArgs rnd bnch, Monad rnd, ResultOf bnch ~ Bench a)
  => String -> bnch -> Proxy a -> rnd Benchmark
hideArgs s f _ = (C.bench s . unbench) <$> genArgs f

newtype Bench params = Bench {unbench :: Benchmarkable}

instance (Monad rnd) => GenArgs rnd (Bench params) where
  genArgs = return
