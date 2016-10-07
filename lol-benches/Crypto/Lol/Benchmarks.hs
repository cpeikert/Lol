{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Infrastructure for benchmarking Lol.

module Crypto.Lol.Benchmarks
(Crypto.Lol.Benchmarks.bench
,benchIO
,benchGroup
,genBenchArgs
,Bench(..)
,Benchmark
,NFData
,addGen) where

import Criterion as C
import Crypto.Lol.Utils.GenArgs
import Control.DeepSeq
import Data.Proxy

-- | Convenience function for benchmarks with an extra parameter.
addGen :: Proxy gen -> Proxy '(t,m,r) -> Proxy '(t,m,r,gen)
addGen _ _ = Proxy

-- | Wrapper for criterion's 'nf'
{-# INLINABLE bench #-}
bench :: NFData b => (a -> b) -> a -> Bench params
bench f = Bench . nf f

-- | Wrapper for criterion's 'nfIO'
benchIO :: NFData b => IO b -> Bench params
benchIO = Bench . nfIO

{-# INLINABLE benchGroup #-}
-- | Wrapper for criterion's 'bgroup'
benchGroup :: (Monad rnd) => String -> [rnd Benchmark] -> rnd Benchmark
benchGroup str = (bgroup str <$>) . sequence

-- | Converts a function mapping zero or more arguments to a 'Bench' @a@
-- by generating random inputs to the function
genBenchArgs :: (GenArgs rnd bnch, Monad rnd, ResultOf bnch ~ Bench a)
  => String -> bnch -> Proxy a -> rnd Benchmark
genBenchArgs s f _ = (C.bench s . unbench) <$> genArgs f

-- | Wrapper around criterion's 'Benchmarkable', with phantom parameters.
newtype Bench params = Bench {unbench :: Benchmarkable}

instance (Monad rnd) => GenArgs rnd (Bench params) where
  genArgs = return
