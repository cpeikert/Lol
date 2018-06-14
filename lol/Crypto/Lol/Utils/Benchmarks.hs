{-|
Module      : Crypto.Lol.Utils.Benchmarks
Description : Infrastructure for benchmarking Lol.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Infrastructure for benchmarking Lol.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Utils.Benchmarks (benchGroup, mkBench, mkBenchIO, Benchmark) where
import Control.DeepSeq
import Control.Monad.Random
import qualified Criterion as C
import Data.Proxy

-- | Make a `Benchmark` from a function and its input
mkBench :: NFData b => String -> (a -> b) -> a -> Benchmark
mkBench name f input = C.bench name $ C.nf f input

-- | Make a `Benchmark` from an IO function and its input
mkBenchIO :: NFData b => String -> IO b -> Benchmark
mkBenchIO name i = C.bench name $ C.nfIO i

-- | Synonym for Criterion's `bgroup`
benchGroup :: String -> [Benchmark] -> Benchmark
benchGroup = C.bgroup

-- | Synonym for Criterion's `Benchmark`
type Benchmark = C.Benchmark
