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
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Utils.Benchmarks
(mkBench, mkBenchIO
-- Re-exports
,C.bgroup, C.Benchmark) where

import Control.DeepSeq

import qualified Criterion as C

-- | Make a `Benchmark` from a function and its input
mkBench :: NFData b => String -> (a -> b) -> a -> C.Benchmark
mkBench name f input = C.bench name $ C.nf f input

-- | Make a `Benchmark` from an IO function and its input
mkBenchIO :: NFData b => String -> IO b -> C.Benchmark
mkBenchIO name i = C.bench name $ C.nfIO i
