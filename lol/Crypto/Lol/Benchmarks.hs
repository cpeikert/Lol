{-|
Module      : Crypto.Lol.Benchmarks
Description : Infrastructure for benchmarking lol.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\C{\mathbb{C}} \)

Benchmarks for individual layers of the Lol stack (Tensor, UCyc, Cyc), plus
default parameters and helper functions for diplaying results.
-}

module Crypto.Lol.Benchmarks (module X) where

import Crypto.Lol.Utils.Benchmarks             as X
import Crypto.Lol.Benchmarks.CycBenches        as X
import Crypto.Lol.Benchmarks.UCycBenches       as X
import Crypto.Lol.Benchmarks.Default           as X
import Crypto.Lol.Benchmarks.TensorBenches     as X
import Crypto.Lol.Utils.ShowType               as X
import Crypto.Lol.Utils.PrettyPrint            as X (Verb(..))
import Crypto.Lol.Utils.PrettyPrint.Table      as X
import Crypto.Lol.Utils.PrettyPrint.Diagnostic as X
