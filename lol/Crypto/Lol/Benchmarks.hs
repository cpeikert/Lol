{-|
Module      : Crypto.Lol.Benchmarks
Description : Infrastructure for benchmarking lol.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\C{\mathbb{C}} \)

Benchmarks for individual layers of the Lol stack (Tensor, UCyc, Cyc), plus
default parameters and helper functions for diplaying results.
-}

-- EAC: See https://github.com/haskell/haddock/issues/563, which claims that
-- Haddock doesn't support "re-exporting modules". However, the following
-- method duplicates the docs of all the re-exported modules here, which is
-- what I want. It doesn't like it if I rename the modules though.
module Crypto.Lol.Benchmarks
(
-- * Default parameters for benchmarks
 module Crypto.Lol.Benchmarks.Default
-- * Benchmarks for different layers of Lol
,module Crypto.Lol.Benchmarks.TensorBenches
,module Crypto.Lol.Benchmarks.UCycBenches
,module Crypto.Lol.Benchmarks.CycBenches
-- * Utilities for creating benchmarks
,module Crypto.Lol.Utils.Benchmarks
,module Crypto.Lol.Utils.GenArgs
-- * Utilities for showing benchmark results
,module Crypto.Lol.Utils.ShowType
,Verb(..)
,module Crypto.Lol.Utils.PrettyPrint.Table
,module Crypto.Lol.Utils.PrettyPrint.Diagnostic) where

import Crypto.Lol.Utils.Benchmarks
import Crypto.Lol.Benchmarks.CycBenches
import Crypto.Lol.Benchmarks.UCycBenches
import Crypto.Lol.Benchmarks.Default
import Crypto.Lol.Benchmarks.TensorBenches
import Crypto.Lol.Utils.GenArgs
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.PrettyPrint (Verb(..))
import Crypto.Lol.Utils.PrettyPrint.Table
import Crypto.Lol.Utils.PrettyPrint.Diagnostic
