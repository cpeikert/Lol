{-|
Module      : Crypto.Lol.Tests
Description : Infrastructure for benchmarking lol.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\C{\mathbb{C}} \)

Tests for individual layers of the Lol stack ('Crypto.Lol.Cyclotomic.Tensor',
'Crypto.Lol.Cyclotomic.UCyc', Crypto.Lol.Cyclotomic.Cyc'), plus
default parameters and helper functions for displaying results.
-}

-- EAC: See https://github.com/haskell/haddock/issues/563
module Crypto.Lol.Tests
(
-- * Default parameters for tests
 module Crypto.Lol.Tests.Default
-- * Tests for different layers of Lol
,module Crypto.Lol.Tests.TensorTests
,module Crypto.Lol.Tests.CycTests
,module Crypto.Lol.Tests.ZqTests
-- * Utilities for creating tests
,module Crypto.Lol.Utils.Tests
,module Crypto.Lol.Utils.GenArgs
-- * Utilities for showing benchmark results
,module Crypto.Lol.Utils.ShowType) where

import Crypto.Lol.Tests.Default
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.CycTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.Tests
import Crypto.Lol.Utils.GenArgs
import Crypto.Lol.Utils.ShowType