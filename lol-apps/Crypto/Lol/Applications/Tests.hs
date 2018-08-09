{-|
Module      : Crypto.Lol.Applications.Tests
Description : Tests for cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic tests for  cryptographic applications.
-}

module Crypto.Lol.Applications.Tests
(
-- * Default tests and parameters
  module Crypto.Lol.Applications.Tests.Default
-- * Tests for various types of functions in SymmSHE
, module Crypto.Lol.Applications.Tests.SHETests
) where

import Crypto.Lol.Applications.Tests.Default
import Crypto.Lol.Applications.Tests.SHETests
