{-|
Module      : Crypto.Lol.Applications.Tests
Description : Tests for cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic tests for  cryptographic applications. Note that tests for
HomomPRF are included in the example.
-}

module Crypto.Lol.Applications.Tests
(
-- * Default tests and parameters
 module Crypto.Lol.Applications.Tests.Default
-- * Tests for the key-homomorphic PRF
,module Crypto.Lol.Applications.Tests.KHPRFTests
-- * Tests for various types of functions in SymmSHE
,module Crypto.Lol.Applications.Tests.SHETests) where

import Crypto.Lol.Applications.Tests.Default
import Crypto.Lol.Applications.Tests.KHPRFTests
import Crypto.Lol.Applications.Tests.SHETests