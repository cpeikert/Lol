{-|
Module      : Crypto.Lol.Applications.Tests
Description : Tests for cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic tests for  cryptographic applications.
-}

module Crypto.Lol.Applications.Tests (module X) where

import Crypto.Lol.Applications.Tests.Default    as X
import Crypto.Lol.Applications.Tests.KHPRFTests as X
import Crypto.Lol.Applications.Tests.SHETests   as X