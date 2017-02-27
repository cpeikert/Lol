{-|
Module      : Crypto.Lol.Applications.Examples
Description : Example cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic example cryptographic applications.
-}

module Crypto.Lol.Applications.Examples (module X) where

import Crypto.Lol.Applications.Examples.HomomPRF as X
import Crypto.Lol.Applications.Examples.KHPRF    as X
import Crypto.Lol.Applications.Examples.SymmSHE  as X