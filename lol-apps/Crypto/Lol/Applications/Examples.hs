{-|
Module      : Crypto.Lol.Applications.Examples
Description : Example cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic example cryptographic applications.
-}

module Crypto.Lol.Applications.Examples
(module Crypto.Lol.Applications.Examples.HomomPRF
,module Crypto.Lol.Applications.Examples.KHPRF
,module Crypto.Lol.Applications.Examples.SymmSHE) where

import Crypto.Lol.Applications.Examples.HomomPRF
import Crypto.Lol.Applications.Examples.KHPRF
import Crypto.Lol.Applications.Examples.SymmSHE