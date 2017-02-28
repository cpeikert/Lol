{-|
Module      : Crypto.Lol.Applications.Examples
Description : Benchmarks for cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic benchmarks for  cryptographic applications.
-}

module Crypto.Lol.Applications.Benchmarks (module X) where

import Crypto.Lol.Applications.Benchmarks.Default      as X
import Crypto.Lol.Applications.Benchmarks.KHPRFBenches as X
import Crypto.Lol.Applications.Benchmarks.SHEBenches   as X