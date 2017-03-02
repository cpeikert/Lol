{-|
Module      : Crypto.Lol.Applications.Examples
Description : Benchmarks for cryptographic applications.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tensor-polymorphic benchmarks for cryptographic applications. Note that
benchmarks for HomomPRF are included in the example.
-}

module Crypto.Lol.Applications.Benchmarks
(
-- * Default benchmarks and parameters
 module Crypto.Lol.Applications.Benchmarks.Default
-- * Benchmarks for the key-homomorphic PRF
,module Crypto.Lol.Applications.Benchmarks.KHPRFBenches
-- * Benchmarks for various types of functions in SymmSHE
,module Crypto.Lol.Applications.Benchmarks.SHEBenches) where

import Crypto.Lol.Applications.Benchmarks.Default
import Crypto.Lol.Applications.Benchmarks.KHPRFBenches
import Crypto.Lol.Applications.Benchmarks.SHEBenches