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
( module Crypto.Lol.Applications.Benchmarks.Default
, module Crypto.Lol.Applications.Benchmarks.BGVBenches
, module Crypto.Lol.Applications.Benchmarks.KHPRFBenches
) where

import Crypto.Lol.Applications.Benchmarks.Default
import Crypto.Lol.Applications.Benchmarks.BGVBenches
import Crypto.Lol.Applications.Benchmarks.KHPRFBenches
