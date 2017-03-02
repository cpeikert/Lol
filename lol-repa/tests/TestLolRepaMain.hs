{-|
Module      : TestLolRepaMain
Description : Main driver for lol tests with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol tests with RT.
-}

module TestLolRepaMain where

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Tests
import Data.Proxy
import Test.Framework

main :: IO ()
main = defaultMainWithArgs
  (defaultLolTests (Proxy::Proxy RT)) ["--threads=1","--maximum-generated-tests=100"]
