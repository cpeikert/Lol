{-|
Module      : TestLolCPPMain
Description : Main driver for lol tests with CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol tests with CPP.
-}

module TestLolCPPMain where

import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Tests.Standard
import Data.Proxy

main :: IO ()
main = defaultTestMain (Proxy::Proxy CT)