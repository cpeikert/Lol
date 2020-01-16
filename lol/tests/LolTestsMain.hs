{-|
Module      : LolTestsMain
Description : Main driver for Lol tests.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for Lol tests.
-}

module LolTestsMain where

import Crypto.Lol.Tests
import Test.Framework

main :: IO ()
main = defaultMainWithArgs
  [defaultZqTests] ["--threads=1","--maximum-generated-tests=20"]
