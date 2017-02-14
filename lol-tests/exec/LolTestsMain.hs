{-|
Module      : LolTestsMain
Description : Main driver for Zq tests.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for Zq tests.
-}

module LolTestsMain where

import Crypto.Lol.Tests.Standard
import Test.Framework

main :: IO ()
main = flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
  [zqTs]
