{-|
Module      : TestAppsRepaMain
Description : Main driver for lol-apps tests with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol-apps tests with RT.
-}

{-# LANGUAGE CPP #-}

module TestAppsRepaMain where

#ifdef WITH_APPS

import Crypto.Lol (TrivGad)
import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Tests.Standard
import Data.Proxy

import Test.Framework

main :: IO ()
main = do
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"] $
    defaultTests (Proxy::Proxy RT) (Proxy::Proxy TrivGad)

#else

main :: IO ()
main = return ()

#endif