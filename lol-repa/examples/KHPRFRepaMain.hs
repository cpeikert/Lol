{-|
Module      : KHPRFRepaMain
Description : Example using KeyHomomorphicPRF with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF with RT.
-}

{-# LANGUAGE CPP #-}

module KHPRFRepaMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Examples.KHPRF
import Data.Proxy

main :: IO ()
main = khprfRingMain (Proxy::Proxy RT)

#else

main :: IO ()
main = return ()

#endif