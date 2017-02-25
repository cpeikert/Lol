{-|
Module      : KHPRFCPPMain
Description : Example using KeyHomomorphicPRF with CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF with CPP.
-}

{-# LANGUAGE CPP #-}

module KHPRFCPPMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Applications.Examples.KHPRF
import Data.Proxy

main :: IO ()
main = khprfRingMain (Proxy::Proxy CT)

#else

main :: IO ()
main = return ()

#endif