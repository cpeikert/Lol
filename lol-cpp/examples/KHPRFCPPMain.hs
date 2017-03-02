{-|
Module      : KHPRFCPPMain
Description : Example using KeyHomomorphicPRF with CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF with CPP.
-}

module KHPRFCPPMain where

import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Applications.Examples
import Data.Proxy

main :: IO ()
main = khprfRingMain (Proxy::Proxy CT)
