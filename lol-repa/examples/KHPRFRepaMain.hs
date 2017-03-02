{-|
Module      : KHPRFRepaMain
Description : Example using KeyHomomorphicPRF with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using KeyHomomorphicPRF with RT.
-}

module KHPRFRepaMain where

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Examples
import Data.Proxy

main :: IO ()
main = khprfRingMain (Proxy::Proxy RT)
