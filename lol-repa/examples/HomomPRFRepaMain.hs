{-|
Module      : HomomPRFRepaMain
Description : Example, test, and macro-benchmark for homomorphic evaluation of a PRF with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example, test, and macro-benchmark for homomorphic evaluation of a PRF with RT.
-}

module HomomPRFRepaMain where

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Examples
import Data.Proxy

main :: IO ()
main = homomPRFMain (Proxy::Proxy RT)
