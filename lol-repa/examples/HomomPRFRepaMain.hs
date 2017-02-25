{-|
Module      : HomomPRFRepaMain
Description : Example, test, and macro-benchmark for homomorphic evaluation of a PRF with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example, test, and macro-benchmark for homomorphic evaluation of a PRF with RT.
-}

{-# LANGUAGE CPP #-}

module HomomPRFRepaMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Examples.HomomPRF
import Data.Proxy

main :: IO ()
main = homomPRFMain (Proxy::Proxy RT)

#else

main :: IO ()
main = return ()

#endif