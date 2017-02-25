{-|
Module      : HomomPRFCPPMain
Description : Example, test, and macro-benchmark for homomorphic evaluation of a PRF with CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example, test, and macro-benchmark for homomorphic evaluation of a PRF with CPP.
-}

{-# LANGUAGE CPP #-}

module HomomPRFCPPMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Applications.Examples.HomomPRF
import Data.Proxy

main :: IO ()
main = homomPRFMain (Proxy::Proxy CT)

#else

main :: IO ()
main = return ()

#endif