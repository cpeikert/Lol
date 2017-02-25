{-|
Module      : SHERepaMain
Description : Example using SymmSHE with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Example using SymmSHE with RT.
-}

{-# LANGUAGE CPP #-}

module SHERepaMain where

#ifdef WITH_APPS

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Applications.Examples.SymmSHE
import Data.Proxy

main :: IO ()
main = sheMain (Proxy::Proxy RT)

#else

main :: IO ()
main = return ()

#endif