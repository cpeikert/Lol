{-|
Module      : BenchAppsRepaMain
Description : Main driver for lol-apps benchmarks with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol-apps benchmarks with RT.
-}

{-# LANGUAGE CPP       #-}
{-# LANGUAGE DataKinds #-}

module BenchAppsRepaMain where

#ifdef WITH_APPS

import Crypto.Lol
import Crypto.Lol.Applications.Benchmarks.Standard
import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Utils.PrettyPrint.Table

import Crypto.Random.DRBG

main :: IO ()
main = do
  let o = (defaultOpts Nothing){benches=[]}
      pct = Proxy::Proxy RT
  bs <- sequence $
          sheBenches pct (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG) ++
          [khprfBenches pct (Proxy::Proxy (BaseBGad 2))]
  mapM_ (prettyBenches o) bs

#else

main :: IO ()
main = return ()

#endif