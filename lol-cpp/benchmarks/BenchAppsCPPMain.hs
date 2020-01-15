{-|
Module      : BenchAppsCPPMain
Description : Main driver for lol-apps benchmarks with CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol-apps benchmarks with CPP.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BenchAppsCPPMain where

import Crypto.Lol
import Crypto.Lol.Applications.Benchmarks
import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Random.DRBG

main :: IO ()
main = do
  let o = defaultTableOpts Nothing
      pct = Proxy::Proxy CT
  bgv   <- defaultBGVBenches pct (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG)
  khprf <- defaultKHPRFBenches pct (Proxy::Proxy (BaseBGad 2))
  mapM_ (prettyBenchesTable o) (bgv ++ khprf)
