{-|
Module      : BenchLolRepaMain
Description : Main driver for lol benchmarks with RT.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Main driver for lol benchmarks with RT.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeOperators         #-}

module BenchLolRepaMain where

import Crypto.Lol.Benchmarks
import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Factored
import Crypto.Random.DRBG

import Data.Proxy

-- choose which layers of Lol to benchmark
ls :: [String]
ls = [
  "STensor",
  "Tensor",
  "SUCyc",
  "UCyc",
  "Cyc"
  ]

-- choose which operations to benchmark
bs :: [String]
bs = [
  "unzipPow",
  "unzipDec",
  "unzipCRT",
  "zipWith (*)",
  "crt",
  "crtInv",
  "l",
  "lInv",
  "*g Pow",
  "*g Dec",
  "*g CRT",
  "divg Pow",
  "divg Dec",
  "divg CRT",
  "lift",
  "error",
  "twacePow",
  "twaceDec",
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"
  ]

main :: IO ()
main = diagnosticMain
{-
tableMain :: IO ()
tableMain = do
  let opts = (defaultTableOpts $ Just "UCyc"){benches=bs}
  g1 <- defaultLolBenches (Proxy::Proxy RT) (Proxy::Proxy HashDRBG)
  mapM_ (prettyBenchesTable opts) g1
-}
diagnosticMain :: IO ()
diagnosticMain = do
  let opts = defaultDiagnosticOpts{levels=ls, benches=bs}
  b1 <- benchGroup "Single Index"
          [oneIdxBenches (Proxy::Proxy '(F64*F9*F25, Zq 14401)) (Proxy::Proxy RT) (Proxy::Proxy HashDRBG)]
  b2 <- benchGroup "Twace-Embed"
          [twoIdxBenches (Proxy::Proxy '(F64*F9*F25, F64*F9*F25, Zq 14401)) (Proxy::Proxy RT)]
  mapM_ (prettyBenchesDiagnostic opts) [b1,b2]
