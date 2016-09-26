{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, GADTs, KindSignatures, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, PolyKinds, RecordWildCards, TemplateHaskell, TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Benchmarks hiding (benches, layers)
import SimpleTensorBenches
import TensorBenches
import SimpleUCycBenches
import UCycBenches
import CycBenches

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Control.Applicative
import Control.Monad (when, join)

import Data.List (transpose)

import System.IO

-- choose which layers of Lol to benchmark
layers :: [String]
layers = [
  --"STensor",
  --"Tensor",
  "SUCyc",
  "UCyc",
  "Cyc"
  ]

benches :: [String]
benches = [
  {-"unzipPow",
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
  "error",-}
  "twacePow",
  "twaceDec",
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"
  ]

type T = CT
type M = F64*F9*F25 --9*F5*F7*F11
type R = Zq 1065601 --Zq 34651
type M' = M -- F3*F5*F11
type Zq (q :: k) = ZqBasic q Int64

-- The random generator used in benchmarks
type Gen = HashDRBG

testParam :: Proxy '(T, M, R)
testParam = Proxy

twoIdxParam :: Proxy '(T, M', M, R)
twoIdxParam = Proxy

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  let opts = defaultWidthOpts Progress layers benches
  reports <- join $ mapM (getReports opts) <$>
    concat <$> transpose <$>
      sequence [oneIdxBenches testParam (Proxy::Proxy Gen),
                twoIdxBenches twoIdxParam]

  when (verb opts == Progress) $ putStrLn ""
  printTable opts $ group2 $ map reverse reports

group2 :: [[a]] -> [[a]]
group2 [] = []
group2 (x:y:zs) = (x++y):(group2 zs)

--oneIdxBenches p :: IO [Benchmark]
{-# INLINABLE oneIdxBenches #-}
oneIdxBenches ptmr pgen = sequence $ (($ pgen) . ($ ptmr)) <$> [
  simpleTensorBenches1,
  tensorBenches1,
  simpleUCycBenches1,
  ucycBenches1,
  cycBenches1
  ]
{-# INLINABLE twoIdxBenches #-}
twoIdxBenches p = sequence $ ($ p) <$> [
  simpleTensorBenches2,
  tensorBenches2,
  simpleUCycBenches2,
  ucycBenches2,
  cycBenches2
  ]
