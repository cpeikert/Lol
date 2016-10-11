{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printing for benchmark results.

module Crypto.Lol.Utils.PrettyPrint.Table
(prettyBenches
,defaultOpts
,opts
,Verb(..)) where

import Control.Monad (forM_, when)

import Criterion.Types

import Criterion.Measurement (secs)

import Crypto.Lol.Utils.PrettyPrint

import Data.List (nub, groupBy, transpose)

import System.IO
import Text.Printf

-- leave params empty to run all parameters
opts :: Verb -> String -> [String] -> [String] -> Int -> Int -> Opts
opts verb lvl benches params colWidth testNameWidth = Opts{levels=[lvl],redThreshold=0,..}

-- | Runs all benchmarks with verbosity 'Progress'.
defaultOpts :: String -> [String] -> Opts
defaultOpts lvl benches =
  Opts {verb = Progress,
        levels = [lvl],
        params = [],
        redThreshold = 1.2,
        colWidth = 30,
        testNameWidth=20, ..}

-- | Takes benchmark options an a benchmark group nested as params/level/op,
-- and prints a table comparing operations across all selected levels of Lol.
prettyBenches :: Opts -> Benchmark-> IO ()
prettyBenches o@Opts{..} bnch = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  let o' = if params == []
           then o{params=nub $ map getBenchParams $ benchNames bnch}
           else o
  rpts <- getReports o' bnch
  when (verb == Progress) $ putStrLn ""
  printTable o' $ reverse rpts

printTable :: Opts -> [Report] -> IO ()
printTable _ [] = return ()
printTable o rpts = do
  let colLbls = nub $ map (getBenchParams . reportName) rpts
      exName = reportName $ head rpts
  printf (testName o) $ (getTableName exName) ++ "/" ++ (getBenchLvl exName)
  mapM_ (printf (col o)) colLbls
  printf "\n"
  let rpts' = transpose $ groupBy (\a b -> getBenchParams (reportName a) == getBenchParams (reportName b)) rpts
  mapM_ (printRow o) rpts'
  putStrLn ""

-- See Criterion.Internal.analyseOne
printRow :: Opts -> [Report] -> IO ()
printRow o@Opts{..} xs@(rpt : _) = do
  printf (testName o) $ getBenchFunc $ reportName rpt
  let times = map (secs . getRuntime) xs
  forM_ times (printf (col o))
  putStrLn ""
