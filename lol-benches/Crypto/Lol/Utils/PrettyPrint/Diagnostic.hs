{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printing for benchmark results.

module Crypto.Lol.Utils.PrettyPrint.Diagnostic
(prettyBenches
,defaultOpts
,opts
,Verb(..)) where

--import Control.DeepSeq
--import Control.Exception (evaluate)
import Control.Monad (forM_, when)

import Criterion.Measurement (secs)
import Criterion.Types

import Crypto.Lol.Utils.PrettyPrint

import Data.List (nub, groupBy, transpose)

import System.Console.ANSI
import System.IO
import Text.Printf

opts :: Verb -> [String] -> [String] -> Double -> Int -> Int -> Opts
opts verb levels benches redThreshold colWidth testNameWidth = Opts{params=[],..}

-- | Runs all benchmarks with verbosity 'Progress'.
defaultOpts :: [String] -> [String] -> Opts
defaultOpts levels benches =
  Opts {verb = Progress,
        params = [], -- set by prettyBenches
        redThreshold = 1.2,
        colWidth = 15,
        testNameWidth=40, ..}

-- | Takes benchmark options an a benchmark group nested as params/level/op,
-- and prints a table comparing operations across all selected levels of Lol.
prettyBenches :: Opts -> Benchmark-> IO ()
prettyBenches o@Opts{..} bnch = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  let o' = o{params=[getBenchParams $ head $ benchNames bnch]}
  rpts <- getReports o' bnch
  when (verb == Progress) $ putStrLn ""
  printTable o' $ reverse rpts

printTable :: Opts -> [Report] -> IO ()
printTable _ [] = return ()
printTable o rpts = do
  let colLbls = nub $ map (getBenchLvl . reportName) rpts
  printf (testName o) $ getBenchParams $ reportName $ head rpts
  mapM_ (printf (col o)) colLbls
  printf "\n"
  let rpts' = transpose $ groupBy (\a b -> getBenchLvl (reportName a) == getBenchLvl (reportName b)) rpts
  mapM_ (printRow o) rpts'
  putStrLn ""

-- See Criterion.Internal.analyseOne
printRow :: Opts -> [Report] -> IO ()
printRow o@Opts{..} xs@(rpt : _) = do
  printf (testName o) $ getBenchFunc $ reportName rpt
  let times = map getRuntime xs
      minTime = minimum times
      printCol t =
        if t > (redThreshold*minTime)
        then do
          setSGR [SetColor Foreground Vivid Red]
          printf (col o) $ secs t
          setSGR [Reset]
        else printf (col o) $ secs t
  forM_ times printCol
  putStrLn ""
