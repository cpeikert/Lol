{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

-- | Pretty-printing for benchmark results.

module Crypto.Lol.Utils.PrettyPrint.Table
(prettyBenches
,defaultOpts
,Opts(..)
,Verb(..)) where

import Control.Monad (forM_, when)

import Criterion.Types

import Criterion.Measurement (secs)

import Crypto.Lol.Utils.PrettyPrint

import Data.List (nub, groupBy, transpose)

import System.IO
import Text.Printf


data Opts = Opts
  {verb          :: Verb,     -- ^ Verbosity
   level         :: String,   -- ^ Which level of Lol to benchmark
   benches       :: [String], -- ^ Which operations to benchmark. The empty list means run all benchmarks.
   params        :: [String], -- ^ Which parameters to benchmark. The empty list means run all parameters.
   colWidth      :: Int,      -- ^ Character width of data columns
   testNameWidth :: Int}      -- ^ Character width of row labels

optsToInternal :: Opts -> Benchmark -> OptsInternal
optsToInternal Opts{..} bnch =
  OptsInternal{params=if null params
                      then nub $ map getBenchParams $ benchNames bnch
                      else params,
               levels = [level],
               benches=if null benches
                       then nub $ map getBenchFunc $ benchNames bnch
                       else benches,
               redThreshold = 0,
               ..}

-- | Runs all benchmarks with verbosity 'Progress'.
defaultOpts :: String -> Opts
defaultOpts level =
  Opts {verb = Progress,
        benches = [],
        params = [],
        colWidth = 30,
        testNameWidth=20, ..}

-- | Takes benchmark options an a benchmark group nested as params/level/op,
-- and prints a table comparing operations across all selected levels of Lol.
prettyBenches :: Opts -> Benchmark-> IO ()
prettyBenches o bnch = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  let o'@OptsInternal{..} = optsToInternal o bnch
  rpts <- getReports o' bnch
  when (verb == Progress) $ putStrLn ""
  printTable o' $ reverse rpts

printTable :: OptsInternal -> [Report] -> IO ()
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
printRow :: OptsInternal -> [Report] -> IO ()
printRow o@OptsInternal{..} xs@(rpt : _) = do
  printf (testName o) $ getBenchFunc $ reportName rpt
  let times = map (secs . getRuntime) xs
  forM_ times (printf (col o))
  putStrLn ""
