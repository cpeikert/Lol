{-|
Module      : Crypto.Lol.Utils.PrettyPrint.Diagnostic
Description : Pretty-printing for benchmark results across levels of the Lol stack.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Pretty-printing for benchmark results across levels of the Lol stack.
-}

{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Crypto.Lol.Utils.PrettyPrint.Diagnostic
(prettyBenches
,defaultOpts
,Opts(..)
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

data Opts = Opts
  {verb          :: Verb,     -- ^ Verbosity
   levels        :: [String], -- ^ Which levels of Lol to benchmark. The empty list means run all levels.
   benches       :: [String], -- ^ Which operations to benchmark. The empty list means run all benchmarks.
   redThreshold  :: Double,   -- ^ How many times larger a benchmark
                              --   must be (compared to the minimum
                              --   benchmark for that parameter,
                              --   across all levels), to be printed in red
   colWidth      :: Int,      -- ^ Character width of data columns
   testNameWidth :: Int}      -- ^ Character width of row labels

-- | Runs all benchmarks with verbosity 'Progress'.
defaultOpts :: Opts
defaultOpts =
  Opts {verb = Progress,
        levels = [],
        benches = [],
        redThreshold = 1.2,
        colWidth = 15,
        testNameWidth=40}

optsToInternal :: Opts -> Benchmark -> OptsInternal
optsToInternal Opts{..} bnch =
  OptsInternal{params=[getBenchParams $ head $ benchNames bnch],
               levels=if null levels
                      then nub $ map getBenchLvl $ benchNames bnch
                      else levels,
               benches=if null benches
                       then nub $ map getBenchFunc $ benchNames bnch
                       else benches,
               ..}

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
  let colLbls = nub $ map (getBenchLvl . reportName) rpts
  printf (testName o) $ getBenchParams $ reportName $ head rpts
  mapM_ (printf (col o)) colLbls
  printf "\n"
  let rpts' = transpose $ groupBy (\a b -> getBenchLvl (reportName a) == getBenchLvl (reportName b)) rpts
  mapM_ (printRow o) rpts'
  putStrLn ""

-- See Criterion.Internal.analyseOne
printRow :: OptsInternal -> [Report] -> IO ()
printRow o@OptsInternal{..} xs@(rpt : _) = do
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
