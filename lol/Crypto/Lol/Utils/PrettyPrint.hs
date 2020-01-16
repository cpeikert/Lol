{-|
Module      : Crypto.Lol.Utils.PrettyPrint
Description : Pretty-printing for benchmark results.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Pretty-printing for benchmark results.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Crypto.Lol.Utils.PrettyPrint
( getTableName
, getBenchParams
, getBenchLvl
, getBenchFunc
, getReports
, getRuntime
, col
, testName
, OptsInternal(..)
, Verb(..)
) where

import Control.Monad          (foldM, when)
import Control.Monad.IO.Class (liftIO)

import Criterion.Internal     (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement  (initializeTime, secs)
import Criterion.Monad        (Criterion, withConfig)
import Criterion.Types

import qualified Data.Map as Map

import Statistics.Types (Estimate (..))

-- | Verbosity of benchmark output.
data Verb = Progress  -- ^ prints a \'.\' when each benchmark completes
          | Abridged  -- ^ prints a one-line summary for each benchmark
          | Full      -- ^ prints full criterion output for each benchmark
          deriving (Eq)

-- | Options for printing benchmark summary.
data OptsInternal = OptsInternal
  {verb          :: Verb,     -- ^ Verbosity
   levels        :: [String], -- ^ Which levels of Lol to benchmark
   benches       :: [String], -- ^ Which operations to benchmark
   params        :: [String], -- ^ Which parameters to benchmark
   redThreshold  :: Double,   -- ^ How many times larger a benchmark
                              --   must be (compared to the minimum
                              --   benchmark for that parameter,
                              --   across all levels), to be printed in red
   colWidth      :: Int,      -- ^ Character width of data columns
   testNameWidth :: Int}      -- ^ Character width of row labels

col, testName :: OptsInternal -> String
testName OptsInternal{..} = "%-" ++ show testNameWidth ++ "s "
col OptsInternal{..} = "%-" ++ show colWidth ++ "s "

-- get the ith 'word' where words are separated by '/'
wordBy :: Int -> String -> String
wordBy 0 = takeWhile (/= '/')
wordBy i = wordBy (i-1) . tail . dropWhile (/= '/')

getTableName :: String -> String
getTableName   = wordBy 0

getBenchParams :: String -> String
getBenchParams = wordBy 1

getBenchLvl :: String -> String
getBenchLvl    = wordBy 2

getBenchFunc :: String -> String
getBenchFunc   = wordBy 3

getReports :: OptsInternal -> Benchmark -> IO [Report]
getReports o = withConfig (config o) . summarizeBenchReports o

config :: OptsInternal -> Config
config OptsInternal{..} = defaultConfig {verbosity = if verb == Full then Normal else Quiet}

-- collect reports from all selected benchmarks, printing a summary along the way
summarizeBenchReports :: OptsInternal -> Benchmark -> Criterion [Report]
summarizeBenchReports OptsInternal{..} b = do
  liftIO initializeTime -- Workaround for criterion issue #195
  snd <$> go (0, []) ("", b)
  where
    select name =
      let param = getBenchParams name
          lvl   = getBenchLvl    name
          func  = getBenchFunc   name
      in (lvl `elem` levels) && (func `elem` benches) && (param `elem` params)
    -- if we find a Benchmark that we want to run (as determined by `select`)
    go r@(rptIdx, reports) (benchPrefix, Benchmark desc b') |
      select benchName = do
          -- get a single report
          when (verb == Abridged || verb == Full) $ liftIO $ putStr $ "benchmark " ++ benchName
          when (verb == Full) $ liftIO $ putStrLn ""
          dr <- runAndAnalyseOne rptIdx benchName b'
          case dr of
            Measurement{} -> error "PrettyPrint Measurement" -- for Wmissing-monadfail-instances
            (Analysed rpt) -> do
              when (verb == Progress) $ liftIO $ putStr "."
              when (verb == Abridged) $ liftIO $ putStrLn $ "..." ++ secs (getRuntime rpt)
              -- return the report
              return (rptIdx, rpt:reports)
                                                            |
      otherwise = do
        -- if we don't want to run this benchmark, print the name anyway.
        liftIO $ putStrLn benchName
        -- and return the input
        return r
          where benchName = addPrefix benchPrefix desc
    go r (benchPrefix, BenchGroup desc bs) =
      let lvlName = addPrefix benchPrefix desc -- append the description to the prefix
          bs' = map (lvlName,) bs
      in foldM go r bs'

-- | The report runtime, in seconds.
getRuntime :: Report -> Double
getRuntime Report{..} =
  let SampleAnalysis{..} = reportAnalysis
      Regression{..} = head anRegress
      Estimate{..} = regCoeffs Map.! "iters"
  in estPoint
