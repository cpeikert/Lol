{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Crypto.Lol.Utils.PrettyPrint
(prettyBenches
,printTable
,getReports
,defaultWidthOpts
,Opts(..)
,Verb(..)) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion, withConfig)
import Criterion.Types

import Data.List (nub, groupBy, transpose)
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe

import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Console.ANSI
import System.IO
import Text.Printf

data Verb = Progress | Abridged | Full deriving (Eq)

data Opts = Opts {verb          :: Verb,
                  layers        :: [String],
                  benches       :: [String],
                  redThreshold  :: Double,
                  colWidth      :: Int,
                  testNameWidth :: Int}

defaultWidthOpts :: Verb -> [String] -> [String] -> Opts
defaultWidthOpts verb layers benches = Opts {colWidth = 15, testNameWidth=40, redThreshold = 1.1, ..}

prettyBenches :: Opts -> Benchmark-> IO ()
prettyBenches o@Opts{..} bnch = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  rpts <- getReports o bnch
  when (verb == Progress) $ putStrLn ""
  printTable o $ reverse rpts

printTable :: Opts -> [Report] -> IO ()
printTable _ [] = return ()
printTable o rpts = do
  let colLbls = nub $ map getBenchLvl rpts
  printf (testName o) $ getBenchParams $ head rpts
  mapM_ (printf (col o)) colLbls
  printf "\n"
  let rpts' = transpose $ groupBy (\a b -> getBenchLvl a == getBenchLvl b) rpts
  mapM_ (printRow o) rpts'
  putStrLn ""

parseBenchName :: String -> [String]
parseBenchName = wordsBy (=='/')

getBenchParams :: Report -> String
getBenchParams = head . parseBenchName . reportName

getBenchLvl :: Report -> String
getBenchLvl = head . tail . parseBenchName . reportName

getBenchFunc :: Report -> String
getBenchFunc = head . tail . tail . parseBenchName . reportName

col, testName :: Opts -> String
testName Opts{..} = "%-" ++ show testNameWidth ++ "s "
col Opts{..} = "%-" ++ show colWidth ++ "s "

config :: Opts -> Config
config Opts{..} = defaultConfig {verbosity = if verb == Full then Normal else Quiet}

getRuntime :: Report -> Double
getRuntime Report{..} =
  let SampleAnalysis{..} = reportAnalysis
      (builtin, _) = splitAt 1 anRegress
      mests = map (\Regression{..} -> Map.lookup "iters" regCoeffs) builtin
      [Estimate{..}] = catMaybes mests
  in estPoint

-- See Criterion.Internal.analyseOne
printRow :: Opts -> [Report] -> IO ()
printRow o@Opts{..} xs@(rpt : _) = do
  printf (testName o) $ getBenchFunc rpt
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

getReports :: Opts -> Benchmark -> IO [Report]
getReports o = withConfig (config o) . runAndAnalyse o

-- | Run, and analyse, one or more benchmarks.
-- From Criterion.Internal
runAndAnalyse :: Opts -> Benchmark -> Criterion [Report]
runAndAnalyse o@Opts{..} bs = for o bs $ \idx desc bm -> do
  when (verb == Abridged || verb == Full) $ liftIO $ putStr $ "benchmark " ++ desc
  when (verb == Full) $ liftIO $ putStrLn ""
  (Analysed rpt) <- runAndAnalyseOne idx desc bm
  when (verb == Progress) $ liftIO $ putStr "."
  when (verb == Abridged) $ liftIO $ putStrLn $ "..." ++ secs (getRuntime rpt)
  return rpt

-- | Iterate over benchmarks.
-- From Criterion.Internal
for :: MonadIO m => Opts -> Benchmark -> (Int -> String -> Benchmarkable -> m a) -> m [a]
for Opts{..} bs0 handle = snd <$> go (0::Int, []) ("", bs0)
  where
    select name =
      let [_,lvl,func] = parseBenchName name
      in (lvl `elem` layers) && (func `elem` benches)
    go (!idx,drs) (pfx, Environment mkenv mkbench)
      | shouldRun pfx mkbench = do
        e <- liftIO $ do
          ee <- mkenv
          evaluate (rnf ee)
          return ee
        go (idx,drs) (pfx, mkbench e)
      | otherwise = return (idx,drs)
    go (!idx, drs) (pfx, Benchmark desc b)
      | select desc' = do
          x <- handle idx desc' b;
          return (idx + 1, x:drs)
      | otherwise = --do
          --liftIO $ putStrLn $ "2: " ++ desc'
          return (idx, drs)
      where desc' = addPrefix pfx desc
    go (!idx,drs) (pfx, BenchGroup desc bs) =
      foldM go (idx,drs) [(addPrefix pfx desc, b) | b <- bs]

    shouldRun pfx mkbench =
      any (select . addPrefix pfx) . benchNames . mkbench $
      error "Criterion.env could not determine the list of your benchmarks since they force the environment (see the documentation for details)"
