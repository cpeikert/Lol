{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}

module Benchmarks
(Benchmarks.bench
,benchIO
,benchGroup
,hideArgs
,Bench(..)
,Benchmark
,NFData
,addGen

,prettyBenches
,printTable
,getReports
,defaultWidthOpts
,Opts(..)
,Verb(..)) where

import Criterion as C
import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion, withConfig)
import Criterion.Types
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Exception (evaluate)

import Control.DeepSeq

import Data.List (transpose)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy

import GenArgs
import Utils

import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Console.ANSI
import System.IO
import Text.Printf

addGen :: Proxy gen -> Proxy '(t,m,r) -> Proxy '(t,m,r,gen)
addGen _ _ = Proxy

{-# INLINABLE bench #-}
-- wrapper for Criterion's `nf`
bench :: NFData b => (a -> b) -> a -> Bench params
bench f = Bench . nf f

-- wrapper for Criterion's `nfIO`
benchIO :: NFData b => IO b -> Bench params
benchIO = Bench . nfIO

{-# INLINABLE benchGroup #-}
-- wrapper for Criterion's
benchGroup :: (Monad rnd) => String -> [rnd Benchmark] -> rnd Benchmark
benchGroup str = (bgroup str <$>) . sequence

-- normalizes any function resulting in a Benchmark to
-- one that takes a proxy for its arguments
hideArgs :: (GenArgs rnd bnch, Monad rnd, ShowType a,
             ResultOf bnch ~ Bench a)
  => String -> bnch -> Proxy a -> rnd Benchmark
hideArgs s f p = (C.bench (s ++ "/" ++ showType p) . unbench) <$> genArgs f

newtype Bench params = Bench {unbench :: Benchmarkable}

instance (Monad rnd) => GenArgs rnd (Bench params) where
  type ResultOf (Bench params) = Bench params
  genArgs = return

-- everything below this line is for printing tables of benchmark results

data Verb = Progress | Abridged | Full deriving (Eq)

data Opts = Opts {verb :: Verb, layers :: [String], benches :: [String], redThreshold :: Double, colWidth :: Int, testNameWidth :: Int}

defaultWidthOpts :: Verb -> [String] -> [String] -> Opts
defaultWidthOpts verb layers benches = Opts {colWidth = 15, testNameWidth=40, redThreshold = 1.1, ..}

prettyBenches :: Opts -> [IO Benchmark]-> IO ()
prettyBenches o@Opts{..} reports = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  rpts <- mapM (getReports o =<<) reports
  when (verb == Progress) $ putStrLn ""
  printTable o $ map reverse rpts

printTable :: Opts -> [[Report]] -> IO ()
printTable o rpts' = do
  let rpts = filter (not . null) rpts'
      colLbls = map (takeWhile (/= '/') . reportName . head) rpts
  printf (testName o) ""
  mapM_ (\lbl -> printf (col o) lbl) colLbls
  printf "\n"
  mapM_ (printRow o) $ transpose rpts

col, testName :: Opts -> String
testName Opts{..} = "%-" ++ (show testNameWidth) ++ "s "
col Opts{..} = "%-" ++ (show colWidth) ++ "s "

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
  printf (testName o) $ stripOuterGroup $ reportName rpt
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

stripOuterGroup :: String -> String
stripOuterGroup = tail . dropWhile (/= '/')

getReports :: Opts -> Benchmark -> IO [Report]
getReports o = withConfig (config o) . runAndAnalyse o

-- | Run, and analyse, one or more benchmarks.
-- From Criterion.Internal
runAndAnalyse :: Opts -> Benchmark
              -> Criterion [Report]
runAndAnalyse o@Opts{..} bs = for o bs $ \idx desc bm -> do
  when (verb == Abridged || verb == Full) $ liftIO $ putStr $ "benchmark " ++ desc
  when (verb == Full) $ liftIO $ putStrLn ""
  (Analysed rpt) <- runAndAnalyseOne idx desc bm
  when (verb == Progress) $ liftIO $ putStr "."
  when (verb == Abridged) $ liftIO $ putStrLn $ "..." ++ (secs $ getRuntime rpt)
  return rpt

-- | Iterate over benchmarks.
-- From Criterion.Internal
for :: MonadIO m => Opts -> Benchmark
    -> (Int -> String -> Benchmarkable -> m a) -> m [a]
for Opts{..} bs0 handle = snd <$> go (0::Int, []) ("", bs0)
  where
    select name =
      let lvl = takeWhile (/= '/') name
          bnch = takeWhile (/= '/') $ stripOuterGroup name
      in (lvl `elem` layers) && (bnch `elem` benches)
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
      | otherwise    = return (idx, drs)
      where desc' = addPrefix pfx desc
    go (!idx,drs) (pfx, BenchGroup desc bs) =
      foldM go (idx,drs) [(addPrefix pfx desc, b) | b <- bs]

    shouldRun pfx mkbench =
      any (select . addPrefix pfx) . benchNames . mkbench $
      error "Criterion.env could not determine the list of your benchmarks since they force the environment (see the documentation for details)"