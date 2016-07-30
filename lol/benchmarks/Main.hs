{-# LANGUAGE BangPatterns, DataKinds, FlexibleContexts, GADTs, KindSignatures, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, PolyKinds, RecordWildCards, TemplateHaskell, TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Benchmarks
import SimpleTensorBenches
import TensorBenches
import SimpleUCycBenches
import UCycBenches
import CycBenches

import Crypto.Lol
import Crypto.Lol.Types

import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion, withConfig)
import Criterion.Types
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Applicative
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)

import Data.List (transpose)
import qualified Data.Map as Map
import Data.Maybe

import Statistics.Resampling.Bootstrap (Estimate(..))
import System.Console.ANSI
import System.IO
import Text.Printf

-- table print parameters
colWidth, testNameWidth :: Int
colWidth = 15
testNameWidth = 40
verb :: Verb
verb = Progress
redThreshold :: Double
redThreshold = 1.1

-- choose which layers of Lol to benchmark
layers :: [String]
layers = [
  "STensor",
  "Tensor",
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
  "*g CRT",-}
  "divg Pow",
  "divg Dec",
  "divg CRT",
  "lift",
  "error"{-,
  "twacePow",
  "twaceDec",
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"-}
  ]

data Verb = Progress | Abridged | Full deriving (Eq)

type T = CT
type M = F9*F5*F7*F11
type R = Zq 34651
type M' = F3*F5*F11
type Zq (q :: k) = ZqBasic q Int64

testParam :: Proxy '(T, M, R)
testParam = Proxy

twoIdxParam :: Proxy '(T, M', M, R)
twoIdxParam = Proxy

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  reports <- mapM (getReports =<<) [
    simpleTensorBenches1 testParam,
    simpleTensorBenches2 twoIdxParam,
    tensorBenches1 testParam,
    tensorBenches2 twoIdxParam,
    simpleUCycBenches1 testParam,
    simpleUCycBenches2 twoIdxParam,
    ucycBenches1 testParam,
    ucycBenches2 twoIdxParam,
    cycBenches1 testParam,
    cycBenches2 twoIdxParam
    ]
  when (verb == Progress) $ putStrLn ""
  printTable $ group2 $ map reverse reports

group2 :: [[a]] -> [[a]]
group2 [] = []
group2 (x:y:zs) = (x++y):(group2 zs)

{-
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  reports1 <- mapM (mapM getReports) =<< benches1
  reports2 <- mapM (mapM getReports) =<< benches2
      -- 1. reports1 has
      --      [[[TensorBenchesParam1], [UCycBenchesParam1]],
      --       [[TensorBenchesParam2], [UCycBenchesParam2]] ...]
      -- 2. transpose to get benchmarks in a *layer* in a row
      --      [[[TensorBenchesParam1], [TensorBenchesParam2], ...],
      --       [[UCycBenchesParam1], [UCycBenchesParam2], ...]]
      -- 3. map transpose to get benchmark for one function in a row
      --      [[[T.crt_param1, T.crt_param2, ...],
      --        [T.l_param1, T.l_param2, ...],
      --        ...],
      --       [same for UCyc]]
      -- 4. map concat so that all benches for a single layer are grouped
      --      [[T.crt_param1, T.crt_param2, ..., T.l_param1, T.l_param2, ...],
      --       [same for UCyc]]
  let reports1' = map (concat . transpose) $ transpose reports1
      reports2' = map (concat . transpose) $ transpose reports2
      -- append benchmarks for each layer
      reports = filter (not . null) $ zipWith (++) reports1' reports2'

  when (verb == Progress) $ putStrLn ""
  printTable reports

flattenTriple :: Proxy '(a, '(b,c)) -> Proxy '(a,b,c)
flattenTriple _ = Proxy

flattenQuadruple :: Proxy '(a, '(b,c,d)) -> Proxy '(a,b,c,d)
flattenQuadruple _ = Proxy

benches1, benches2 :: IO [[Benchmark]]
benches1 = sequence $(applyBenchN 'oneIdxBenches params1)
benches2 = sequence $(applyBenchN 'twoIdxBenches params2)

oneIdxBenches param =
  let p = flattenTriple param
  in sequence $ [
      simpleTensorBenches1 p,
      tensorBenches1 p,
      simpleUCycBenches1 p,
      ucycBenches1 p,
      cycBenches1 p
      ] :: IO [Benchmark]

twoIdxBenches param =
  let p = flattenQuadruple param
  in sequence [
      simpleTensorBenches2 p,
      tensorBenches2 p,
      simpleUCycBenches2 p,
      ucycBenches2 p,
      cycBenches2 p
      ] :: IO [Benchmark]

-}
getReports :: Benchmark -> IO [Report]
getReports = withConfig config . runAndAnalyse

printTable :: [[Report]] -> IO ()
printTable rpts = do
  let colLbls = map (takeWhile (/= '/') . reportName . head) rpts
  printf testName ""
  mapM_ (\lbl -> printf col lbl) colLbls
  printf "\n"
  mapM_ printRow $ transpose rpts

col, testName :: String
testName = "%-" ++ (show testNameWidth) ++ "s "
col = "%-" ++ (show colWidth) ++ "s "

printANSI :: (MonadIO m) => Color -> String -> m ()
printANSI sgr str = liftIO $ do
  setSGR [SetColor Foreground Vivid sgr]
  putStrLn str
  setSGR [Reset]

config :: Config
config = defaultConfig {verbosity = if verb == Full then Normal else Quiet}

getRuntime :: Report -> Double
getRuntime Report{..} =
  let SampleAnalysis{..} = reportAnalysis
      (builtin, _) = splitAt 1 anRegress
      mests = map (\Regression{..} -> Map.lookup "iters" regCoeffs) builtin
      [Estimate{..}] = catMaybes mests
  in estPoint

-- See Criterion.Internal.analyseOne
printRow :: [Report] -> IO ()
printRow xs@(rpt : _) = do
  printf testName $ stripOuterGroup $ reportName rpt
  let times = map getRuntime xs
      minTime = minimum times
      printCol t =
        if t > (redThreshold*minTime)
        then do
          setSGR [SetColor Foreground Vivid Red]
          printf col $ secs t
          setSGR [Reset]
        else printf col $ secs t
  forM_ times printCol
  putStrLn ""

stripOuterGroup :: String -> String
stripOuterGroup = tail . dropWhile (/= '/')

-- | Run, and analyse, one or more benchmarks.
-- From Criterion.Internal
runAndAnalyse :: Benchmark
              -> Criterion [Report]
runAndAnalyse bs = for bs $ \idx desc bm -> do
  when (verb == Abridged || verb == Full) $ liftIO $ putStr $ "benchmark " ++ desc
  when (verb == Full) $ liftIO $ putStrLn ""
  (Analysed rpt) <- runAndAnalyseOne idx desc bm
  when (verb == Progress) $ liftIO $ putStr "."
  when (verb == Abridged) $ liftIO $ putStrLn $ "..." ++ (secs $ getRuntime rpt)
  return rpt

-- | Iterate over benchmarks.
-- From Criterion.Internal
for :: MonadIO m => Benchmark
    -> (Int -> String -> Benchmarkable -> m a) -> m [a]
for bs0 handle = snd <$> go (0::Int, []) ("", bs0)
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
          return (idx + 1, drs ++ [x])
      | otherwise = return (idx, drs)
      where desc' = addPrefix pfx desc
    go (!idx,drs) (pfx, BenchGroup desc bs) =
      foldM go (idx,drs) [(addPrefix pfx desc, b) | b <- bs]

    shouldRun pfx mkbench =
      any (select . addPrefix pfx) . benchNames . mkbench $
      error "Criterion.env could not determine the list of your benchmarks since they force the environment (see the documentation for details)"
