{-
import TensorBenches
import Criterion.Main

main :: IO ()
main = defaultMain =<< sequence [
  tensorBenches
  ]
-}
{-# LANGUAGE BangPatterns, RecordWildCards #-}

import CycBenches
import SimpleTensorBenches
import TensorBenches
import SimpleUCycBenches
import UCycBenches

import Criterion.Internal (runAndAnalyseOne)
import Criterion.Main.Options (defaultConfig)
import Criterion.Measurement (secs)
import Criterion.Monad (Criterion, withConfig)
import Criterion.Types
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Exception (evaluate)

import Control.DeepSeq (rnf)

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

benches :: [String]
benches = [
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

data Verb = Progress | Abridged | Full deriving (Eq)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- for better printing of progress
  reports <- mapM (getReports =<<) [
    simpleTensorBenches,
    tensorBenches,
    simpleUCycBenches,
    ucycBenches,
    cycBenches
    ]
  when (verb == Progress) $ putStrLn ""
  printTable $ map reverse reports

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
        if t > (1.1*minTime)
        then do
          setSGR [SetColor Foreground Vivid Red]
          printf col $ secs t
          setSGR [Reset]
        else printf col $ secs t
  forM_ times printCol
  putStrLn ""

stripOuterGroup :: String -> String
stripOuterGroup = tail . dropWhile (/= '/')

getReports :: Benchmark -> IO [Report]
getReports = withConfig config . runAndAnalyse

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
    select = flip elem benches . takeWhile (/= '/') . stripOuterGroup
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
