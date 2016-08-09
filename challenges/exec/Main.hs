{-# LANGUAGE RecordWildCards #-}

-- | Main module for the rlwe-challenges executable.

module Main where

import Control.Monad
import Data.Time.Clock.POSIX
import Options

import System.Console.ANSI
import System.Directory
import System.IO
import System.IO.Unsafe

import Beacon
import Common   (InstanceID, printANSI)
import Generate
import Params
import Suppress
import Verify

data MainOpts =
  MainOpts
  { optChallDir :: FilePath -- ^ location of challenges
  }

instance Options MainOpts where
  defineOptions = MainOpts <$>
    simpleOption "challenge-dir" "rlwe-challenges/" "Path to challenges"

data GenOpts =
  GenOpts
  { optParamsFile      :: FilePath, -- ^ file with parameters for generation
    optNumInstances    :: InstanceID, -- ^ number of instances per challenge
    optInitBeaconEpoch :: BeaconEpoch -- ^ initial beacon epoch for suppress phase
  }

instance Options GenOpts where
  defineOptions = GenOpts <$>
    simpleOption "params" "params.txt" "File containing RLWE/R parameters" <*>
    simpleOption "num-instances" 16
    "Number N of instances per challenge, N = 2^k <= 256" <*>
    simpleOption "init-beacon"
    -- CJP: sneaky! not referentially transparent, but handy as a default
    (unsafePerformIO $ daysFromNow 3)
    "Initial beacon epoch for suppress phase (default is 3 days from now)."

-- | Epoch that's @n@ days from now, rounded to a multiple of 60 for
-- NIST beacon purposes.
daysFromNow :: Int -> IO BeaconEpoch
daysFromNow n = do
  t <- round <$> getPOSIXTime
  return $ 86400 * fromIntegral n + t

data NullOpts = NullOpts

instance Options NullOpts where
  defineOptions = pure NullOpts

main :: IO ()
main = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering
  runSubcommand
    [ subcommand "generate" generate
    , subcommand "suppress" suppress
    , subcommand "verify" verify
    ]

generate :: MainOpts -> GenOpts -> [String] -> IO ()
generate MainOpts{..} GenOpts{..} _ = do
  challDirExists <- doesDirectoryExist optChallDir
  when challDirExists $
    error $ "The output directory " ++ optChallDir ++
      " already exists. Delete it or choose a new destination."
  let initBeaconTime = beaconFloor optInitBeaconEpoch
      initBeacon = BA initBeaconTime 0
  currTime <- round <$> getPOSIXTime
  case initBeaconTime > currTime of
    True -> putStrLn $ "Challenges can be revealed starting at " ++
            show initBeaconTime ++ ", " ++ show (initBeaconTime-currTime) ++
            " seconds from now."
    False -> printANSI Yellow "WARNING: Reveal time is in the past!"
  paramContents <- readFile optParamsFile
  let params = parseChallParams paramContents optNumInstances
  generateMain optChallDir initBeacon params

suppress :: MainOpts -> NullOpts -> [String] -> IO ()
suppress MainOpts{..} _ _ = suppressMain optChallDir

verify :: MainOpts -> NullOpts -> [String] -> IO ()
verify MainOpts{..} _ _ = verifyMain optChallDir
