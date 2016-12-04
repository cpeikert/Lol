{-# LANGUAGE RecordWildCards #-}

-- | Main module for the rlwe-challenges executable.

module RLWEChallengesMain where

import Control.Monad         (when)
import Data.Time.Clock.POSIX
import Options

import System.Console.ANSI
import System.Exit
import System.IO

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
    simpleOption "challenge-dir" "challenges/" "Path to challenges"

data GenOpts =
  GenOpts
  { optParamsFile      :: FilePath, -- ^ file with parameters for generation
    optNumInstances    :: InstanceID, -- ^ number of instances per challenge
    optInitBeaconEpoch :: BeaconEpoch -- ^ initial beacon epoch for suppress phase
  }

instance Options GenOpts where
  defineOptions = GenOpts <$>
    simpleOption "params" "params.txt" "File containing RLWE/R parameters" <*>
    simpleOption "num-instances" 32
    "Number N of instances per challenge, N = 2^k <= 256" <*>
    defineOption optionType_int64 (\o ->
      o {optionLongFlags = ["init-beacon"],
         optionDescription = "Initial beacon epoch for suppress phase."})

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
  let initBeaconTime = beaconFloor optInitBeaconEpoch
      initBeacon = BA initBeaconTime 0
  when (initBeaconTime == 0) $ do
    putStrLn "You must specify the initial beacon time with --init-beacon"
    exitFailure
  currTime <- round <$> getPOSIXTime
  if initBeaconTime > currTime
    then putStrLn $ "Challenges can be revealed starting at " ++
         show initBeaconTime ++ ", " ++ show (initBeaconTime-currTime) ++
         " seconds from now."
    else printANSI Yellow "WARNING: Reveal time is in the past!"
  paramContents <- readFile optParamsFile
  let params = parseChallParams paramContents optNumInstances
  generateMain optChallDir initBeacon params

suppress :: MainOpts -> NullOpts -> [String] -> IO ()
suppress MainOpts{..} _ _ = suppressMain optChallDir

verify :: MainOpts -> NullOpts -> [String] -> IO ()
verify MainOpts{..} _ _ = verifyMain optChallDir
