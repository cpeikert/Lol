{-# LANGUAGE RecordWildCards #-}

-- | Main module for the rlwe-challenges executable.

module Main where

import Data.Time.Clock.POSIX
import Options
import System.IO
import System.IO.Unsafe

import Beacon
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
    optNumInstances    :: Int, -- ^ number of instances per challenge
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
    "Initial beacon epoch for suppress phase (default is 3 days from now)"

-- | Epoch that's @n@ days from now, rounded to a multiple of 60 for
-- NIST beacon purposes.
daysFromNow :: Int -> IO BeaconEpoch
daysFromNow n = do
  t <- round <$> getPOSIXTime
  let d = 86400 * fromIntegral n + t
  return $ lastBeaconBefore d

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
  let initBeacon = (BA optInitBeaconEpoch 0)
  paramContents <- readFile $ optChallDir </> optParamsFile
  let params = parseChallParams paramContents
  generateMain optChallDir initBeacon params

{-
generateMain optChallDir  [
    Cont 10 16 128 257 1.0 (1/(2^40)),
    RLWR 10 16 128 256 32,
    Cont 10 16 128 257 0.5 (1/(2^40)),
    RLWR 10 16 128 256 128]
    -}



suppress :: MainOpts -> NullOpts -> [String] -> IO ()
suppress = error "TODO"

verify :: MainOpts -> NullOpts -> [String] -> IO ()
verify = error "TODO"
