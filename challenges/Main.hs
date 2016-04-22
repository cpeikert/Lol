-- | Main module for the rlwe-challenges executable.

module Main where

import Data.Int
import Data.Time.Clock.POSIX
import Options
import System.IO.Unsafe

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
    optInitBeaconEpoch :: Int64 -- ^ initial beacon epoch for reveal phase
  }

instance Options GenOpts where
  defineOptions = GenOpts <$>
    simpleOption "params" "params.txt" "File containing RLWE/R parameters" <*>
    simpleOption "num-instances" 16
    "Number N of instances per challenge, N = 2^k <= 256" <*>
    simpleOption "init-beacon"
    -- CJP: sneaky! not referentially transparent, but handy as a default
    (round (unsafePerformIO $ daysFromNow 3))
    "Initial beacon epoch for reveal phase (default is 3 days from now)"

daysFromNow :: Int -> IO POSIXTime
daysFromNow n = (posixDayLength * fromIntegral n + ) <$> getPOSIXTime

data NullOpts = NullOpts

instance Options NullOpts where
  defineOptions = pure NullOpts

main :: IO ()
main = runSubcommand
    [ subcommand "generate" generate
    , subcommand "reveal" reveal
    , subcommand "verify" verify
    ]

generate :: MainOpts -> GenOpts -> [String] -> IO ()
generate mopts gopts _ = print $ optInitBeaconEpoch gopts

reveal :: MainOpts -> NullOpts -> [String] -> IO ()
reveal = error "TODO"

verify :: MainOpts -> NullOpts -> [String] -> IO ()
verify = error "TODO"

-- hello :: MainOptions -> HelloOpts -> [String] -> IO ()
-- hello mainOpts opts args = unless (optQuiet mainOpts) $ do
--     putStrLn (optHello opts)
