-- | Main module for the rlwe-challenges executable.

module Main where

import Control.Applicative
import Options

data MainOpts =
  MainOpts
  { optChallDir :: FilePath -- ^ location of challenges
  }

instance Options MainOpts where
  defineOptions = MainOpts <$>
    simpleOption "challenge-dir" "challenges/" "Path to challenges"

data GenOpts =
  GenOpts
  { optParamsFile   :: FilePath, -- ^ file containing parameters for generation
    optNumInstances :: Int -- ^ number of instances per challenge
  }

instance Options GenOpts where
  defineOptions = GenOpts <$>
    simpleOption "params" "params.txt" "File containing RLWE/R parameters" <*>
    simpleOption "num-instances" 16
    "Number N of instances per challenge, N = 2^k <= 256"

data RevOpts =
  RevOpts
  { -- ??
  }

data VerOpts =
  VerOpts
  { -- ??
  }

main :: IO ()
main = runSubcommand
    [ subcommand "generate" generate
    , subcommand "reveal" reveal
    , subcommand "verify" verify
    ]

generate :: MainOpts -> GenOpts -> [String] -> IO ()
reveal :: MainOpts -> RevOpts -> [String] -> IO ()
verify :: MainOpts -> VerOpts -> [String] -> IO ()

-- hello :: MainOptions -> HelloOpts -> [String] -> IO ()
-- hello mainOpts opts args = unless (optQuiet mainOpts) $ do
--     putStrLn (optHello opts)
