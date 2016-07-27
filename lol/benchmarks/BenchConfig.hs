{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module BenchConfig where

import Crypto.Lol hiding ((*))
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Language.Haskell.TH

-- The random generator used in benchmarks
type Gen = HashDRBG

type Zq (q :: k) = ZqBasic q Int64

-- table parameters
colWidth, testNameWidth :: Int
colWidth = 15      -- width for numeric columns
testNameWidth = 40 -- width for benchmark name

-- Values this much larger than the minimum benchmark time are printed in red.
-- Useful when multiple layers are being benchmarked against each other.
redThreshold :: Double
redThreshold = 1.1

-- verbosity of progress
--   "Progress" just prints a '.' for each benchmark as it completes.
--   "Abridged" prints a one-line summary for each benchmark as it completes.
--   "Full" prints the full criterion for each benchmark as it completes.
data Verb = Progress | Abridged | Full deriving (Eq)
verb :: Verb
verb = Abridged

-- choose which layers of Lol to benchmark
layers :: [String]
layers = [
  "STensor",
  "Tensor",
  "SUCyc",
  "UCyc",
  "Cyc"
  ]

-- list of tensors to benchmark
tensors :: [TypeQ]
tensors = [ [t| CT |] ]

-- list of m/r combos to benchmark
mrs :: [Q Type]
mrs = [
  [t| '(F9*F5*F7*F11, Zq 34651) |]
  --[t| '(F64*F9*F25, Zq 14401) |]
  ]

-- list m/m'/r combos to benchmark, where m | m'
mm'rs :: [Q Type]
mm'rs = [
  [t| '(F3*F5*F11, F9*F5*F7*F11, Zq 34651) |]
  ]

-- choose which functions to benchmark
benches :: [String]
benches = [
  -- benchmarked with "params1" parameters
  "unzipPow",
  "unzipDec",
  "unzipCRT",
  "zipWith (*)",
  "crt",
  "crtInv",
  "l",
  "lInv",
  "*g Pow",
  "*g CRT",
  "lift",
  "error",

  -- benchmarked with "params2" parameters
  "twacePow",
  "twaceCRT"{-,
  "embedPow",
  "embedDec"-}

  ]

params1, params2 :: [TypeQ]
params1 = appT <$> appT (conT '(,)) <$> tensors <*> mrs
params2 = appT <$> appT (conT '(,)) <$> tensors <*> mm'rs
{-
type Tensors = '[CT,RT]
type MRCombos =
  '[ '(F1024, Zq 1051649),      -- 1024 / 512
     '(F2048, Zq 1054721),      -- 2048 / 1024
     '(F64 * F27, Zq 1048897),  -- 1728 / 576
     '(F64 * F81, Zq 1073089),  -- 5184 / 1728
     '(F64*F9*F25, Zq 1065601)  -- 14400 / 3840
    ]


type MM'RCombos =
  '[ '(F8 * F91, F8 * F91 * F4, Zq 8737),
     '(F8 * F91, F8 * F91 * F5, Zq 14561),
     '(F128, F128 * F91, Zq 23297)
    ]
-}