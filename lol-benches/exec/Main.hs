{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

import Crypto.Lol.Benchmarks.Standard
import Crypto.Lol.Factored
import Crypto.Lol.Types
import Crypto.Lol.Utils.PrettyPrint hiding (benches, layers)
import Crypto.Random.DRBG

import Data.Proxy

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

main :: IO ()
main = do
  let opts = defaultWidthOpts Progress layers benches
  b1 <- oneIdxBenches (Proxy::Proxy '(F64*F9*F25,   Zq 14401)) (Proxy::Proxy CT) (Proxy::Proxy HashDRBG)
  --b2 <- oneIdxBenches (Proxy::Proxy '(F9*F5*F7*F11, Zq 34651)) (Proxy::Proxy CT) (Proxy::Proxy HashDRBG)
  --g1 <- defaultBenches (Proxy::Proxy CT)
  --g2 <- defaultBenches (Proxy::Proxy RT)
  let benchGroups = [b1] -- :b2:g1
  mapM_ (prettyBenches opts) benchGroups
