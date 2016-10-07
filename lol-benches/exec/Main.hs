{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import Crypto.Lol
import Crypto.Lol.Benchmarks
import Crypto.Lol.Benchmarks.SimpleTensorBenches
import Crypto.Lol.Benchmarks.TensorBenches
import Crypto.Lol.Benchmarks.SimpleUCycBenches
import Crypto.Lol.Benchmarks.UCycBenches
import Crypto.Lol.Benchmarks.CycBenches
import Crypto.Lol.Types
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.PrettyPrint hiding (benches, layers)

import Crypto.Random.DRBG

import Control.Monad (liftM2)


-- choose which layers of Lol to benchmark
layers :: [String]
layers = [
  {-"STensor",
  "Tensor",
  "SUCyc",-}
  "UCyc",
  "Cyc"
  ]

benches :: [String]
benches = [
  "unzipPow",
  "unzipDec",{-
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
  "error",-}
  "twacePow",
  "twaceDec"{-,
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"-}
  ]

type Zq (q :: k) = ZqBasic q Int64

instance Show (ArgType HashDRBG) where
  show _ = "HashDRBG"

main :: IO ()
main = do
  let opts = defaultWidthOpts Progress layers benches
  benchGroups <- concat <$> sequence [defaultBenches (Proxy::Proxy CT),
                                      defaultBenches (Proxy::Proxy RT)]
  mapM_ (prettyBenches opts) benchGroups

{-# INLINABLE defaultBenches #-}
defaultBenches :: _ => Proxy t -> IO [Benchmark]
defaultBenches pt = liftM2 (++)
  (mapM (($ (Proxy::Proxy HashDRBG)) . ($ pt)) [
    oneIdxBenches (Proxy::Proxy '(F64*F9*F25,   Zq 1065601)),
    oneIdxBenches (Proxy::Proxy '(F9*F5*F7*F11, Zq 34651)),
    oneIdxBenches (Proxy::Proxy '(F1024,        Zq 12289)),
    oneIdxBenches (Proxy::Proxy '(F2048,        Zq 12289)),
    oneIdxBenches (Proxy::Proxy '(F64*F27,      Zq 3457)),
    oneIdxBenches (Proxy::Proxy '(F64*F81,      Zq 10369)),
    oneIdxBenches (Proxy::Proxy '(F64*F9*F25,   Zq 14401))
    ])
  (mapM ($ pt) [
    twoIdxBenches (Proxy::Proxy '(F8*F7*F13,  F32*F7*F13,   Zq 8737)),
    twoIdxBenches (Proxy::Proxy '(F8*F7*F13,  F8*F5*F7*F13, Zq 145561)),
    twoIdxBenches (Proxy::Proxy '(F128,       F128*F7*F13,  Zq 23297))
    ])

{-# INLINABLE oneIdxBenches #-}
oneIdxBenches :: forall t m r gen . _ => Proxy '(m,r) -> Proxy t -> Proxy gen -> IO Benchmark
oneIdxBenches _ _ pgen =
  let ptmr = Proxy :: Proxy '(t,m,r)
  in benchGroup (showType ptmr) $ (($ pgen) . ($ ptmr)) <$> [
      simpleTensorBenches1,
      tensorBenches1,
      simpleUCycBenches1,
      ucycBenches1,
      cycBenches1
      ]

{-# INLINABLE twoIdxBenches #-}
twoIdxBenches :: forall t m m' r . _ => Proxy '(m,m',r) -> Proxy t -> IO Benchmark
twoIdxBenches _ _ =
  let ptmr = Proxy :: Proxy '(t,m,m',r)
  in benchGroup (showType ptmr) $ ($ ptmr) <$> [
      simpleTensorBenches2,
      tensorBenches2,
      simpleUCycBenches2,
      ucycBenches2,
      cycBenches2
      ]
