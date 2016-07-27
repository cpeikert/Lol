{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module SimpleUCycBenches (simpleUCycBenches) where

import Control.Applicative
import Control.Monad.Random
import BenchParams

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Criterion

simpleUCycBenches :: IO Benchmark
simpleUCycBenches = do
  x1 :: UCyc T M P (R, R) <- getRandom
  let x1' = toDec x1
  (Right x2) :: UCycPC T M (R, R) <- getRandom
  x3 :: UCycEC T M R <- pcToEC <$> getRandom
  x4 :: UCyc T M P R <- getRandom
  let x5 = toDec x4
  (Right x6) :: UCycPC T M R <- getRandom
  x7 :: UCyc T M' P R <- getRandom
  let x8 = toDec x7
  gen <- newGenIO
  return $ bgroup "SUCyc" [
    bench "unzipPow"    $ nf unzipPow x1,
    bench "unzipDec"    $ nf unzipDec x1',
    bench "unzipCRT"    $ nf unzipCRTC x2,
    bench "zipWith (*)" $ nf (x3*) x3,
    bench "crt"         $ nf toCRT x4,
    bench "crtInv"      $ nf toPow x6,
    bench "l"           $ nf toPow x5,
    bench "lInv"        $ nf toDec x4,
    bench "*g Pow"      $ nf mulG x4,
    bench "*g Dec"      $ nf mulG x5,
    bench "*g CRT"      $ nf mulG x6,
    bench "lift"        $ nf lift x4,
    bench "error"       $ nf (evalRand (errorRounded (0.1 :: Double) :: Rand (CryptoRand HashDRBG) (UCyc T M D Int64))) gen,
    bench "twacePow"    $ nf (twacePow :: UCyc T M P R -> UCyc T M' P R) x4,
    bench "twaceCRT"    $ nf (twaceCRTC :: UCyc T M C R -> UCycPC T M' R) x6,
    bench "embedPow"    $ nf (embedPow :: UCyc T M' P R -> UCyc T M P R) x7,
    bench "embedDec"    $ nf (embedDec :: UCyc T M' D R -> UCyc T M D R) x8
    ]

pcToEC :: UCycPC t m r -> UCycEC t m r
pcToEC (Right x) = (Right x)