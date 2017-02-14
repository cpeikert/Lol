{-|
Module      : Crypto.Lol.Benchmarks.SimpleUCycBenches
Description : Benchmarks for the 'UCyc' interface, without benchmark harness.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'UCyc' interface, without benchmark
harness (for performance comparison).
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.SimpleUCycBenches (simpleUCycBenches1, simpleUCycBenches2) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types
import Crypto.Random

import Criterion

-- | Benchmarks for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
{-# INLINE simpleUCycBenches1 #-}
simpleUCycBenches1 :: forall t m r (gen :: *) . _
  => Proxy '(t,m,r) -> Proxy gen -> IO Benchmark
simpleUCycBenches1 _ _ = do
  x1 :: UCyc t m P (r, r) <- getRandom
  let x1' = toDec x1
  (Right x2) :: UCycPC t m (r, r) <- getRandom
  x3 :: UCycEC t m r <- pcToEC <$> getRandom
  x4 :: UCyc t m P r <- getRandom
  let x5 = toDec x4
  (Right x6) :: UCycPC t m r <- getRandom
  let x4' = mulG x4
      x5' = mulG x5
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
    bench "divg Pow"    $ nf divGPow x4',
    bench "divg Dec"    $ nf divGDec x5',
    bench "divg CRT"    $ nf divGCRTC x6,
    bench "lift"        $ nf lift x4,
    bench "error"       $ nf (evalRand (errorRounded (0.1 :: Double) :: Rand (CryptoRand gen) (UCyc t m D Int64))) gen
    ]

-- | Benchmarks for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
{-# INLINE simpleUCycBenches2 #-}
simpleUCycBenches2 :: forall t m m' r . _ => Proxy '(t,m,m',r) -> IO Benchmark
simpleUCycBenches2 _ = do
  x4 :: UCyc t m' P r <- getRandom
  let x5 = toDec x4
  (Right x6) :: UCycPC t m' r <- getRandom
  x7 :: UCyc t m P r <- getRandom
  let x8 = toDec x7
  (Right x9) :: UCycPC t m r <- getRandom
  return $ bgroup "SUCyc" [
    bench "twacePow" $ nf (twacePow :: UCyc t m' P r -> UCyc t m P r) x4,
    bench "twaceDec" $ nf (twaceDec :: UCyc t m' D r -> UCyc t m D r) x5,
    bench "twaceCRT" $ nf (twaceCRTC :: UCyc t m' C r -> UCycPC t m r) x6,
    bench "embedPow" $ nf (embedPow :: UCyc t m P r -> UCyc t m' P r) x7,
    bench "embedDec" $ nf (embedDec :: UCyc t m D r -> UCyc t m' D r) x8,
    bench "embedCRT" $ nf (embedCRTC :: UCyc t m C r -> UCycPC t m' r) x9
    ]

pcToEC :: UCycPC t m r -> UCycEC t m r
pcToEC (Right x) = (Right x)
