{-|
Module      : Crypto.Lol.Benchmarks.SimpleTensorBenches
Description : Benchmarks for the 'Tensor' interface, without benchmark harness.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'Tensor' interface, without benchmark
harness (for performance comparison).
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Benchmarks for the 'Tensor' interface. These benchmarks do not use the
-- benchmark harness, so they may perform differently than TensorBenches.hs.

module Crypto.Lol.Benchmarks.SimpleTensorBenches (simpleTensorBenches1, simpleTensorBenches2) where

import Control.Applicative
import Control.Monad.Random

import Criterion

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random

-- | Benchmarks for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
{-# INLINABLE simpleTensorBenches1 #-}
simpleTensorBenches1 :: forall t m r (gen :: *) . _
  => Proxy '(t,m,r) -> Proxy gen -> IO Benchmark
simpleTensorBenches1 _ _ = do
  x1 :: t m (r, r) <- getRandom
  x2 :: t m r <- getRandom
  x3 :: t m r <- getRandom
  let x2' = mulGPow x2
      x2'' = mulGDec x2
  gen <- newGenIO
  return $ bgroup "STensor" [
    bench "unzipPow"    $ nf unzipT x1,
    bench "unzipDec"    $ nf unzipT x1,
    bench "unzipCRT"    $ nf unzipT x1,
    bench "zipWith (*)" $ nf (zipWithT (*) x2) x3,
    bench "crt"         $ nf (fromJust' "SimpleTensorBenches.crt" crt) x2,
    bench "crtInv"      $ nf (fromJust' "SimpleTensorBenches.crtInv" crtInv) x2,
    bench "l"           $ nf l x2,
    bench "lInv"        $ nf lInv x2,
    bench "*g Pow"      $ nf mulGPow x2,
    bench "*g Dec"      $ nf mulGDec x2,
    bench "*g CRT"      $ nf (fromJust' "SimpleTensorBenches.*gcrt" mulGCRT) x2,
    bench "divg Pow"    $ nf divGPow x2',
    bench "divg Dec"    $ nf divGDec x2'',
    bench "divg CRT"    $ nf (fromJust' "SimpleTensorBenches./gcrt" divGCRT) x2,
    bench "lift"        $ nf (fmapT lift) x2,
    bench "error"       $ nf (evalRand (fmapT (roundMult one) <$>
                           (tGaussianDec
                             (0.1 :: Double) :: Rand (CryptoRand gen) (t m Double)))
                               :: CryptoRand gen -> t m Int64) gen
    ]

-- | Benchmarks for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
{-# INLINABLE simpleTensorBenches2 #-}
simpleTensorBenches2 :: forall t m m' r . _ => Proxy '(t,m',m,r) -> IO Benchmark
simpleTensorBenches2 _ = do
  x2 :: t m r <- getRandom
  x4 :: t m' r <- getRandom
  return $ bgroup "STensor" [
    bench "twacePow" $ nf (twacePowDec :: t m r -> t m' r) x2,
    bench "twaceDec" $ nf (twacePowDec :: t m r -> t m' r) x2,
    bench "twaceCRT" $ nf (fromJust' "SimpleTensorBenches.twaceCRT" twaceCRT :: t m r -> t m' r) x2,
    bench "embedPow" $ nf (embedPow :: t m' r -> t m r) x4,
    bench "embedDec" $ nf (embedDec :: t m' r -> t m r) x4,
    bench "embedCRT" $ nf (fromJust' "SimpleTensorBenches.embedCRT" embedCRT :: t m' r -> t m r) x4
    ]
