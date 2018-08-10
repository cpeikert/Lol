{-|
Module      : Crypto.Lol.Benchmarks.SimpleCycRepBenches
Description : Benchmarks for the 'CycRep' interface, without benchmark harness.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for the 'CycRep' interface, without benchmark
harness (for performance comparison).
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.SimpleCycRepBenches
( simpleCycRepBenches1, simpleCycRepBenches2
) where

import Control.Applicative
import Control.Monad.Random hiding (lift)

import Crypto.Lol.Cyclotomic.CycRep
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random

import Criterion

-- | Benchmarks for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
{-# INLINE simpleCycRepBenches1 #-}
simpleCycRepBenches1 :: forall t m r (gen :: *) . _
  => Proxy '(t,m,r) -> Proxy gen -> IO Benchmark
simpleCycRepBenches1 _ _ = do
--  x1 :: CycRep t P m (r, r) <- getRandom
--  let x1' = toDec x1
--  (Right x2) :: CycRepPC t m (r, r) <- getRandom
  x3 :: CycRepEC t m r <- pcToEC <$> getRandom
  x4 :: CycRep t P m r <- getRandom
  let x5 = toDec x4
  (Right x6) :: CycRepPC t m r <- getRandom
  let x4' = mulGPow x4
      x5' = mulGDec x5
  gen <- newGenIO
  return $ bgroup "SCycRep" [
--    bench "unzipPow"    $ nf unzipPow x1,
--    bench "unzipDec"    $ nf unzipDec x1',
--    bench "unzipCRT"    $ nf unzipCRTC x2,
    bench "zipWith (*)" $ nf (x3*) x3,
    bench "crt"         $ nf toCRT x4,
    bench "crtInv"      $ nf toPow x6,
    bench "l"           $ nf toPow x5,
    bench "lInv"        $ nf toDec x4,
    bench "*g Pow"      $ nf mulGPow x4,
    bench "*g Dec"      $ nf mulGDec x5,
    bench "*g CRT"      $ nf mulGCRTC x6,
    bench "divg Pow"    $ nf divGPow x4',
    bench "divg Dec"    $ nf divGDec x5',
    bench "divg CRT"    $ nf divGCRTC x6,
    bench "lift"        $ nf lift x4,
    bench "error"       $ nf (evalRand (roundedGaussian (0.1 :: Double) :: Rand (CryptoRand gen) (CycRep t D m Int64))) gen
    ]

-- | Benchmarks for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
{-# INLINE simpleCycRepBenches2 #-}
simpleCycRepBenches2 :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> IO Benchmark
simpleCycRepBenches2 _ = do
  x4 :: CycRep t P m' r <- getRandom
  let x5 = toDec x4
  (Right x6) :: CycRepPC t m' r <- getRandom
  x7 :: CycRep t P m r <- getRandom
--  let x8 = toDec x7
  (Right x9) :: CycRepPC t m r <- getRandom
  return $ bgroup "SCycRep" [
    bench "twacePow" $ nf (twacePow  :: CycRep t P m' r -> CycRep t P m  r) x4,
    bench "twaceDec" $ nf (twaceDec  :: CycRep t D m' r -> CycRep t D m  r) x5,
    bench "twaceCRT" $ nf (twaceCRTC :: CycRep t C m' r -> CycRepPC t m  r) x6,
    bench "embedPow" $ nf (embedPow  :: CycRep t P m  r -> CycRep t P m' r) x7,
--    bench "embedDec" $ nf (embedDec  :: CycRep t D m  r -> CycRep t D m' r) x8,
    bench "embedCRT" $ nf (embedCRTC :: CycRep t C m  r -> CycRepPC t m' r) x9
    ]

pcToEC :: CycRepPC t m r -> CycRepEC t m r
pcToEC (Right x) = Right x
