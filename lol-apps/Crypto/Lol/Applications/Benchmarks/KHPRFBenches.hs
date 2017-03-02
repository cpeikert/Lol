{-|
Module      : Crypto.Lol.Applications.Benchmarks.KHPRFBenches
Description : Benchmarks for KeyHomomorphicPRF.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for KeyHomomorphicPRF.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Benchmarks.KHPRFBenches (khPRFBenches) where

import Control.Applicative
import Control.Monad.Random hiding (fromList)
import Control.Monad.State hiding (state)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Benchmarks

import MathObj.Matrix hiding (zipWith)

khPRFBenches :: forall rnd t m zq zp gad . (MonadRandom rnd, _)
  => Int -> Proxy t -> Proxy m -> Proxy '(zq,zp,gad) -> (Int -> FullBinTree) -> [rnd Benchmark]
khPRFBenches n _ _ plwe t =
  let pcyc = Proxy::Proxy '(t,m,zq,zp,gad)
  in [
      genBenchArgs "ring-startup" (benchRingPRF n t [0]) pcyc,
      genBenchArgs "ring-amortized" (benchRingPRF n t (grayCode n)) pcyc,
      genBenchArgs "lwe-startup" (benchLatticePRF n 3 t [0]) plwe,
      genBenchArgs "lwe-amortized" (benchLatticePRF n 3 t (grayCode n)) plwe
      ]

-- benchmarks time to run the PRF on each input, including the time
-- it takes to initialize the state with input 0.
benchRingPRF :: forall t m zq (zp :: *) (gad :: *) . (_)
  => Int -> (Int -> FullBinTree) -> [Int] -> Cyc t m zq -> Bench '(t,m,zq,zp,gad)
benchRingPRF size t xs s = benchM $ do
  let gadLen = length $ untag (gadget :: Tagged gad [Cyc t m zq])
  a0 <- fromList 1 gadLen <$> take gadLen <$> getRandoms
  a1 <- fromList 1 gadLen <$> take gadLen <$> getRandoms
  let family = makeFamily a0 a1 (t size) :: PRFFamily gad (Cyc t m zq) (Cyc t m zp)
  return $ bench
    (let st = prfState family Nothing -- initialize with input 0
     in (flip evalState st . mapM (ringPRFM s))) xs

-- benchmarks time to run the PRF on each input, including the time
-- it takes to initialize the state with input 0.
benchLatticePRF :: forall (zp :: *) (zq :: *) (gad :: *) . (_)
  => Int -> Int -> (Int -> FullBinTree) -> [Int] -> Bench '(zq,zp,gad)
benchLatticePRF size n t xs = benchM $ do
  let gadLen = length $ untag (gadget :: Tagged gad [zq])
  a0 :: Matrix zq <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
  a1 :: Matrix zq <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
  s :: Matrix zq <- fromList 1 n <$> take n <$> getRandoms
  let family = makeFamily a0 a1 (t size) :: PRFFamily gad zq zp
  return $ bench
    (let state = prfState family Nothing -- initialize with input 0
     in (flip evalState state . mapM (latticePRFM s))) xs
