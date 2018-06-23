{-|
Module      : Crypto.Lol.Applications.Benchmarks.KHPRFBenches
Description : Benchmarks for KeyHomomorphicPRF.
Copyright   : (c) Bogdan Manga, 2018
                  Chris Peikert, 2018
License     : GPL-3
Maintainer  : bmanga@umich.edu
Stability   : experimental
Portability : POSIX

Benchmarks for KeyHomomorphicPRF.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Applications.Benchmarks.KHPRFBenches
( khprfBenches, main ) where

import Crypto.Lol hiding (replicate)
import Crypto.Lol.Types
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.Tensor.CPP

import Control.Applicative
import Control.Monad.Random hiding (fromList, split)

import Criterion.Main

type M = F64
type N = 1
type Q = 256
type P = 2
type Rq = Cyc CT M (ZqBasic Q Int64)
type Rp = Cyc CT M (ZqBasic P Int64)
type Gad = BaseBGad 2

-- | left-spine tree with the given number of leaves
type family Left n where
  Left 'O       = 'Leaf
  Left ('S n')  = 'Intern (Left n') 'Leaf

-- | right-spine tree with the given number of leaves
type family Right n where
  Right 'O      = 'Leaf
  Right ('S n') = 'Intern 'Leaf (Right n')

type Complete0 = 'Leaf
type Complete1 = 'Intern Complete0 Complete0
type Complete2 = 'Intern Complete1 Complete1
type Complete3 = 'Intern Complete2 Complete2
type Complete4 = 'Intern Complete3 Complete3
type Complete5 = 'Intern Complete4 Complete4

main :: IO ()
main = do
  x <- khprfBenches
  defaultMain [x]

khprfBenches :: MonadRandom rnd => rnd Benchmark
khprfBenches = do
  key <- genKey
  params :: PRFParams N Gad Rq <- genParams
  let sc = singFBT :: Sing Complete5
      sl = singFBT :: Sing (Left  P32)
      sr = singFBT :: Sing (Right P32)
      xs = take 32 values
      x  = head xs
  return $ bgroup "KHPRF Benchmarks"
    [ bench "complete-solo"      $ prfBench          sc params key x
    , bench "left-solo"          $ prfBench          sl params key x
    , bench "right-solo"         $ prfBench          sr params key x
    , bench "complete-amortized" $ prfAmortizedBench sc params key xs
    , bench "left-amortized"     $ prfAmortizedBench sl params key xs
    , bench "right-amortized"    $ prfAmortizedBench sr params key xs
    ]

prfBench :: (Rescale rq Rp, Decompose gad rq, FBTC t)
  => SFBT t -> PRFParams n gad rq -> PRFKey n rq -> BitString (SizeFBT t)
  -> Benchmarkable
prfBench t p s x = nf (prf t p s :: _ -> Matrix Rp) x

prfAmortizedBench :: (Rescale rq Rp, Decompose gad rq)
  => SFBT t -> PRFParams n gad rq -> PRFKey n rq -> [BitString (SizeFBT t)]
  -> Benchmarkable
prfAmortizedBench t p s xs =
  nf (run :: _ -> [Matrix Rp]) (sequence $ prfAmortized t p s <$> xs)
