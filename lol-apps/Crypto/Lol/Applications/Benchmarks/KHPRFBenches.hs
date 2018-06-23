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

module Crypto.Lol.Applications.Benchmarks.KHPRFBenches
( khprfBenches, khprfBenchesMain ) where

import Crypto.Lol hiding (replicate)
import Crypto.Lol.Types
import Crypto.Lol.Applications.KeyHomomorphicPRF

import Control.Applicative
import Control.Monad.Random hiding (fromList, split)

import Criterion.Main

type N = 2
type Q = 256
type P = 2
type Rq = ZqBasic Q Int64
type Rp = ZqBasic P Int64
type Gad = BaseBGad 2

-- | FBT topologies with 8 leaves each
type LeftTop =         -- left spine
  'Intern ('Intern ('Intern ('Intern ('Intern ('Intern ('Intern
  'Leaf 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf
type RightTop =        -- right spine
  'Intern 'Leaf ('Intern 'Leaf ('Intern 'Leaf ('Intern 'Leaf (
  'Intern 'Leaf ('Intern 'Leaf ('Intern 'Leaf 'Leaf))))))
type CompleteTop =     -- complete and balanced
  'Intern ('Intern ('Intern 'Leaf 'Leaf) ('Intern 'Leaf 'Leaf))
          ('Intern ('Intern 'Leaf 'Leaf) ('Intern 'Leaf 'Leaf))

khprfBenchesMain :: IO ()
khprfBenchesMain = do
  x <- khprfBenches
  defaultMain [x]

khprfBenches :: MonadRandom rnd => rnd Benchmark
khprfBenches = do
  key <- genKey
  params :: PRFParams N Gad Rq <- genParams
  let pl = singFBT :: Sing LeftTop
  let pc = singFBT :: Sing CompleteTop
  let pr = singFBT :: Sing RightTop
  let x  = replicate False
  return $ bgroup "KHPRF Benchmarks"
    [ bench "left-solo"          $ prfBench          pl params key x
    , bench "complete-solo"      $ prfBench          pc params key x
    , bench "right-solo"         $ prfBench          pr params key x
    , bench "left-amortized"     $ prfAmortizedBench pl params key values
    , bench "complete-amortized" $ prfAmortizedBench pc params key values
    , bench "right-amortized"    $ prfAmortizedBench pr params key values
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
