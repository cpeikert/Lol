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

type N = 16
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
  let pl = sing :: Sing LeftTop
  let pc = sing :: Sing CompleteTop
  let pr = sing :: Sing RightTop
  return $ bgroup "KHPRF Benchmarks"
    [ bench "left-startup"       $ prfPureBench pl params key [replicate False]
    , bench "left-amortized"     $ prfPureBench pl params key values
    , bench "complete-startup"   $ prfPureBench pc params key [replicate False]
    , bench "complete-amortized" $ prfPureBench pc params key values
    , bench "right-startup"      $ prfPureBench pr params key [replicate False]
    , bench "right-amortized"    $ prfPureBench pr params key values ]

prfPureBench :: (TopC t, Rescale rq Rp, Decompose gad rq)
  => STop t -> PRFParams n gad rq -> PRFKey n rq
  -> [BitString (SizeTop t)] -> Benchmarkable
prfPureBench s params key xs = nf (run s params :: _ -> [Matrix Rp])
                                  (sequence $ prf key <$> xs)


-- CONSIDER benchmarking both Gray code enumeration and `normal`
-- enumeration for comparison purposes.
