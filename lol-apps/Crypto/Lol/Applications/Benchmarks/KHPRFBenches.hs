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
{-# LANGUAGE LiberalTypeSynonyms   #-}

module Crypto.Lol.Applications.Benchmarks.KHPRFBenches (
  khprfLeftBenches
, khprfRightBenches
, khprfCompleteBenches
) where

import Crypto.Lol hiding (replicate, head)
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Benchmarks

import Control.Monad.Random hiding (fromList, split)
import Control.Monad.State

-- | Are there kind synonyms in Haskell??

-- | Trees with 8 leaves
type leftFBT =         -- left spine
    FBT ('Intern ('Intern ('Intern ('Intern ('Intern ('Intern ('Intern
         'Leaf 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf) 'Leaf)
type rightFBT =        -- right spine
    FBT ('Intern 'Leaf $ 'Intern 'Leaf $ 'Intern 'Leaf $ 'Intern 'Leaf $
         'Intern 'Leaf $ 'Intern 'Leaf $ 'Intern 'Leaf 'Leaf)
type completeFBT =     -- complete and balanced
    FBT ('Intern ('Intern ('Intern 'Leaf 'Leaf) ('Intern 'Leaf 'Leaf))
                 ('Intern ('Intern 'Leaf 'Leaf) ('Intern 'Leaf 'Leaf)))


khprfBenches :: forall t gad rnd zq zp m . (MonadRandom rnd)
             => Proxy t -> Proxy '(zq,zp,gad)
             -> [rnd Benchmark]
khprfBenches pt pz =
  [
    genBenchArgs "khprf-startup"   <something> pz
    genBenchArgs "khprf-amortized" <something> pz
  ]

-- `zq / zp` vs. `rq / rp`?

khprfLeftBenches :: forall gad rnd zq zp n t .
                   (MonadRandom rnd, FBT t ~ leftFBT)
                 => Proxy t -> Proxy '(zq,zp,gad)
                 -> [rnd Benchmark]
khprfLeftBenches = khprfBranches

khprfRightBenches :: forall gad rnd zq zp n t .
                    (MonadRandom rnd, FBT t ~ rightFBT)
                  => Proxy t -> Proxy '(zq,zp,gad)
                  -> [rnd Benchmark]
khprfRightBenches = khprfBranches

khprfCompleteBenches :: forall gad rnd zq zp n t .
                       (MonadRandom rnd, FBT t ~ completeFBT)
                     => Proxy t -> Proxy '(zq,zp,gad)
                     -> [rnd Benchmark]
khprfCompleteBenches = khprfBranches
