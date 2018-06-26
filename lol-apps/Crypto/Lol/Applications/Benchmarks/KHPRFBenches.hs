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

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (fromList, split)

import Crypto.Lol hiding (replicate)
import Crypto.Lol.Types
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Reflects

import Criterion.Main

type M = F64
type N = 1
type Q = 256
type P = 2
type Zq q = ZqBasic q Int64
type Rq t = Cyc t M (Zq Q)
type Rp t = Cyc t M (Zq P)
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

main :: forall t . (_) => Proxy t -> IO ()
main pt = do
  x <- khprfBenches pt
  defaultMain [x]

khprfBenches :: forall ts rnd .
    (MonadRandom rnd, Random (Rq ts), Rescale (Rq ts) (Rp ts),
     Decompose Gad (Rq ts), Reflects N Int, Gadget Gad (Rq ts), NFData (Rp ts))
  => Proxy ts -> rnd Benchmark
khprfBenches pts = do
  key :: PRFKey N (Rq ts) <- genKey
  params :: PRFParams N Gad (Rq ts) <- genParams
  let sc  = singFBT :: Sing Complete5
      sl  = singFBT :: Sing (Left  P32)
      sr  = singFBT :: Sing (Right P32)
      xs  = take 32 values
      x   = head xs
      prp = Proxy :: Proxy (Rp ts)
  return $ bgroup "KHPRF Benchmarks"
    [ bench "complete-solo"      $ prfBench          pts prp sc params key x
    , bench "left-solo"          $ prfBench          pts prp sl params key x
    , bench "right-solo"         $ prfBench          pts prp sr params key x
    , bench "complete-amortized" $ prfAmortizedBench pts prp sc params key xs
    , bench "left-amortized"     $ prfAmortizedBench pts prp sl params key xs
    , bench "right-amortized"    $ prfAmortizedBench pts prp sr params key xs
    ]

prfBench :: forall ts t n gad rq rp .
    (Rescale rq rp, Decompose gad rq, FBTC t, NFData rp)
  => Proxy ts -> Proxy rp
  -> SFBT t -> PRFParams n gad rq -> PRFKey n rq -> BitString (SizeFBT t)
  -> Benchmarkable
prfBench _ _ t p s x = nf (prf t p s :: _ -> Matrix rp) x

prfAmortizedBench :: forall ts t n gad rq rp .
    (Rescale rq rp, Decompose gad rq, NFData rp)
  => Proxy ts -> Proxy rp
  -> SFBT t -> PRFParams n gad rq -> PRFKey n rq -> [BitString (SizeFBT t)]
  -> Benchmarkable
prfAmortizedBench _ _ t p s xs =
  nf (run :: _ -> [Matrix rp]) (sequence $ prfAmortized t p s <$> xs)
