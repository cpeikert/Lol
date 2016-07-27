{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module CycBenches (cycBenches1, cycBenches2) where

import Apply.Cyc
import Benchmarks
import BenchConfig
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG
{-# INLINE cycBenches1 #-}
cycBenches1 param = benchGroup "Cyc" [
  benchGroup "unzipPow"    $ [hideArgs bench_unzipCycPow param],
  benchGroup "unzipDec"    $ [hideArgs bench_unzipCycDec param],
  benchGroup "unzipCRT"    $ [hideArgs bench_unzipCycCRT param],
  benchGroup "zipWith (*)" $ [hideArgs bench_mul param],
  benchGroup "crt"         $ [hideArgs bench_crt param],
  benchGroup "crtInv"      $ [hideArgs bench_crtInv param],
  benchGroup "l"           $ [hideArgs bench_l param],
  benchGroup "lInv"        $ [hideArgs bench_lInv param],
  benchGroup "*g Pow"      $ [hideArgs bench_mulgPow param],
  benchGroup "*g CRT"      $ [hideArgs bench_mulgCRT param],
  benchGroup "lift"        $ [hideArgs bench_liftPow param],
  benchGroup "error"       $ [hideArgs (bench_errRounded 0.1) param]
  ]
{-# INLINE cycBenches2 #-}
cycBenches2 param = benchGroup "Cyc" [
  benchGroup "twacePow"    $ [hideArgs bench_twacePow param],
  benchGroup "twaceCRT"    $ [hideArgs bench_twaceCRT param],
  benchGroup "embedPow"    $ [hideArgs bench_embedPow param],
  benchGroup "embedDec"    $ [hideArgs bench_embedDec param]
  ]

bench_unzipCycPow :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycPow a =
  let a' = advisePow a
  in bench unzipCyc a'

bench_unzipCycDec :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycDec a =
  let a' = adviseDec a
  in bench unzipCyc a'

bench_unzipCycCRT :: (UnzipCtx t m r) => Cyc t m (r,r) -> Bench '(t,m,r)
bench_unzipCycCRT a =
  let a' = adviseCRT a
  in bench unzipCyc a'

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = adviseCRT a
      b' = adviseCRT b
  in bench (a' *) b'

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_crt x = let y = advisePow x in bench adviseCRT y

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_crtInv x = let y = adviseCRT x in bench advisePow y

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_l x = let y = adviseDec x in bench advisePow y

-- convert input from Pow basis to Dec basis
bench_lInv :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_lInv x = let y = advisePow x in bench adviseDec y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_liftPow x = let y = advisePow x in bench (liftCyc Pow) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgPow x = let y = advisePow x in bench mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgCRT x = let y = adviseCRT x in bench mulG y

-- generate a rounded error term
bench_errRounded :: forall t m r . (ErrorCtx t m r Gen)
  => Double -> Bench '(t,m,r)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand Gen) (Cyc t m (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twacePow x =
  let y = advisePow x
  in bench (twace :: Cyc t m' r -> Cyc t m r) y

bench_twaceCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m' r -> Bench '(t,m,m',r)
bench_twaceCRT x =
  let y = adviseCRT x
  in bench (twace :: Cyc t m' r -> Cyc t m r) y

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedPow x =
  let y = advisePow x
  in bench (advisePow . embed :: Cyc t m r -> Cyc t m' r) y

bench_embedDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedDec x =
  let y = adviseDec x
  in bench (adviseDec . embed :: Cyc t m r -> Cyc t m' r) y