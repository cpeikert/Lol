{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module CycBenches (cycBenches) where

import Apply.Cyc
import Benchmarks
import BenchParams

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Random.DRBG

cycBenches :: IO Benchmark
cycBenches = benchGroup "Cyc" [
  benchGroup "unzipPow"    $ [hideArgs bench_unzipCycPow testParam], -- applyUnzip  allParams    $ hideArgs bench_unzipCycPow,
  benchGroup "unzipDec"    $ [hideArgs bench_unzipCycDec testParam],
  benchGroup "unzipCRT"    $ [hideArgs bench_unzipCycCRT testParam], --applyUnzip  allParams    $ hideArgs bench_unzipCycCRT,
  benchGroup "zipWith (*)" $ [hideArgs bench_mul testParam], -- applyBasic  allParams    $ hideArgs bench_mul,
  benchGroup "crt"         $ [hideArgs bench_crt testParam],     --applyBasic  allParams    $ hideArgs bench_crt,
  benchGroup "crtInv"      $ [hideArgs bench_crtInv testParam],    --applyBasic  allParams    $ hideArgs bench_crtInv,
  benchGroup "l"           $ [hideArgs bench_l testParam],         --applyBasic  allParams    $ hideArgs bench_l,
  benchGroup "lInv"        $ [hideArgs bench_lInv testParam],
  benchGroup "*g Pow"      $ [hideArgs bench_mulgPow testParam],   --applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "*g Dec"      $ [hideArgs bench_mulgDec testParam],
  benchGroup "*g CRT"      $ [hideArgs bench_mulgCRT testParam], --applyBasic  allParams    $ hideArgs bench_mulgCRT,
  benchGroup "divg Pow"    $ [hideArgs bench_divgPow testParam],   --applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "divg Dec"    $ [hideArgs bench_divgDec testParam],
  benchGroup "divg CRT"    $ [hideArgs bench_divgCRT testParam],
  benchGroup "lift"        $ [hideArgs bench_liftPow testParam], --applyLift   liftParams   $ hideArgs bench_liftPow,
  benchGroup "error"       $ [hideArgs (bench_errRounded 0.1) testParam'], --applyError  errorParams  $ hideArgs $ bench_errRounded 0.1,
  benchGroup "twacePow"    $ [hideArgs bench_twacePow twoIdxParam], --applyTwoIdx twoIdxParams $ hideArgs bench_twacePow,
  benchGroup "twaceCRT"    $ [hideArgs bench_twaceCRT twoIdxParam],
  benchGroup "embedPow"    $ [hideArgs bench_embedPow twoIdxParam], --applyTwoIdx twoIdxParams $ hideArgs bench_embedPow
  benchGroup "embedDec"    $ [hideArgs bench_embedDec twoIdxParam],
  benchGroup "embedCRT"    $ [hideArgs bench_embedCRT twoIdxParam]
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

-- multiply by g when input is in Dec basis
bench_mulgDec :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgDec x = let y = adviseDec x in bench mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_mulgCRT x = let y = adviseCRT x in bench mulG y

-- divide by g when input is in Pow basis
bench_divgPow :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgPow x = let y = advisePow $ mulG x in bench divG y

-- divide by g when input is in Dec basis
bench_divgDec :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgDec x = let y = adviseDec $ mulG x in bench divG y

-- divide by g when input is in CRT basis
bench_divgCRT :: (BasicCtx t m r) => Cyc t m r -> Bench '(t,m,r)
bench_divgCRT x = let y = adviseCRT x in bench divG y

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))) gen

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

bench_embedCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => Cyc t m r -> Bench '(t,m,m',r)
bench_embedCRT x =
  let y = adviseCRT x
  in bench (adviseCRT . embed :: Cyc t m r -> Cyc t m' r) y
