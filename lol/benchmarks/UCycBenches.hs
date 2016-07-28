{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module UCycBenches (ucycBenches) where

import Apply.Cyc
import Benchmarks
import BenchParams

import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types
import Crypto.Random.DRBG

ucycBenches :: IO Benchmark
ucycBenches = benchGroup "UCyc" [
  benchGroup "unzipPow"    $ [hideArgs bench_unzipUCycPow testParam], -- applyUnzip  allParams    $ hideArgs bench_unzipUCycPow,
  benchGroup "unzipDec"    $ [hideArgs bench_unzipUCycDec testParam],
  benchGroup "unzipCRT"    $ [hideArgs bench_unzipUCycCRT testParam], -- applyUnzip  allParams    $ hideArgs bench_unzipUCycCRT,
  benchGroup "zipWith (*)" $ [hideArgs bench_mul testParam], -- applyBasic  allParams    $ hideArgs bench_mul,
  benchGroup "crt"         $ [hideArgs bench_crt testParam], -- applyBasic  allParams    $ hideArgs bench_crt,
  benchGroup "crtInv"      $ [hideArgs bench_crtInv testParam], -- applyBasic  allParams    $ hideArgs bench_crtInv,
  benchGroup "l"           $ [hideArgs bench_l testParam], -- applyBasic  allParams    $ hideArgs bench_l,
  benchGroup "lInv"        $ [hideArgs bench_lInv testParam],
  benchGroup "*g Pow"      $ [hideArgs bench_mulgPow testParam], -- applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "*g Dec"      $ [hideArgs bench_mulgDec testParam],
  benchGroup "*g CRT"      $ [hideArgs bench_mulgCRT testParam], -- applyBasic  allParams    $ hideArgs bench_mulgCRT,
  benchGroup "divg Pow"    $ [hideArgs bench_divgPow testParam], -- applyBasic  allParams    $ hideArgs bench_mulgPow,
  benchGroup "divg Dec"    $ [hideArgs bench_divgDec testParam],
  benchGroup "divg CRT"    $ [hideArgs bench_divgCRT testParam],
  benchGroup "lift"        $ [hideArgs bench_liftPow testParam], -- applyLift   liftParams   $ hideArgs bench_liftPow,
  benchGroup "error"       $ [hideArgs (bench_errRounded 0.1) testParam'], -- applyError  errorParams  $ hideArgs $ bench_errRounded 0.1
  benchGroup "twacePow"    $ [hideArgs bench_twacePow twoIdxParam], -- applyTwoIdx twoIdxParams $ hideArgs bench_twacePow,
  benchGroup "twaceCRT"    $ [hideArgs bench_twaceCRT twoIdxParam],
  benchGroup "embedPow"    $ [hideArgs bench_embedPow twoIdxParam], -- applyTwoIdx twoIdxParams $ hideArgs bench_embedPow
  benchGroup "embedDec"    $ [hideArgs bench_embedDec twoIdxParam],
  benchGroup "embedCRT"    $ [hideArgs bench_embedCRT twoIdxParam]
  ]

bench_unzipUCycPow :: (UnzipCtx t m r) => UCyc t m P (r,r) -> Bench '(t,m,r)
bench_unzipUCycPow = bench unzipPow

bench_unzipUCycDec :: (UnzipCtx t m r) => UCyc t m D (r,r) -> Bench '(t,m,r)
bench_unzipUCycDec = bench unzipDec

bench_unzipUCycCRT :: (UnzipCtx t m r) => UCycPC t m (r,r) -> Bench '(t,m,r)
bench_unzipUCycCRT (Right a) = bench unzipCRTC a

pcToEC :: UCycPC t m r -> UCycEC t m r
pcToEC (Right x) = (Right x)

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => UCycPC t m r -> UCycPC t m r -> Bench '(t,m,r)
bench_mul a b =
  let a' = pcToEC a
      b' = pcToEC b
  in bench (a' *) b'

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => UCyc t m P r -> Bench '(t,m,r)
bench_crt = bench toCRT

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => UCycPC t m r -> Bench '(t,m,r)
bench_crtInv (Right a) = bench toPow a

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => UCyc t m D r -> Bench '(t,m,r)
bench_l = bench toPow

-- convert input from Pow basis to Dec basis
bench_lInv :: (BasicCtx t m r) => UCyc t m P r -> Bench '(t,m,r)
bench_lInv = bench toDec

-- lift an element in the Pow basis
bench_liftPow :: (LiftCtx t m r) => UCyc t m P r -> Bench '(t,m,r)
bench_liftPow = bench lift

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => UCyc t m P r -> Bench '(t,m,r)
bench_mulgPow = bench mulG

-- multiply by g when input is in Dec basis
bench_mulgDec :: (BasicCtx t m r) => UCyc t m D r -> Bench '(t,m,r)
bench_mulgDec = bench mulG

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => UCycPC t m r -> Bench '(t,m,r)
bench_mulgCRT (Right a) = bench mulG a

-- divide by g when input is in Pow basis
bench_divgPow :: (BasicCtx t m r) => UCyc t m P r -> Bench '(t,m,r)
bench_divgPow x =
  let y = mulG x
  in bench divG y

-- divide by g when input is in Dec basis
bench_divgDec :: (BasicCtx t m r) => UCyc t m D r -> Bench '(t,m,r)
bench_divgDec x =
  let y = mulG x
  in bench divG y

-- divide by g when input is in CRT basis
bench_divgCRT :: (BasicCtx t m r) => UCycPC t m r -> Bench '(t,m,r)
bench_divgCRT (Right a) = bench divG a

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen)
  => Double -> Bench '(t,m,r,gen)
bench_errRounded v = benchIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (UCyc t m D (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCyc t m' P r -> Bench '(t,m,m',r)
bench_twacePow = bench (twacePow :: UCyc t m' P r -> UCyc t m P r)

bench_twaceCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCycPC t m' r -> Bench '(t,m,m',r)
bench_twaceCRT (Right a) = bench (twaceCRTC :: UCyc t m' C r -> UCycPC t m r) a

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCyc t m P r -> Bench '(t,m,m',r)
bench_embedPow = bench (embedPow :: UCyc t m P r -> UCyc t m' P r)

bench_embedDec :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCyc t m D r -> Bench '(t,m,m',r)
bench_embedDec = bench (embedDec :: UCyc t m D r -> UCyc t m' D r)

bench_embedCRT :: forall t m m' r . (TwoIdxCtx t m m' r)
  => UCycPC t m r -> Bench '(t,m,m',r)
bench_embedCRT (Right a) = bench (embedCRTC :: UCyc t m C r -> UCycPC t m' r) a
