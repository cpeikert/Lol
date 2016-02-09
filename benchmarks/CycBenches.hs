{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses,NoImplicitPrelude, RankNTypes, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, 
             TypeOperators, UndecidableInstances #-}

module CycBenches (cycBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

import Utils hiding (Liftable)
import Harness.Cyc

cycBenches :: (MonadRandom rnd) => rnd Benchmark
cycBenches = bgroupRnd "Cyc"
  [bgroupRnd "*"       $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_mul,
   bgroupRnd "crt"     $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_crt,
   bgroupRnd "crtInv"  $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_crtInv,
   bgroupRnd "l"       $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_l,
   bgroupRnd "*g Pow"  $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_mulgPow,
   bgroupRnd "*g CRT"  $ benchBasic (Proxy::Proxy AllParams) $ wrap' bench_mulgCRT,
   bgroupRnd "lift"    $ benchLift  (Proxy::Proxy LiftParams) $ wrap' bench_liftPow,
   bgroupRnd "error"   $ benchError (Proxy::Proxy ErrorParams) $ wrap' $ bench_errRounded 0.1,
   bgroupRnd "twace"   $ benchTwoIdx (Proxy::Proxy TwoIdxParams) $ wrap' bench_twacePow,
   bgroupRnd "embed"   $ benchTwoIdx (Proxy::Proxy TwoIdxParams) $ wrap' bench_embedPow
  ]

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => Cyc t m r -> Cyc t m r -> NFValue' '(t,m,r)
bench_mul a b = 
  let a' = adviseCRT a
      b' = adviseCRT b
  in nfv (a' *) b'

-- convert input from Pow basis to CRT basis
bench_crt :: (BasicCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_crt x = let y = advisePow x in nfv adviseCRT y

-- convert input from CRT basis to Pow basis
bench_crtInv :: (BasicCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_crtInv x = let y = adviseCRT x in nfv advisePow y

-- convert input from Dec basis to Pow basis
bench_l :: (BasicCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_l x = let y = adviseDec x in nfv advisePow y

-- lift an element in the Pow basis
bench_liftPow :: forall t m r . (LiftCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_liftPow x = let y = advisePow x in nfv (liftCyc Pow :: Cyc t m r -> Cyc t m (LiftOf r)) y

-- multiply by g when input is in Pow basis
bench_mulgPow :: (BasicCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_mulgPow x = let y = advisePow x in nfv mulG y

-- multiply by g when input is in CRT basis
bench_mulgCRT :: (BasicCtx t m r) => Cyc t m r -> NFValue' '(t,m,r)
bench_mulgCRT x = let y = adviseCRT x in nfv mulG y

-- generate a rounded error term
bench_errRounded :: forall t m r gen . (ErrorCtx t m r gen) 
  => Double -> NFValue' '(t,m,r,gen)
bench_errRounded v = NFV $ nfIO $ do
  gen <- newGenIO
  return $ evalRand (errorRounded v :: Rand (CryptoRand gen) (Cyc t m (LiftOf r))) gen

bench_twacePow :: forall t m m' r . (TwoIdxCtx t m m' r) 
  => Cyc t m' r -> NFValue' '(t,m,m',r)
bench_twacePow x = let y = advisePow x in nfv (twace :: Cyc t m' r -> Cyc t m r) y

bench_embedPow :: forall t m m' r . (TwoIdxCtx t m m' r) 
  => Cyc t m r -> NFValue' '(t,m,m',r)
bench_embedPow x = let y = advisePow x in nfv (embed :: Cyc t m r -> Cyc t m' r) y

type Tensors = '[CT,RT]
type MM'RCombos = 
  '[ '(F4, F128, Zq 257),
     '(F1, PToF Prime281, Zq 563),
     '(F12, F32 * F9, Zq 512),
     '(F12, F32 * F9, Zq 577),
     '(F12, F32 * F9, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 14401)
    ]
-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> (Nub (Map RemoveM MM'RCombos))
type LiftParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable (Map RemoveM MM'RCombos)))
type TwoIdxParams = ( '(,) <$> Tensors) <*> MM'RCombos

type ErrorParams = ( '(,) <$> '[HashDRBG]) <*> LiftParams

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m',r) = Int64 :== (LiftOf r)

data RemoveM :: TyFun (Factored, Factored, *) (Factored, *) -> *
type instance Apply RemoveM '(m,m',r) = '(m',r)


