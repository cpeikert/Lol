{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses,NoImplicitPrelude, RankNTypes, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, 
             TypeOperators, UndecidableInstances #-}

module UCycBenches (ucycBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol hiding (Cyc, adviseCRT)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types.Random

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

import Utils

ucycBenches :: (MonadRandom rnd) => rnd Benchmark
ucycBenches = bgroupRnd "UCyc"
  [bgroupRnd "*"       $ benchBasic $ wrapUCyc bench_mul
  ]

-- no CRT conversion, just coefficient-wise multiplication
bench_mul :: (BasicCtx t m r) => UCyc t m r -> UCyc t m r -> NFValue
bench_mul a b = 
  let a' = adviseCRT a
      b' = adviseCRT b
  in nf (a' *) b'

type Tensors = '[{-CT,-}RT]
type MM'RCombos = 
  '[ --'(F4, F128, Zq 257),
     --'(F1, PToF Prime281, Zq 563),
     --'(F12, F32 * F9, Zq 512),
     '(F12, F32 * F9, Zq 577)
     --'(F12, F32 * F9, Zq (577 ** 1153)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017))
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593))
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     --'(F12, F32 * F9, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     --'(F12, F32 * F9 * F25, Zq 14401)
    ]

-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> (Nub (Map RemoveM MM'RCombos))
type LiftParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable (Map RemoveM MM'RCombos)))

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m',r) = Int64 :== (LiftOf r)

data RemoveM :: TyFun (Factored, Factored, *) (Factored, *) -> *
type instance Apply RemoveM '(m,m',r) = '(m',r)


data BasicCtxD
type BasicCtx t m r = (CElt t r, Fact m, ShowArgs '(t,m,r))
instance (params `Satisfy` BasicCtxD, BasicCtx t m r) => ( '(t, '(m,r)) ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx t m r) => Proxy '(t,m,r) -> ArgsCtx BasicCtxD
  runAll _ f = (f $ BC (Proxy::Proxy '(t,m,r))) : (runAll (Proxy::Proxy params) f)

hideTMR :: (forall t m r . (BasicCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> ArgsCtx BasicCtxD -> rnd Benchmark
hideTMR f (BC p) = f p

wrapUCyc :: (Functor rnd, Benchmarkable rnd (UCyc t m r -> bnch), ShowArgs '(t,m,r)) 
  => (UCyc t m r -> bnch) -> Proxy '(t,m,r) -> rnd Benchmark
wrapUCyc f p = bench (showArgs p) <$> genArgs f

benchBasic :: (forall t m r . (BasicCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) -> [rnd Benchmark]
benchBasic g = runAll (Proxy::Proxy AllParams) $ hideTMR g
