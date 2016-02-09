{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, RankNTypes, 
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Harness.Cyc where

import Control.Applicative

import Crypto.Lol hiding (CT)
import Crypto.Random.DRBG

import Utils

wrap' :: forall a rnd bnch res c . 
  (Benchmarkable rnd bnch, Monad rnd, ShowArgs a,
   WrapFunc res, res ~ ResultOf bnch, res ~ c a)
  => bnch -> Proxy a -> rnd (WrapOf res)
wrap' f p = wrap (showArgs p) <$> genArgs f

data BasicCtxD
type BasicCtx t m r = (CElt t r, Fact m, ShowArgs '(t,m,r))
instance (params `Satisfy` BasicCtxD, BasicCtx t m r) 
  => ( '(t, '(m,r)) ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx t m r) => Proxy '(t,m,r) -> ArgsCtx BasicCtxD
  run _ f = (f $ BC (Proxy::Proxy '(t,m,r))) : (run (Proxy::Proxy params) f)

benchBasic :: (params `Satisfy` BasicCtxD) =>
  Proxy params 
  -> (forall t m r . (BasicCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) 
  -> [rnd Benchmark]
benchBasic params g = run params $ \(BC p) -> g p


data LiftCtxD
type LiftCtx t m r = (BasicCtx t m r, CElt t (LiftOf r), Lift' r, ToInteger (LiftOf r))
instance (params `Satisfy` LiftCtxD, LiftCtx t m r) 
  => ( '(t, '(m,r)) ': params) `Satisfy` LiftCtxD  where
  data ArgsCtx LiftCtxD where
    LC :: (LiftCtx t m r) => Proxy '(t,m,r) -> ArgsCtx LiftCtxD
  run _ f = (f $ LC (Proxy::Proxy '(t,m,r))) : (run (Proxy::Proxy params) f)

benchLift :: (params `Satisfy` LiftCtxD) =>
  Proxy params 
  -> (forall t m r . (LiftCtx t m r) => Proxy '(t,m,r) -> rnd Benchmark) 
  -> [rnd Benchmark]
benchLift params g = run params $ \(LC p) -> g p


data ErrorCtxD
type ErrorCtx t m r gen = (CElt t r, Fact m, ShowArgs '(t,m,r,gen), 
                           CElt t (LiftOf r), Lift' r, 
                           ToInteger (LiftOf r), CryptoRandomGen gen)
instance (params `Satisfy` ErrorCtxD, ErrorCtx t m r gen) 
  => ( '(gen, '(t, '(m,r))) ': params) `Satisfy` ErrorCtxD  where
  data ArgsCtx ErrorCtxD where
    EC :: (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> ArgsCtx ErrorCtxD
  run _ f = (f $ EC (Proxy::Proxy '(t,m,r,gen))) : (run (Proxy::Proxy params) f)

benchError :: (params `Satisfy` ErrorCtxD, Monad rnd) =>
  Proxy params 
  -> (forall t m r gen . (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> rnd Benchmark) 
  -> [rnd Benchmark]
benchError params g = run params $ \(EC p) -> g p


data TwoIdxCtxD
type TwoIdxCtx t m m' r = (m `Divides` m', CElt t r, ShowArgs '(t,m,m',r))

instance (params `Satisfy` TwoIdxCtxD, TwoIdxCtx t m m' r) 
  => ( '(t, '(m,m',r)) ': params) `Satisfy` TwoIdxCtxD where
  data ArgsCtx TwoIdxCtxD where
    TI :: (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx TwoIdxCtxD
  run _ f = (f $ TI (Proxy::Proxy '(t,m,m',r))) : (run (Proxy::Proxy params) f)

benchTwoIdx :: (params `Satisfy` TwoIdxCtxD) =>
  Proxy params 
  -> (forall t m m' r . (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> rnd Benchmark) 
  -> [rnd Benchmark]
benchTwoIdx params g = run params $ \(TI p) -> g p
