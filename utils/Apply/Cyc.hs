{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Apply.Cyc where

import Control.DeepSeq
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Lol.Types.ZPP
import Crypto.Random.DRBG

import Apply
import GenArgs
import Utils

data BasicCtxD
type BasicCtx t m r =
  (CElt t r, Fact m, IntegralDomain r, Random r, Eq r, NFElt r, ShowType '(t,m,r),
   Random (t m r), m `Divides` m, NFData (t m r))
data instance ArgsCtx BasicCtxD where
    BC :: (BasicCtx t m r) => Proxy '(t,m,r) -> ArgsCtx BasicCtxD
instance (params `Satisfy` BasicCtxD, BasicCtx t m r)
  => ( '(t, '(m,r)) ': params) `Satisfy` BasicCtxD where
  run _ f = f (BC (Proxy::Proxy '(t,m,r))) : run (Proxy::Proxy params) f

applyBasic :: (params `Satisfy` BasicCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m r . (BasicCtx t m r, Generatable rnd r, Generatable rnd (t m r))
       => Proxy '(t,m,r) -> rnd res)
  -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p

data UnzipCtxD
type UnzipCtx t m r =
  (Fact m, CElt t (r,r), Random (t m (r,r)), CElt t r, ShowType '(t,m,r), NFElt r, Random r, NFData (t m r))
data instance ArgsCtx UnzipCtxD where
    UzC :: (UnzipCtx t m r) => Proxy '(t,m,r) -> ArgsCtx UnzipCtxD
instance (params `Satisfy` UnzipCtxD, UnzipCtx t m r)
  => ( '(t, '(m,r)) ': params) `Satisfy` UnzipCtxD where
  run _ f = f (UzC (Proxy::Proxy '(t,m,r))) : run (Proxy::Proxy params) f

applyUnzip :: (params `Satisfy` UnzipCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m r . (UnzipCtx t m r, Generatable rnd (t m (r,r)))
       => Proxy '(t,m,r) -> rnd res)
  -> [rnd res]
applyUnzip params g = run params $ \(UzC p) -> g p

-- r is Liftable
data LiftCtxD
type LiftCtx t m r =
  (BasicCtx t m r, Lift' r, CElt t (LiftOf r), NFElt (LiftOf r), ToInteger (LiftOf r),
   TElt CT r, TElt RT r, TElt CT (LiftOf r), TElt RT (LiftOf r), NFData (t m (LiftOf r)))
data instance ArgsCtx LiftCtxD where
    LC :: (LiftCtx t m r) => Proxy '(t,m,r) -> ArgsCtx LiftCtxD
instance (params `Satisfy` LiftCtxD, LiftCtx t m r)
  => ( '(t, '(m,r)) ': params) `Satisfy` LiftCtxD  where
  run _ f = f (LC (Proxy::Proxy '(t,m,r))) : run (Proxy::Proxy params) f

applyLift :: (params `Satisfy` LiftCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m r . (LiftCtx t m r, Generatable rnd r) => Proxy '(t,m,r) -> rnd res)
  -> [rnd res]
applyLift params g = run params $ \(LC p) -> g p

-- similar to LiftCtxD, but with a `gen` param
data ErrorCtxD
type ErrorCtx t m r gen = (CElt t r, Fact m, ShowType '(t,m,r,gen),
                           CElt t (LiftOf r), NFElt (LiftOf r), Lift' r,
                           ToInteger (LiftOf r), CryptoRandomGen gen, NFData (t m (LiftOf r)))
data instance ArgsCtx ErrorCtxD where
    EC :: (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> ArgsCtx ErrorCtxD
instance (params `Satisfy` ErrorCtxD, ErrorCtx t m r gen)
  => ( '(gen, '(t, '(m,r))) ': params) `Satisfy` ErrorCtxD  where
  run _ f = f (EC (Proxy::Proxy '(t,m,r,gen))) : run (Proxy::Proxy params) f

applyError :: (params `Satisfy` ErrorCtxD) =>
  Proxy params
  -> (forall t m r gen . (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> rnd res)
  -> [rnd res]
applyError params g = run params $ \(EC p) -> g p


data TwoIdxCtxD
type TwoIdxCtx t m m' r = (m `Divides` m', CElt t r, IntegralDomain r, Eq r, Random r, NFElt r,
                           ShowType '(t,m,m',r), Random (t m r), Random (t m' r), NFData (t m r), NFData (t m' r))
data instance ArgsCtx TwoIdxCtxD where
    TI :: (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx TwoIdxCtxD
instance (params `Satisfy` TwoIdxCtxD, TwoIdxCtx t m m' r)
  => ( '(t, '(m,m',r)) ': params) `Satisfy` TwoIdxCtxD where
  run _ f = f (TI (Proxy::Proxy '(t,m,m',r))) : run (Proxy::Proxy params) f

applyTwoIdx :: (params `Satisfy` TwoIdxCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m m' r . (TwoIdxCtx t m m' r, Generatable rnd (t m r), Generatable rnd (t m' r))
        => Proxy '(t,m,m',r) -> rnd res)
  -> [rnd res]
applyTwoIdx params g = run params $ \(TI p) -> g p

-- similar to TwoIdxCtxD, but r must be a prime-power
data BasisCtxD
type BasisCtx t m m' r = (m `Divides` m', Eq r, ZPP r, CElt t r, CElt t (ZpOf r), ShowType '(t,m,m',r))
data instance ArgsCtx BasisCtxD where
    BsC :: (BasisCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx BasisCtxD
instance (params `Satisfy` BasisCtxD, BasisCtx t m m' r)
  => ( '(t, '(m,m',r)) ': params) `Satisfy` BasisCtxD where
  run _ f = f (BsC (Proxy::Proxy '(t,m,m',r))) : run (Proxy::Proxy params) f

applyBasis :: (params `Satisfy` BasisCtxD) =>
  Proxy params
  -> (forall t m m' r . (BasisCtx t m m' r) => Proxy '(t,m,m',r) -> rnd res)
  -> [rnd res]
applyBasis params g = run params $ \(BsC p) -> g p
