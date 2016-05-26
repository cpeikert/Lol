{-# LANGUAGE CPP                   #-}
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

module Harness.Cyc where

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Types.ZPP
import Crypto.Random.DRBG

import Utils
import Gen
import Apply

#if ACCELERATE_TENSOR_ENABLE
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
#endif


data BasicCtxD
type BasicCtx t m r =
  (CElt t r, Fact m, Random r, Eq r, NFElt r, ShowType '(t,m,r), Random (t m r), m `Divides` m)

instance (params `Satisfy` BasicCtxD, BasicCtx t m r)
    => ( '(t, '(m,r)) ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx t m r) => Proxy '(t,m,r) -> ArgsCtx BasicCtxD
  run _ f = (f $ BC (Proxy::Proxy '(t,m,r))) : (run (Proxy::Proxy params) f)

applyBasic
    :: (params `Satisfy` BasicCtxD, MonadRandom rnd)
    => Proxy params
    -> (forall t m r . (BasicCtx t m r, Generatable rnd r, Generatable rnd (t m r)) => Proxy '(t,m,r) -> rnd res)
    -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p

-- r is Liftable
data LiftCtxD
type LiftCtx t m r =
  ( BasicCtx t m r, Lift' r, Lift' (TRep t r), CElt t (LiftOf r), NFElt (LiftOf r), ToInteger (LiftOf r)
  , TRep t (LiftOf r) ~ LiftOf (TRep t r) -- TLM: hmm...
  , CRTrans Maybe Int r
  , TElt CT r, TElt CT (LiftOf r)
  , TElt RT r, TElt RT (LiftOf r)
#if ACCELERATE_TENSOR_ENABLE
  , TElt AT r, TElt AT (LiftOf r)
#endif
  )

instance (params `Satisfy` LiftCtxD, LiftCtx t m r)
    => ( '(t, '(m,r)) ': params) `Satisfy` LiftCtxD  where
  data ArgsCtx LiftCtxD where
    LC :: (LiftCtx t m r) => Proxy '(t,m,r) -> ArgsCtx LiftCtxD
  run _ f = (f $ LC (Proxy::Proxy '(t,m,r))) : (run (Proxy::Proxy params) f)

applyLift
    :: (params `Satisfy` LiftCtxD, MonadRandom rnd)
    => Proxy params
    -> (forall t m r . (LiftCtx t m r, Generatable rnd r) => Proxy '(t,m,r) -> rnd res)
    -> [rnd res]
applyLift params g = run params $ \(LC p) -> g p

-- similar to LiftCtxD, but with a `gen` param
data ErrorCtxD
type ErrorCtx t m r gen =
  ( Fact m, ShowType '(t,m,r,gen)
  , CElt t r, CElt t (LiftOf r), NFElt (LiftOf r), Lift' r
  , ToInteger (TRep t (LiftOf r)), CryptoRandomGen gen
  , RealField (TRep t Double) , Transcendental (TRep t Double)
  )

instance (params `Satisfy` ErrorCtxD, ErrorCtx t m r gen)
  => ( '(gen, '(t, '(m,r))) ': params) `Satisfy` ErrorCtxD  where
  data ArgsCtx ErrorCtxD where
    EC :: (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> ArgsCtx ErrorCtxD
  run _ f = (f $ EC (Proxy::Proxy '(t,m,r,gen))) : (run (Proxy::Proxy params) f)

applyError
    :: (params `Satisfy` ErrorCtxD, Monad rnd)
    => Proxy params
    -> (forall t m r gen . (ErrorCtx t m r gen) => Proxy '(t,m,r,gen) -> rnd res)
    -> [rnd res]
applyError params g = run params $ \(EC p) -> g p


data TwoIdxCtxD
type TwoIdxCtx t m m' r = (m `Divides` m', CElt t r, Eq r, Random r, NFElt r, ShowType '(t,m,m',r), Random (t m r), Random (t m' r))

instance (params `Satisfy` TwoIdxCtxD, TwoIdxCtx t m m' r)
  => ( '(t, '(m,m',r)) ': params) `Satisfy` TwoIdxCtxD where
  data ArgsCtx TwoIdxCtxD where
    TI :: (TwoIdxCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx TwoIdxCtxD
  run _ f = (f $ TI (Proxy::Proxy '(t,m,m',r))) : (run (Proxy::Proxy params) f)

applyTwoIdx :: (params `Satisfy` TwoIdxCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m m' r . (TwoIdxCtx t m m' r, Generatable rnd (t m r), Generatable rnd (t m' r))
        => Proxy '(t,m,m',r) -> rnd res)
  -> [rnd res]
applyTwoIdx params g = run params $ \(TI p) -> g p

-- similar to TwoIdxCtxD, but r must be a prime-power
data BasisCtxD
type BasisCtx t m m' r =
  ( m `Divides` m', Eq r, ShowType '(t,m,m',r)
  , ZPP r, ZPP (TRep t r), TRep t (ZpOf r) ~ ZpOf (TRep t r)
  , CElt t r, CElt t (ZpOf r)
  )

instance (params `Satisfy` BasisCtxD, BasisCtx t m m' r)
  => ( '(t, '(m,m',r)) ': params) `Satisfy` BasisCtxD where
  data ArgsCtx BasisCtxD where
    BsC :: (BasisCtx t m m' r) => Proxy '(t,m,m',r) -> ArgsCtx BasisCtxD
  run _ f = (f $ BsC (Proxy::Proxy '(t,m,m',r))) : (run (Proxy::Proxy params) f)

applyBasis
    :: (params `Satisfy` BasisCtxD)
    => Proxy params
    -> (forall t m m' r . (BasisCtx t m m' r) => Proxy '(t,m,m',r) -> rnd res)
    -> [rnd res]
applyBasis params g = run params $ \(BsC p) -> g p

