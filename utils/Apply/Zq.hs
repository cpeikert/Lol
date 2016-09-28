{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, MultiParamTypeClasses, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Apply.Zq
(applyBasic
,LiftedMod(..)
,LiftedInvertible(..)
,Invertible(..)) where

import Apply
import GenArgs
import GenArgs.Zq
import Utils

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.CRTrans

data BasicCtxD
type BasicCtx r =
  (Field r, Eq r, Random r, Random (ModRep r), ToInteger (ModRep r),
   PID (ModRep r), ShowType r, CRTEmbed r, Mod r)
data instance ArgsCtx BasicCtxD where
    BC :: (BasicCtx r) => Proxy r -> ArgsCtx BasicCtxD
instance (params `Satisfy` BasicCtxD, BasicCtx r)
  => ( r ': params) `Satisfy` BasicCtxD where
  run _ f = f (BC (Proxy::Proxy r)) : run (Proxy::Proxy params) f

applyBasic :: (params `Satisfy` BasicCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall r . (BasicCtx r, Generatable rnd r, Generatable rnd (Invertible r))
       => Proxy r -> rnd res)
  -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p