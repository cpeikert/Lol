{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Harness.Zq
(applyBasic
,LiftedMod(..)
,LiftedInvertible(..)
,Invertible(..)) where

import Apply
import Gen
import Utils

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.CRTrans

data BasicCtxD
type BasicCtx r =
  (Field r, Eq r, Random r, Random (ModRep r), ToInteger (ModRep r),
   PID (ModRep r), ShowType r, CRTEmbed r, Mod r)
instance (params `Satisfy` BasicCtxD, BasicCtx r)
  => ( r ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx r) => Proxy r -> ArgsCtx BasicCtxD
  run _ f = f (BC (Proxy::Proxy r)) : run (Proxy::Proxy params) f

applyBasic :: (params `Satisfy` BasicCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall r . (BasicCtx r, Generatable rnd r, Generatable rnd (Invertible r))
       => Proxy r -> rnd res)
  -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p


data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

newtype Invertible r = Invertible r

instance (MonadRandom rnd, Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedMod r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
    in LMod <$> getRandomR (0,q-1)

instance (MonadRandom rnd, Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedInvertible r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
        go = do
          x <- getRandomR (1,q-1)
          if gcd x q == 1
          then return $ LInv x
          else go
    in go

instance (MonadRandom rnd, Generatable rnd (LiftedInvertible r), Ring r)
  => Generatable rnd (Invertible r) where
  genArg = do
    (LInv x) :: LiftedInvertible r <- genArg
    return $ Invertible $ fromIntegral x