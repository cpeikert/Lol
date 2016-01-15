{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module TestTypes (
  ZP2, ZP3, ZP4, ZP8
, PP2, F89
, SmoothZQ1, SmoothZQ2, SmoothZQ3
, SmoothQ1
, Zq, ZQ1, ZQ2, ZQ3) where

import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Reflects

import Test.QuickCheck.Monadic

instance (MonadRandom m) => MonadRandom (PropertyM m) where
  getRandom = run getRandom
  getRandoms = run getRandoms
  getRandomR r = run $ getRandomR r
  getRandomRs r = run $ getRandomRs r

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = (Zq 19393921, ZQ1)
type ZQ3 = (Zq 19918081, ZQ2)

-- the next three moduli are "good" for any index dividing 128*27*25*7
type SmoothQ1 = 2148249601
type SmoothQ2 = 2148854401
type SmoothQ3 = 2150668801

type Zq (q :: k) = ZqBasic q Z
type Z = Int64

type ZP2 = Zq PP2
type ZP3 = Zq PP3
type ZP4 = Zq PP4
type ZP8 = Zq PP8

type SmoothZQ1 = Zq SmoothQ1
type SmoothZQ2 = (Zq SmoothQ2, SmoothZQ1)
type SmoothZQ3 = (Zq SmoothQ3, SmoothZQ2)
