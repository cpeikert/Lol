{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses,
             PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module TestTypes
(SmoothQ1, SmoothQ2, SmoothQ3
,SmoothZQ1, SmoothZQ2, SmoothZQ3
,Zq
,ZQ1,ZQ2,ZQ3) where

import Control.Monad.Random

import Crypto.Lol

import Utils

import Test.QuickCheck.Monadic

instance (MonadRandom m) => MonadRandom (PropertyM m) where
  getRandom = run getRandom
  getRandoms = run getRandoms
  getRandomR r = run $ getRandomR r
  getRandomRs r = run $ getRandomRs r

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = Zq (19393921 ** 18869761)
type ZQ3 = Zq (19918081 ** 19393921 ** 18869761)

-- the next three moduli are "good" for any index dividing 128*27*25*7
type SmoothQ1 = 2148249601
type SmoothQ2 = 2148854401
type SmoothQ3 = 2150668801

type SmoothZQ1 = Zq 2148249601
type SmoothZQ2 = Zq (2148854401 ** 2148249601)
type SmoothZQ3 = Zq (2148854401 ** 2148249601 ** 2150668801)
