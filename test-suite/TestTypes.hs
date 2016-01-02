{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, KindSignatures, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RankNTypes, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module TestTypes (
  ZP2, ZP3, ZP4, ZP8
, PP2, F89
, SmoothZQ1, SmoothZQ2, SmoothZQ3
, SmoothQ1
, Zq, ZQ1, ZQ2, ZQ3
, Q17, Q29, Q179, Q8191, Q80221, Q536871001) where

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
data Q18869761
instance (ToInteger i) => Reflects Q18869761 i where value = return 18869761
type ZQ1 = Zq Q18869761

data Q19393921
instance (ToInteger i) => Reflects Q19393921 i where value = return 19393921
type ZQ2 = (Zq Q19393921, ZQ1)

data Q19918081
instance (ToInteger i) => Reflects Q19918081 i where value = return 19918081
type ZQ3 = (Zq Q19918081, ZQ2)

-- a 31-bit modulus, for rounding off after the last four hops
data Q2149056001
instance (ToInteger i) => Reflects Q2149056001 i where value = return 2149056001
type ZQ4 = (Zq Q2149056001, ZQ3)

-- for rounding off after the first hop
data Q3144961
instance (ToInteger i) => Reflects Q3144961 i where value = return 3144961
type ZQ5 = (Zq Q3144961, ZQ4)

data Q7338241
instance (ToInteger i) => Reflects Q7338241 i where value = return 7338241
type ZQ6 = (Zq Q7338241, ZQ5)

-- concrete types useful for building tests or real applications
data Q17
data Q29
data Q179   -- = 1 mod 89
data Q8191  -- 1028th prime, = 1 mod 21
data Q80221  -- good for 28
data Q536871001
-- the next three moduli are "good" for any index dividing 128*27*25*7
data SmoothQ1
data SmoothQ2
data SmoothQ3

instance (ToInteger i) => Reflects Q17 i where value = return 17
instance (ToInteger i) => Reflects Q29 i where value = return 29
instance (ToInteger i) => Reflects Q179 i where value = return 179
instance (ToInteger i) => Reflects Q8191 i where value = return 8191
instance (ToInteger i) => Reflects Q536871001 i where value = return 536871001
instance (ToInteger i) => Reflects Q80221 i where value = return 80221
instance (ToInteger i) => Reflects SmoothQ1 i where value = return 2148249601
instance (ToInteger i) => Reflects SmoothQ2 i where value = return 2148854401
instance (ToInteger i) => Reflects SmoothQ3 i where value = return 2150668801

type Zq (q :: k) = ZqBasic q Z
type Z = Int64

type ZP2 = Zq PP2
type ZP3 = Zq PP3
type ZP4 = Zq PP4
type ZP8 = Zq PP8

type SmoothZQ1 = Zq SmoothQ1
type SmoothZQ2 = (Zq SmoothQ2, SmoothZQ1)
type SmoothZQ3 = (Zq SmoothQ3, SmoothZQ2)
