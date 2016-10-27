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
{-
instance (MonadRandom m) => MonadRandom (PropertyM m) where
  getRandom = run getRandom
  getRandoms = run getRandoms
  getRandomR r = run $ getRandomR r
  getRandomRs r = run $ getRandomRs r


-}