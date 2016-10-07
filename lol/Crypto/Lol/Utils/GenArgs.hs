{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Classes used to generate arguments for tests and benchmarks.
-- This is analagous to 'Arbitrary' in quickcheck, except it can also be used
-- with criterion.

module Crypto.Lol.Utils.GenArgs where

import Control.Monad.Random

type family ResultOf f where
  ResultOf (a -> b) = ResultOf b
  ResultOf a = a

-- | Generalization of 'Testable' from QuickCheck: generates function inputs
class GenArgs rnd fun where
  genArgs :: fun -> rnd (ResultOf fun)

instance (Generatable rnd a, GenArgs rnd b,
          Monad rnd, ResultOf b ~ ResultOf (a -> b))
  => GenArgs rnd (a -> b) where
  genArgs f = do
    x <- genArg
    genArgs $ f x

-- | Similar to 'Arbitrary' from QuickCheck: generate 'arg' in monad 'rnd'
class Generatable rnd arg where
  genArg :: rnd arg

instance {-# Overlappable #-} (Random a, MonadRandom rnd) => Generatable rnd a where
  genArg = getRandom
