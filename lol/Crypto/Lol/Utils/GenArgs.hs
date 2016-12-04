{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Classes used to generate arguments for tests and benchmarks.
-- This is analagous to 'Arbitrary' in quickcheck, except it can also be used
-- with criterion.

module Crypto.Lol.Utils.GenArgs where

import Control.Monad.Random

type family ResultOf a where
  ResultOf (a -> b) = ResultOf b
  ResultOf a = a

-- | Generalization of 'Testable' from QuickCheck: generates function inputs
class GenArgs fun where
  genArgs :: (MonadRandom rnd) => fun -> rnd (ResultOf fun)

instance (Random a, GenArgs b) => GenArgs (a -> b) where
  genArgs f = getRandom >>= (genArgs . f)
