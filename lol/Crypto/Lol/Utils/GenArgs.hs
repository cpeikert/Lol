{-|
Module      : Crypto.Lol.Utils.GenArgs
Description : Generate arguments for tests and benchmarks.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\C{\mathbb{C}} \)

Classes used to generate arguments for tests and benchmarks.
This is analagous to 'Arbitrary' in quickcheck, except it can also be used
with criterion.
-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Utils.GenArgs where

import Control.Monad.Random

-- | Type of the output of a fully-applied function.
type family ResultOf a where
  ResultOf (a -> b) = ResultOf b
  ResultOf a = a

-- | Generalization of 'Testable' from QuickCheck: generates function inputs.
class GenArgs fun where
  genArgs :: (MonadRandom rnd) => fun -> rnd (ResultOf fun)

-- | Generate arguments to a function.
instance (Random a, GenArgs b) => GenArgs (a -> b) where
  genArgs f = getRandom >>= (genArgs . f)
