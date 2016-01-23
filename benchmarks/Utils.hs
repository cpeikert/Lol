{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Utils where

import Control.Monad.Random
import Criterion

bgroupRnd :: (Monad rnd) => String -> [rnd Benchmark] -> rnd Benchmark
bgroupRnd str = (bgroup str <$>) . sequence

class GenArgs bnch where
  genArgs :: (MonadRandom rnd) => String -> bnch -> rnd Benchmark

instance GenArgs Benchmark where
  genArgs _ = return

instance (Random a) => GenArgs (a -> String -> Benchmark) where
  genArgs str f = do
    x <- getRandom
    return $ f x str

instance {-# Overlappable #-} (Random a, GenArgs b) => GenArgs (a -> b) where
  genArgs str f = do
    x <- getRandom
    genArgs str $ f x