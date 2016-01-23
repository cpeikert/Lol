{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, TypeOperators, PolyKinds #-}

module Utils where

import Control.Monad.Random
import Criterion

import Crypto.Lol (Int64)
import Crypto.Lol.Types.ZqBasic

{-
import Math.NumberTheory.Primes.Testing (isPrime)

-- an infinite list of primes greater than the input and congruent to
-- 1 mod m
goodQs :: (Integral i) => i -> i -> [i]
goodQs m lower = checkVal (lower + ((m-lower) `mod` m) + 1)
  where checkVal v = if (isPrime (fromIntegral v :: Integer))
                     then v : checkVal (v+m)
                    else checkVal (v+m)
-}

bgroupRnd :: (Monad rnd) => String -> [rnd Benchmark] -> rnd Benchmark
bgroupRnd str = (bgroup str <$>) . sequence

class GenArgs bnch where
  genArgs :: (MonadRandom rnd) => bnch -> rnd Benchmarkable

instance GenArgs Benchmarkable where
  genArgs = return

instance (Random a) => GenArgs (a -> Benchmarkable) where
  genArgs f = do
    x <- getRandom
    return $ f x

instance {-# Overlappable #-} (Random a, GenArgs b) => GenArgs (a -> b) where
  genArgs f = do
    x <- getRandom
    genArgs $ f x

data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)