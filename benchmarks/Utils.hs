{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             PolyKinds, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Utils where

import Control.Monad.Random
import Control.Monad (liftM)
import Control.Exception
import Criterion

import Crypto.Lol (Int64,Fact,Factored,valueFact,Mod(..), Proxy(..), proxy, Cyc)
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZqBasic
import Crypto.Random.DRBG

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

-- for tests that produce a (rnd Benchmarkable), to avoid incoherent instances
newtype MBench rnd = MBench (rnd Benchmarkable)

class GenArgs rnd bnch where
  genArgs :: bnch -> rnd Benchmarkable

instance (Monad rnd) => GenArgs rnd Benchmarkable where
  genArgs = return

instance (Monad rnd) => GenArgs rnd (MBench rnd) where
  genArgs (MBench x) = x

instance {-# Overlappable #-} (Random a, GenArgs rnd b, MonadRandom rnd) => GenArgs rnd (a -> b) where
  genArgs f = do
    x <- getRandom
    genArgs $ f x

data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)

-- a wrapper type for printing test/benchmark names
data BenchType (a :: k) = BT

instance (Fact m) => Show (BenchType m) where
  show _ = "F" ++ (show $ proxy valueFact (Proxy::Proxy m))

instance (Mod (ZqBasic q i), Show i) => Show (BenchType (ZqBasic q i)) where
  show _ = "Q" ++ (show $ proxy modulus (Proxy::Proxy (ZqBasic q i)))

instance (Show (BenchType a), Show (BenchType b)) => Show (BenchType (a,b)) where
  show _ = (show (BT :: BenchType a)) ++ "*" ++ (show (BT :: BenchType b))

instance (Show (BenchType a), Show (BenchType b)) => Show (BenchType '((a :: Factored), (b :: *))) where
  show _ = (show (BT :: BenchType a)) ++ "/" ++ (show (BT :: BenchType b))
