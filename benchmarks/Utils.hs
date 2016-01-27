{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             PolyKinds, RankNTypes, ConstraintKinds, ScopedTypeVariables, KindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Utils where

import Control.Monad.Random
import Control.Monad (liftM)
import Control.Exception
import Criterion

import Crypto.Lol (Int64,Fact,Factored,valueFact,Mod(..), Proxy(..), proxy, Cyc, RT, CT)
--import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZqBasic
import Crypto.Random.DRBG

import Data.Constraint

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

instance (GenArg rnd a, GenArgs rnd b, Monad rnd) => GenArgs rnd (a -> b) where
  genArgs f = do
    x <- genArg
    genArgs $ f x

class GenArg rnd arg where
  genArg :: rnd arg

instance {-# Overlappable #-} (Random a, MonadRandom rnd) => GenArg rnd a where
  genArg = getRandom


data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)

-- a wrapper type for printing test/benchmark names
data BenchArgs (a :: k) = BT

instance (Fact m) => Show (BenchArgs m) where
  show _ = "F" ++ (show $ proxy valueFact (Proxy::Proxy m))

instance (Mod (ZqBasic q i), Show i) => Show (BenchArgs (ZqBasic q i)) where
  show _ = "Q" ++ (show $ proxy modulus (Proxy::Proxy (ZqBasic q i)))

instance Show (BenchArgs RT) where
  show _ = "RT"

instance Show (BenchArgs CT) where
  show _ = "CT"

instance (Show (BenchArgs a), Show (BenchArgs b)) => Show (BenchArgs (a,b)) where
  show _ = (show (BT :: BenchArgs a)) ++ "*" ++ (show (BT :: BenchArgs b))

instance (Show (BenchArgs t), Show (BenchArgs m), Show (BenchArgs r)) 
  => Show (BenchArgs '((t :: Factored -> * -> *), (m :: Factored), (r :: *))) where
  show _ = (show (BT :: BenchArgs t)) ++ " " ++ (show (BT :: BenchArgs m)) ++ " " ++ (show (BT :: BenchArgs r))

instance (Show (BenchArgs t), Show (BenchArgs m), Show (BenchArgs m'), Show (BenchArgs r)) 
  => Show (BenchArgs '((t :: Factored -> * -> *), (m :: Factored), (m' :: Factored), (r :: *))) where
  show _ = (show (BT :: BenchArgs t)) ++ " " ++ 
           (show (BT :: BenchArgs m)) ++ " " ++ 
           (show (BT :: BenchArgs m')) ++ " " ++
           (show (BT :: BenchArgs r))

type family (f :: (k1 -> k2)) <$>  (xs :: [k1]) where
  f <$> '[] = '[]
  f <$> (x ': xs) = (f x) ': (f <$> xs)

type family (fs :: [k1 -> k2]) <*> (xs :: [k1]) where
  fs <*> xs = Go fs xs xs

type family Go (fs :: [k1 -> k2]) (xs :: [k1]) (ys :: [k1]) where
  Go '[] xs ys = '[]
  Go (f ': fs) '[] ys = Go fs ys ys
  Go (f ': fs) (x ': xs) ys = (f x) ': (Go (f ': fs) xs ys)


class Run ctx (params :: [k]) where
  runAll :: (MonadRandom rnd) => 
            Proxy params
            -> (ArgsCtx ctx -> rnd Benchmark) 
            -> [rnd Benchmark]

instance Run ctx '[] where
  runAll _ _ = []

data family ArgsCtx ctx