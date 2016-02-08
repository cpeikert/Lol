{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             PolyKinds, RankNTypes, ConstraintKinds, ScopedTypeVariables, KindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Utils 
(bgroupRnd
,module Criterion
,Benchmarkable(..)
,Generatable(..)
,NFValue
,BenchArgs
,Satisfy(..)

,Zq
,type (**)

,type (<$>)
,type (<*>)

,showArgs
,ShowArgs) where

import Control.Monad.Random
import Control.Monad (liftM)
import GHC.TypeLits

import Criterion (Benchmark,bench,nf,nfIO,bgroup)
import qualified Criterion as C

import Crypto.Lol (Int64,Fact,Factored,valueFact,Mod(..), Proxy(..), proxy, Cyc, RT, CT)
import Crypto.Lol.Types.ZqBasic
import Crypto.Random.DRBG
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Repa as R

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

type NFValue = C.Benchmarkable

-- bnch represents a function whose arguments can be generated,
-- resulting in a "NFValue"
class Benchmarkable rnd bnch where
  genArgs :: bnch -> rnd NFValue

instance (Monad rnd) => Benchmarkable rnd NFValue where
  genArgs = return

instance (Generatable rnd a, Benchmarkable rnd b, Monad rnd) => Benchmarkable rnd (a -> b) where
  genArgs f = do
    x <- genArg
    genArgs $ f x

-- a parameter that can be generated using a particular monad
class Generatable rnd arg where
  genArg :: rnd arg

instance {-# Overlappable #-} (Random a, MonadRandom rnd) => Generatable rnd a where
  genArg = getRandom

instance {-# Overlaps #-} (U.Unbox a, Random a, MonadRandom rnd) => Generatable rnd (U.Vector a) where
  genArg = U.replicateM 100 getRandom

instance {-# Overlaps #-} (U.Unbox a, Random a, MonadRandom rnd)
    => Generatable rnd (R.Array R.U R.DIM1 a) where
  genArg = R.fromUnboxed (R.Z R.:. 100) <$> genArg

class (params :: [k]) `Satisfy` ctx  where
  data family ArgsCtx ctx

  runAll :: Proxy params
            -> (ArgsCtx ctx -> rnd Benchmark) 
            -> [rnd Benchmark]

instance '[] `Satisfy` ctx  where
  -- any implementation of ArgsCtx must conflict
  
  runAll _ _ = []


infixr 9 **
data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)


type family (f :: (k1 -> k2)) <$>  (xs :: [k1]) where
  f <$> '[] = '[]
  f <$> (x ': xs) = (f x) ': (f <$> xs)

type family (fs :: [k1 -> k2]) <*> (xs :: [k1]) where
  fs <*> xs = Go fs xs xs

type family Go (fs :: [k1 -> k2]) (xs :: [k1]) (ys :: [k1]) where
  Go '[] xs ys = '[]
  Go (f ': fs) '[] ys = Go fs ys ys
  Go (f ': fs) (x ': xs) ys = (f x) ': (Go (f ': fs) xs ys)


-- allows automatic printing of test parameters
type ShowArgs a = Show (BenchArgs a)

showArgs :: forall a . (Show (BenchArgs a)) => Proxy a -> String
showArgs _ = show (BT :: BenchArgs a)

-- a wrapper type for printing test/benchmark names
data BenchArgs (a :: k) = BT

instance (Fact m) => Show (BenchArgs m) where
  show _ = "F" ++ (show $ proxy valueFact (Proxy::Proxy m))

instance (Mod (ZqBasic q i), Show i) => Show (BenchArgs (ZqBasic q i)) where
  show _ = "Q" ++ (show $ proxy modulus (Proxy::Proxy (ZqBasic q i)))

instance (KnownNat q) => Show (BenchArgs (q :: Nat)) where
  show _ = show $ natVal (Proxy::Proxy q)

instance Show (BenchArgs RT) where
  show _ = "RT"

instance Show (BenchArgs CT) where
  show _ = "CT"

-- for RNS-style moduli
instance (Show (BenchArgs a), Show (BenchArgs b)) => Show (BenchArgs (a,b)) where
  show _ = (show (BT :: BenchArgs a)) ++ "*" ++ (show (BT :: BenchArgs b))

instance (Show (BenchArgs a), Show (BenchArgs b)) 
  => Show (BenchArgs '(a,b)) where
  show _ = (show (BT :: BenchArgs a)) ++ " " ++ (show (BT :: BenchArgs b))

instance (Show (BenchArgs a), Show (BenchArgs '(b,c))) 
  => Show (BenchArgs '(a,b,c)) where
  show _ = (show (BT :: BenchArgs a)) ++ " " ++ (show (BT :: BenchArgs '(b,c)))

instance (Show (BenchArgs a), Show (BenchArgs '(b,c,d))) 
  => Show (BenchArgs '(a,b,c,d)) where
  show _ = (show (BT :: BenchArgs a)) ++ " " ++ (show (BT :: BenchArgs '(b,c,d)))

instance (Show (BenchArgs a), Show (BenchArgs '(b,c,d,e))) 
  => Show (BenchArgs '(a,b,c,d,e)) where
  show _ = (show (BT :: BenchArgs a)) ++ " " ++ (show (BT :: BenchArgs '(b,c,d,e)))
