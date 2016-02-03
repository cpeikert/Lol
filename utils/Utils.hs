{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, 
             PolyKinds, RankNTypes, ConstraintKinds, ScopedTypeVariables, 
             KindSignatures,
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Utils 
(bgroupRnd
,module Criterion
,Benchmarkable(..)
,Generatable(..)
,NFValue
,NFValue'
,ResultOf
,AddZq
,Liftable
,NonLiftable

,WrapFunc(..)

,Satisfy(..)

,Zq
,type (**)

,type (<$>)
,type (<*>)

,nfv
,TestBool
,test

,showArgs
,ShowArgs) where

import Control.Monad.Random
import Control.Monad (liftM)
import Control.Monad.State

import Control.DeepSeq

import Criterion (Benchmark,bench,nf,nfIO,bgroup)
import qualified Criterion as C

import Crypto.Lol (Int64,Fact,Factored,valueFact,Mod(..), Proxy(..), proxy, Cyc, RT, CT, LiftOf, TrivGad, BaseBGad)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic
import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
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
newtype NFValue' params = NFV C.Benchmarkable

nfv :: NFData b => (a -> b) -> a -> NFValue' params
nfv f = NFV . nf f

newtype TestBool params = TestBool Bool

test :: Bool -> TestBool params
test = TestBool

type family ResultOf a where
  ResultOf (a -> b) = ResultOf b
  ResultOf a = a

-- bnch represents a function whose arguments can be generated,
-- resulting in a "NFValue"
class Benchmarkable rnd bnch where
  genArgs :: bnch -> rnd (ResultOf bnch)

instance (Monad rnd) => Benchmarkable rnd NFValue where
  genArgs = return

instance (Monad rnd) => Benchmarkable rnd (NFValue' params) where
  genArgs = return

instance (Monad rnd) => Benchmarkable rnd (TestBool params) where
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



class (params :: [k]) `Satisfy` ctx  where
  data family ArgsCtx ctx

  runAll :: Proxy params
            -> (ArgsCtx ctx -> rnd res) 
            -> [rnd res]

instance '[] `Satisfy` ctx  where
  -- any implementation of ArgsCtx must conflict
  
  runAll _ _ = []

class WrapFunc res where
  type WrapOf res

  wrap :: String -> res -> WrapOf res

instance WrapFunc NFValue where
  type WrapOf NFValue = Benchmark
  wrap = bench

instance WrapFunc (NFValue' params) where
  type WrapOf (NFValue' params) = Benchmark
  wrap str (NFV x) = bench str x

instance WrapFunc (TestBool params) where
  type WrapOf (TestBool params) = Test
  wrap str (TestBool x) = testProperty str $ property x






-- useful for generating parameters
type family RoundDown zq where
  RoundDown (a,(b,c)) = (b,c)
  RoundDown ((a,b),c) = (a,b)
  RoundDown (a,b) = a

data AddZq :: TyFun (Factored, Factored, *, *) (Factored, Factored, *, *, *) -> *
type instance Apply AddZq '(m,m',zp,zq) = '(m,m',zp,RoundDown zq,zq)

data Liftable :: TyFun (Factored, Factored, *, *) Bool -> *
type instance Apply Liftable '(m,m',zp,zq) = Int64 :== (LiftOf zq)

data NonLiftable :: TyFun (Factored, Factored, *, *) Bool -> *
type instance Apply NonLiftable '(m,m',zp,zq) = Integer :== (LiftOf zq)

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

instance Show (BenchArgs RT) where
  show _ = "RT"

instance Show (BenchArgs CT) where
  show _ = "CT"

instance Show (BenchArgs TrivGad) where
  show _ = "TrivGad"

instance (Reflects b Integer) => Show (BenchArgs (BaseBGad b)) where
  show _ = "Base" ++ (show $ (proxy value (Proxy::Proxy b) :: Integer)) ++ "Gad"

-- for RNS-style moduli
instance (Show (BenchArgs a), Show (BenchArgs b)) => Show (BenchArgs (a,b)) where
  show _ = (show (BT :: BenchArgs a)) ++ "*" ++ (show (BT :: BenchArgs b))

-- we use tuples rather than lists because types in a list must have the same kind,
-- but tuples permit different kinds
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

instance (Show (BenchArgs a), Show (BenchArgs '(b,c,d,e,f))) 
  => Show (BenchArgs '(a,b,c,d,e,f)) where
  show _ = (show (BT :: BenchArgs a)) ++ " " ++ (show (BT :: BenchArgs '(b,c,d,e,f)))