{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, KindSignatures, MultiParamTypeClasses, PolyKinds,
             RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Utils
(Zq
,type (**)
,type (<$>)
,type (<*>)

,module Data.Promotion.Prelude.List
,showType
,ShowType) where

import Crypto.Lol (Int64,Fact,valueFact,Mod(..), Proxy(..), proxy, TrivGad, BaseBGad)
import Crypto.Lol.Reflects
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Types.ZqBasic
import Crypto.Random.DRBG

import Data.Promotion.Prelude.List

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







-- a wrapper type for printing test/benchmark names
data ArgType (a :: k) = AT

-- allows automatic printing of test parameters
type ShowType a = Show (ArgType a)

showType :: forall a . (Show (ArgType a)) => Proxy a -> String
showType _ = show (AT :: ArgType a)

instance Show (ArgType HashDRBG) where
  show _ = "HashDRBG"

instance (Fact m) => Show (ArgType m) where
  show _ = "F" ++ show (proxy valueFact (Proxy::Proxy m))

instance (Show (ArgType a), Show (ArgType b)) => Show (ArgType '(a,b)) where
  show _ = "(" ++ show (AT :: ArgType a) ++ "," ++ show (AT :: ArgType b) ++ ")"

data InternalList a

instance (Show (ArgType (InternalList xs))) => Show (ArgType (xs :: [k])) where
  show _ = "[" ++ show (AT :: ArgType (InternalList xs)) ++ "]"

instance (Show (ArgType a), Show (ArgType (InternalList as))) => Show (ArgType (InternalList (a ': as))) where
  show _ = show (AT :: ArgType a) ++ "," ++ show (AT :: ArgType (InternalList as))

instance Show (ArgType (InternalList '[])) where
  show _ = ""

instance (Mod (ZqBasic q i), Show i) => Show (ArgType (ZqBasic q i)) where
  show _ = "Q" ++ show (proxy modulus (Proxy::Proxy (ZqBasic q i)))

instance Show (ArgType RT) where
  show _ = "RT"

instance Show (ArgType CT) where
  show _ = "CT"

instance Show (ArgType Int64) where
  show _ = "Int64"

instance Show (ArgType TrivGad) where
  show _ = "TrivGad"

instance (Reflects b Integer) => Show (ArgType (BaseBGad (b :: k))) where
  show _ = "Base" ++ show (proxy value (Proxy::Proxy b) :: Integer) ++ "Gad"

-- for RNS-style moduli
instance (Show (ArgType a), Show (ArgType b)) => Show (ArgType (a,b)) where
  show _ = show (AT :: ArgType a) ++ "*" ++ show (AT :: ArgType b)

instance (Show (ArgType a), Show (ArgType '(b,c)))
  => Show (ArgType '(a,b,c)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c))

instance (Show (ArgType a), Show (ArgType '(b,c,d)))
  => Show (ArgType '(a,b,c,d)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c,d))

instance (Show (ArgType a), Show (ArgType '(b,c,d,e)))
  => Show (ArgType '(a,b,c,d,e)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c,d,e))

instance (Show (ArgType a), Show (ArgType '(b,c,d,e,f)))
  => Show (ArgType '(a,b,c,d,e,f)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c,d,e,f))

instance (Show (ArgType a), Show (ArgType '(b,c,d,e,f,g)))
  => Show (ArgType '(a,b,c,d,e,f,g)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c,d,e,f,g))

instance (Show (ArgType a), Show (ArgType '(b,c,d,e,f,g,h)))
  => Show (ArgType '(a,b,c,d,e,f,g,h)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType '(b,c,d,e,f,g,h))
