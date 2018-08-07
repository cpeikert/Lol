{-|
Module      : Crypto.Lol.Utils.ShowType
Description : Pretty print type parameters.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\C{\mathbb{C}} \)

Class for pretty-printing type parameters to tests and benchmarks
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Utils.ShowType
( ArgType
, showType
, ShowType
, Zq
, type (**)
) where

import Crypto.Lol                      (BaseBGad, Complex, Fact, Int64,
                                        Mod (..), Proxy (..), TrivGad,
                                        proxy, valueFact)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic hiding (ZqB)

import GHC.TypeLits

-- | Convenient syntax for multiplication of moduli in a 'Zq'.
infixr 9 **
data a ** b

-- | Converts '**' syntax into nested pair representation.
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


-- | Wrapper type for printing test/benchmark names.
data ArgType (a :: k) = AT

-- | Constraint synonym for printing test parameters.
type ShowType a = Show (ArgType a)

-- | Print a showable type argument.
showType :: forall a . (Show (ArgType a)) => Proxy a -> String
showType _ = show (AT :: ArgType a)

instance (KnownNat n) => Show (ArgType n) where
  show _ = show $ natVal (Proxy::Proxy n)

instance (Fact m) => Show (ArgType m) where
  show _ = "F" ++ show (valueFact @m)

instance (Show (ArgType a), Show (ArgType b)) => Show (ArgType '(a,b)) where
  show _ = "(" ++ show (AT :: ArgType a) ++ "," ++ show (AT :: ArgType b) ++ ")"

data InternalList a

instance (Show (ArgType (InternalList xs))) => Show (ArgType (xs :: [k])) where
  show _ = "[" ++ show (AT :: ArgType (InternalList xs)) ++ "]"

instance (Show (ArgType a), Show (ArgType (InternalList as))) => Show (ArgType (InternalList (a ': as))) where
  show _ = show (AT :: ArgType a) ++ "," ++ show (AT :: ArgType (InternalList as))

instance Show (ArgType (InternalList '[])) where
  show _ = ""

instance (Mod (ZqBasic q z), Show z) => Show (ArgType (ZqBasic q z)) where
  show _ = "Q" ++ show (modulus @(ZqBasic q z))

instance Show (ArgType Int64) where
  show _ = "Int64"

instance Show (ArgType Double) where
  show _ = "Double"

instance Show (ArgType (Complex Double)) where
  show _ = "Complex Double"

instance Show (ArgType TrivGad) where
  show _ = "TrivGad"

instance (Reflects b Integer) => Show (ArgType (BaseBGad (b :: k))) where
  show _ = "Base" ++ show (value @b :: Integer) ++ "Gad"

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
