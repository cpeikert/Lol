{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Utils
(Zq
,ArgType
,showType
,ShowType) where

import Crypto.Lol (Int64,Fact,valueFact,Mod(..), Proxy(..), proxy, TrivGad, BaseBGad)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

infixr 9 **
data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)

-- a wrapper type for printing test/benchmark names
data ArgType (a :: k) = AT

-- allows automatic printing of test parameters
type ShowType a = Show (ArgType a)

showType :: forall a . (Show (ArgType a)) => Proxy a -> String
showType _ = show (AT :: ArgType a)

instance (Fact m) => Show (ArgType m) where
  show _ = "F" ++ show (proxy valueFact (Proxy::Proxy m))

instance (Mod (ZqBasic q i), Show i) => Show (ArgType (ZqBasic q i)) where
  show _ = "Q" ++ show (proxy modulus (Proxy::Proxy (ZqBasic q i)))

instance Show (ArgType Int64) where
  show _ = "Int64"

instance Show (ArgType TrivGad) where
  show _ = "TrivGad"

instance (Reflects b Integer) => Show (ArgType (BaseBGad (b :: k))) where
  show _ = "Base" ++ show (proxy value (Proxy::Proxy b) :: Integer) ++ "Gad"

-- for RNS-style moduli
instance (Show (ArgType a), Show (ArgType b)) => Show (ArgType (a,b)) where
  show _ = show (AT :: ArgType a) ++ "*" ++ show (AT :: ArgType b)

-- we use tuples rather than lists because types in a list must have the same kind,
-- but tuples permit different kinds
instance (Show (ArgType a), Show (ArgType b))
  => Show (ArgType '(a,b)) where
  show _ = show (AT :: ArgType a) ++ " " ++ show (AT :: ArgType b)

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
