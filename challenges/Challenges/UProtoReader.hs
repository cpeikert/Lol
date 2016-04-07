{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.UProtoReader
(LWEInstance(..)
,LWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact, Factored)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types.Proto
import qualified Challenges.Proto.LWEInstance as P
import qualified Challenges.Proto.LWESample as P
import qualified Challenges.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection hiding (D)
import Data.Sequence as S (fromList, Seq)

-- | Corresponds to LWESecret proto type.
data LWESecret t m z = LWESecret Int (UCyc t m P z)
deriving instance (Read (UCyc t m P z)) => Read (LWESecret t m z)
deriving instance (Show (UCyc t m P z)) => Show (LWESecret t m z)
deriving instance (Eq (UCyc t m P z)) => Eq (LWESecret t m z)
instance (NFData (UCyc t m P z)) => NFData (LWESecret t m z) where
  rnf (LWESecret idx s) = (rnf idx) `seq` (rnf s)
instance (Protoable (UCyc t m P z), Fact m) => Protoable (LWESecret t m z) where
  type ProtoType (LWESecret t m z) = P.LWESecret
  toProto (LWESecret idx s) = 
    P.LWESecret (fromIntegral idx) (fromIntegral $ proxy valueFact (Proxy::Proxy m)) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    LWESecret (fromIntegral idx) $ fromProto s

-- | Corresponds to LWEInstance proto type.
data LWEInstance v t m zq = LWEInstance Int v [LWESample t m zq]
instance (NFData (LWESample t m zq), NFData v) => NFData (LWEInstance v t m zq) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss)
deriving instance (Read (LWESample t m zq), Read v) => Read (LWEInstance v t m zq)
deriving instance (Show (LWESample t m zq), Show v) => Show (LWEInstance v t m zq)
deriving instance (Eq (LWESample t m zq), Eq v) => Eq (LWEInstance v t m zq)
instance (Protoable (UCyc t m D zq), Mod zq, ModRep zq ~ Int64, Fact m) => Protoable (LWEInstance Double t m zq) where
  type ProtoType (LWEInstance Double t m zq) = P.LWEInstance
  toProto (LWEInstance idx v samples) = 
    P.LWEInstance (fromIntegral idx) 
                  (fromIntegral (proxy valueFact (Proxy::Proxy m)))
                  (fromIntegral (proxy modulus (Proxy::Proxy zq)))
                  v
                  (S.fromList $ map toProto samples)
  fromProto (P.LWEInstance idx m q v samples) = LWEInstance (fromIntegral idx) v $ map fromProto $ S.toList samples

-- | Corresponds to LWESample proto type.
data LWESample t m r = LWESample (UCyc t m D r) (UCyc t m D r)
deriving instance (Read (UCyc t m D r)) => Read (LWESample t m r)
deriving instance (Show (UCyc t m D r)) => Show (LWESample t m r)
deriving instance (Eq (UCyc t m D r)) => Eq (LWESample t m r)
instance (NFData (UCyc t m D r)) => NFData (LWESample t m r) where
  rnf (LWESample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (UCyc t m D r)) => Protoable (LWESample t m r) where
  type ProtoType (LWESample t m r) = P.LWESample
  toProto (LWESample a b) = P.LWESample (toProto a) (toProto b)
  fromProto (P.LWESample a b) = LWESample (fromProto a) (fromProto b)
