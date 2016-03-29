{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module Challenges.ProtoReader
(-- type LWE data
 LWEInstance(..)
,LWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, CElt, LiftOf, proxy, Proxy(..), reifyFactI, valueFact, Int64, Lift', ToInteger, modulus, Mod(..), Fact, CT, RT, ZqBasic)
import Crypto.Lol.Types.Proto
import qualified Challenges.Proto.LWEInstance as P
import qualified Challenges.Proto.LWESample as P
import qualified Challenges.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)

-- corresponds to LWESecret proto type
data LWESecret t m z = LWESecret Int (Cyc t m z)
deriving instance (Read (Cyc t m z)) => Read (LWESecret t m z)
deriving instance (Show (Cyc t m z)) => Show (LWESecret t m z)
deriving instance (Eq (Cyc t m z)) => Eq (LWESecret t m z)
instance (NFData (Cyc t m z)) => NFData (LWESecret t m z) where
  rnf (LWESecret idx s) = (rnf idx) `seq` (rnf s)
instance (Protoable (Cyc t m z), Fact m) => Protoable (LWESecret t m z) where
  type ProtoType (LWESecret t m z) = P.LWESecret
  toProto (LWESecret idx s) = 
    P.LWESecret (fromIntegral idx) (fromIntegral $ proxy valueFact (Proxy::Proxy m)) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    LWESecret (fromIntegral idx) $ fromProto s

-- corresponds to LWEInstance proto type
data LWEInstance v t m zq = LWEInstance Int v [LWESample t m zq]
instance (NFData (LWESample t m zq), NFData v) => NFData (LWEInstance v t m zq) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss)
deriving instance (Read (LWESample t m zq), Read v) => Read (LWEInstance v t m zq)
deriving instance (Show (LWESample t m zq), Show v) => Show (LWEInstance v t m zq)
deriving instance (Eq (LWESample t m zq), Eq v) => Eq (LWEInstance v t m zq)
instance (Protoable (Cyc t m zq), Mod zq, ModRep zq ~ Int64, Fact m) => Protoable (LWEInstance Double t m zq) where
  type ProtoType (LWEInstance Double t m zq) = P.LWEInstance
  toProto (LWEInstance idx v samples) = 
    P.LWEInstance (fromIntegral idx) 
                  (fromIntegral (proxy valueFact (Proxy::Proxy m)))
                  (fromIntegral (proxy modulus (Proxy::Proxy zq)))
                  v
                  (S.fromList $ map toProto samples)
  fromProto (P.LWEInstance idx m q v samples) = LWEInstance (fromIntegral idx) v $ map fromProto $ S.toList samples

-- corresponds to LWESample proto type
data LWESample t m r = LWESample (Cyc t m r) (Cyc t m r)
deriving instance (Read (Cyc t m r)) => Read (LWESample t m r)
deriving instance (Show (Cyc t m r)) => Show (LWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (LWESample t m r)
instance (NFData (Cyc t m r)) => NFData (LWESample t m r) where
  rnf (LWESample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (Cyc t m r)) => Protoable (LWESample t m r) where
  type ProtoType (LWESample t m r) = P.LWESample
  toProto (LWESample a b) = P.LWESample (toProto a) (toProto b)
  fromProto (P.LWESample a b) = LWESample (fromProto a) (fromProto b)
