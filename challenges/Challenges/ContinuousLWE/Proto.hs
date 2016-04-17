{-# LANGUAGE FlexibleInstances, GADTs, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.ContinuousLWE.Proto
(ContLWEInstance(..)
,ContLWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact, Factored)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.R
import Crypto.Lol.Types.Proto.Rq
import Crypto.Lol.Types.Proto.RRq
import qualified Challenges.Proto.ContLWEInstance as P
import qualified Challenges.Proto.ContLWESample as P
import qualified Challenges.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection hiding (D)
import Data.Sequence as S (fromList, Seq)

-- | Corresponds to LWESecret proto type.
data LWESecret t m z = LWESecret Int (Cyc t m z)
deriving instance (Read (Cyc t m z)) => Read (LWESecret t m z)
deriving instance (Show (Cyc t m z)) => Show (LWESecret t m z)
deriving instance (Eq (Cyc t m z)) => Eq (LWESecret t m z)
instance (NFData (Cyc t m z)) => NFData (LWESecret t m z) where
  rnf (LWESecret idx s) = (rnf idx) `seq` (rnf s)
instance (Protoable (Cyc t m z), ProtoType (Cyc t m z) ~ R, Fact m) => Protoable (LWESecret t m z) where
  type ProtoType (LWESecret t m z) = P.LWESecret
  toProto (LWESecret idx s) = 
    P.LWESecret (fromIntegral idx) (fromIntegral $ proxy valueFact (Proxy::Proxy m)) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    LWESecret (fromIntegral idx) $ fromProto s

-- | Corresponds to ContLWEInstance proto type.
data ContLWEInstance v t m zq rq = ContLWEInstance Int v v [ContLWESample t m zq rq]
instance (NFData (ContLWESample t m zq rq), NFData v) => NFData (ContLWEInstance v t m zq rq) where
  rnf (ContLWEInstance idx v bound ss) = (rnf idx) `seq` (rnf v) `seq` (rnf bound) `seq` (rnf ss)
deriving instance (Read (ContLWESample t m zq rq), Read v) => Read (ContLWEInstance v t m zq rq)
deriving instance (Show (ContLWESample t m zq rq), Show v) => Show (ContLWEInstance v t m zq rq)
deriving instance (Eq (ContLWESample t m zq rq), Eq v) => Eq (ContLWEInstance v t m zq rq)
instance (Protoable (ContLWESample t m zq rq), Mod zq, ModRep zq ~ Int64, Fact m)
  => Protoable (ContLWEInstance Double t m zq rq) where
  type ProtoType (ContLWEInstance Double t m zq rq) = P.ContLWEInstance
  toProto (ContLWEInstance idx v bound samples) = 
    P.ContLWEInstance
      (fromIntegral idx) 
      (fromIntegral (proxy valueFact (Proxy::Proxy m)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq)))
      v
      bound
      (S.fromList $ map toProto samples)
  fromProto (P.ContLWEInstance idx m q v bound samples) = 
    ContLWEInstance (fromIntegral idx) v bound $ map fromProto $ S.toList samples

-- | Corresponds to ContLWESample proto type.
data ContLWESample t m zq rq = ContLWESample (Cyc t m zq) (UCyc t m P rq)
deriving instance (Read (Cyc t m zq), Read (UCyc t m P rq)) => Read (ContLWESample t m zq rq)
deriving instance (Show (Cyc t m zq), Show (UCyc t m P rq)) => Show (ContLWESample t m zq rq)
deriving instance (Eq (Cyc t m zq), Eq (UCyc t m P rq)) => Eq (ContLWESample t m zq rq)
instance (NFData (Cyc t m zq), NFData (UCyc t m P rq)) => NFData (ContLWESample t m zq rq) where
  rnf (ContLWESample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (Cyc t m zq), 
          ProtoType (Cyc t m zq) ~ Rq,
          Protoable (UCyc t m P rq),
          ProtoType (UCyc t m P rq) ~ RRq) 
  => Protoable (ContLWESample t m zq rq) where
  type ProtoType (ContLWESample t m zq rq) = P.ContLWESample
  toProto (ContLWESample a b) = P.ContLWESample (toProto a) (toProto b)
  fromProto (P.ContLWESample a b) = ContLWESample (fromProto a) (fromProto b)
