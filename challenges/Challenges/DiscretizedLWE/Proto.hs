{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.DiscretizedLWE.Proto
(LWEInstance(..)
,LWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact)
import Crypto.Lol.Types.Proto
import Challenges.ContinuousLWE.Proto (LWESecret(..))
import qualified Challenges.Proto.LWEInstance as P
import qualified Challenges.Proto.LWESample as P
import qualified Challenges.Proto.LWESecret as P
import Challenges.Proto.InstType

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)

-- | Corresponds to LWEInstance proto type.
data LWEInstance v t m zq = LWEInstance Int v v [LWESample t m zq]
instance (NFData (LWESample t m zq), NFData v) => NFData (LWEInstance v t m zq) where
  rnf (LWEInstance idx v bound ss) = (rnf idx) `seq` (rnf v) `seq` (rnf bound) `seq` (rnf ss)
deriving instance (Read (LWESample t m zq), Read v) => Read (LWEInstance v t m zq)
deriving instance (Show (LWESample t m zq), Show v) => Show (LWEInstance v t m zq)
deriving instance (Eq (LWESample t m zq), Eq v) => Eq (LWEInstance v t m zq)
instance (Protoable (Cyc t m zq), Mod zq, ModRep zq ~ Int64, Fact m) 
  => Protoable (LWEInstance Double t m zq) where
  type ProtoType (LWEInstance Double t m zq) = P.LWEInstance
  toProto (LWEInstance idx v bound samples) = 
    P.LWEInstance DiscLWE
                  (fromIntegral idx) 
                  (fromIntegral (proxy valueFact (Proxy::Proxy m)))
                  (fromIntegral (proxy modulus (Proxy::Proxy zq)))
                  v
                  bound
                  (S.fromList $ map toProto samples)
  fromProto (P.LWEInstance DiscLWE idx m q v bound samples) = 
    LWEInstance (fromIntegral idx) v bound $ map fromProto $ S.toList samples

-- | Corresponds to LWESample proto type.
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
