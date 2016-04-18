{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.DiscretizedLWE.Proto
(DiscLWEInstance(..)
,DiscLWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact)
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Rq
import Challenges.ContinuousLWE.Proto (LWESecret(..))
import qualified Challenges.Proto.DiscLWEInstance as P
import qualified Challenges.Proto.DiscLWESample as P
import qualified Challenges.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)
import Data.Word

-- | Corresponds to DiscLWEInstance proto type.
data DiscLWEInstance v t m zq = DiscLWEInstance Int v Word64 [DiscLWESample t m zq]
instance (NFData (DiscLWESample t m zq), NFData v) => NFData (DiscLWEInstance v t m zq) where
  rnf (DiscLWEInstance idx v bound ss) = (rnf idx) `seq` (rnf v) `seq` (rnf bound) `seq` (rnf ss)
deriving instance (Show (DiscLWESample t m zq), Show v) => Show (DiscLWEInstance v t m zq)
deriving instance (Eq (DiscLWESample t m zq), Eq v) => Eq (DiscLWEInstance v t m zq)
instance (Protoable (DiscLWESample t m zq), Mod zq, ModRep zq ~ Int64, Fact m) 
  => Protoable (DiscLWEInstance Double t m zq) where
  type ProtoType (DiscLWEInstance Double t m zq) = P.DiscLWEInstance
  toProto (DiscLWEInstance idx v bound samples) = 
    P.DiscLWEInstance 
      (fromIntegral idx) 
      (fromIntegral (proxy valueFact (Proxy::Proxy m)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq)))
      v
      bound
      (S.fromList $ map toProto samples)
  fromProto (P.DiscLWEInstance idx m q v bound samples) = 
    DiscLWEInstance (fromIntegral idx) v bound $ map fromProto $ S.toList samples

-- | Corresponds to DiscLWESample proto type.
data DiscLWESample t m r = DiscLWESample (Cyc t m r) (Cyc t m r)
deriving instance (Show (Cyc t m r)) => Show (DiscLWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (DiscLWESample t m r)
instance (NFData (Cyc t m r)) => NFData (DiscLWESample t m r) where
  rnf (DiscLWESample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (Cyc t m r), ProtoType (Cyc t m r) ~ Rq) => Protoable (DiscLWESample t m r) where
  type ProtoType (DiscLWESample t m r) = P.DiscLWESample
  toProto (DiscLWESample a b) = P.DiscLWESample (toProto a) (toProto b)
  fromProto (P.DiscLWESample a b) = DiscLWESample (fromProto a) (fromProto b)
