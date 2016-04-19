{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Crypto.Challenges.DiscretizedLWE.Proto
(RLWEInstanceDisc(..)
,RLWESampleDisc(..)
,RLWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Rq
import Crypto.Challenges.ContinuousLWE.Proto (RLWESecret(..))
import qualified Crypto.Challenges.Proto.RLWE.RLWEInstanceDisc as P
import qualified Crypto.Challenges.Proto.RLWE.RLWESampleDisc as P
import qualified Crypto.Challenges.Proto.RLWE.RLWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)
import Data.Word

-- | Corresponds to RLWEInstanceDisc proto type.
data RLWEInstanceDisc v t m zq = RLWEInstanceDisc Word32 v Word64 [RLWESampleDisc t m zq]
instance (NFData (RLWESampleDisc t m zq), NFData v) => NFData (RLWEInstanceDisc v t m zq) where
  rnf (RLWEInstanceDisc idx v bound ss) = (rnf idx) `seq` (rnf v) `seq` (rnf bound) `seq` (rnf ss)
deriving instance (Show (RLWESampleDisc t m zq), Show v) => Show (RLWEInstanceDisc v t m zq)
deriving instance (Eq (RLWESampleDisc t m zq), Eq v) => Eq (RLWEInstanceDisc v t m zq)
instance (Protoable (RLWESampleDisc t m zq), Mod zq, Fact m) 
  => Protoable (RLWEInstanceDisc Double t m zq) where
  type ProtoType (RLWEInstanceDisc Double t m zq) = P.RLWEInstanceDisc
  toProto (RLWEInstanceDisc idx v bound samples) = 
    P.RLWEInstanceDisc 
      idx
      (fromIntegral (proxy valueFact (Proxy::Proxy m)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq)))
      v
      bound
      (S.fromList $ map toProto samples)
  fromProto = 
    let mval = fromIntegral $ proxy valueFact (Proxy::Proxy m)
        qval = fromIntegral $ proxy modulus (Proxy::Proxy zq)
    in \(P.RLWEInstanceDisc idx m q v bound samples) ->
      if (m == mval) && (q == qval)
      then RLWEInstanceDisc idx v bound $ map fromProto $ S.toList samples
      else error "RLWEInstanceDisc ProtoType: m or q values don't match"

-- | Corresponds to RLWESampleDisc proto type.
data RLWESampleDisc t m r = RLWESampleDisc (Cyc t m r) (Cyc t m r)
instance (Protoable (Cyc t m r), ProtoType (Cyc t m r) ~ Rq) 
  => Protoable (RLWESampleDisc t m r) where
  type ProtoType (RLWESampleDisc t m r) = P.RLWESampleDisc
  toProto (RLWESampleDisc a b) = P.RLWESampleDisc (toProto a) (toProto b)
  fromProto (P.RLWESampleDisc a b) = RLWESampleDisc (fromProto a) (fromProto b)
