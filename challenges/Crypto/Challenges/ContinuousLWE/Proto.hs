{-# LANGUAGE FlexibleInstances, NoImplicitPrelude, ScopedTypeVariables, 
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized prototypes into parameterized Haskell
-- types.

module Crypto.Challenges.ContinuousLWE.Proto
( RLWEInstanceCont(..)
, RLWESampleCont(..)
, RLWESecret(..)
) where

import Control.Applicative
import Control.DeepSeq

import qualified Crypto.Challenges.Proto.RLWE.RLWEInstanceCont as P
import qualified Crypto.Challenges.Proto.RLWE.RLWESampleCont   as P
import qualified Crypto.Challenges.Proto.RLWE.RLWESecret       as P
import           Crypto.Lol
import           Crypto.Lol.Cyclotomic.UCyc
import           Crypto.Lol.Types.Proto
import           Crypto.Lol.Types.Proto.Kq
import           Crypto.Lol.Types.Proto.R
import           Crypto.Lol.Types.Proto.Rq

import Data.Foldable as S (toList)
import Data.Sequence as S (fromList)
import Data.Word

-- | Corresponds to 'RLWESecret' proto type.
data RLWESecret t m z = RLWESecret Word32 (Cyc t m z)

-- | Corresponds to 'RLWESampleCont' proto type.
data RLWESampleCont t m zq rq = RLWESampleCont (Cyc t m zq) (UCyc t m D rq)

-- | Corresponds to 'RLWEInstanceCont' proto type.
data RLWEInstanceCont v t m zq rq =
  RLWEInstanceCont Word32 v v [RLWESampleCont t m zq rq]

deriving instance (Show (RLWESampleCont t m zq rq), Show v)
                  => Show (RLWEInstanceCont v t m zq rq)
deriving instance (Eq (RLWESampleCont t m zq rq), Eq v)
                  => Eq (RLWEInstanceCont v t m zq rq)

instance (NFData (RLWESampleCont t m zq rq), NFData v)
         => NFData (RLWEInstanceCont v t m zq rq) where
  rnf (RLWEInstanceCont idx v bound ss) =
    rnf idx `seq` rnf v `seq` rnf bound `seq` rnf ss

instance (Protoable (Cyc t m z), ProtoType (Cyc t m z) ~ R, Fact m)
         => Protoable (RLWESecret t m z) where
  type ProtoType (RLWESecret t m z) = P.RLWESecret

  toProto (RLWESecret idx s) = P.RLWESecret idx
    (fromIntegral $ proxy valueFact (Proxy::Proxy m)) $ toProto s

  fromProto = let mval = fromIntegral $ proxy valueFact (Proxy::Proxy m)
              in \(P.RLWESecret idx m s) ->
                   if m == mval then RLWESecret idx $ fromProto s
                   else error "RLWESecret ProtoType: m values don't match"

instance (Protoable (Cyc t m zq), ProtoType (Cyc t m zq) ~ Rq,
          Protoable (UCyc t m D rq), ProtoType (UCyc t m D rq) ~ Kq)
         => Protoable (RLWESampleCont t m zq rq) where

  type ProtoType (RLWESampleCont t m zq rq) = P.RLWESampleCont

  toProto (RLWESampleCont a b) = P.RLWESampleCont (toProto a) (toProto b)
  fromProto (P.RLWESampleCont a b) = RLWESampleCont (fromProto a) (fromProto b)

instance (Protoable (RLWESampleCont t m zq rq), Mod zq, Fact m)
         => Protoable (RLWEInstanceCont Double t m zq rq) where

  type ProtoType (RLWEInstanceCont Double t m zq rq) = P.RLWEInstanceCont

  toProto (RLWEInstanceCont idx v bound samples) =
    P.RLWEInstanceCont idx
    (fromIntegral (proxy valueFact (Proxy::Proxy m)))
    (fromIntegral (proxy modulus (Proxy::Proxy zq)))
    v
    bound
    (S.fromList $ toProto <$> samples)

  fromProto =
    let mval = fromIntegral $ proxy valueFact (Proxy::Proxy m)
        qval = fromIntegral $ proxy modulus (Proxy::Proxy zq)
    in \ (P.RLWEInstanceCont idx m q v bound samples) ->
         if (m == mval) && (q == qval)
         then RLWEInstanceCont idx v bound $ fromProto <$> S.toList samples
         else error "RLWEInstanceCont ProtoType: m or q values don't match"
