{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.UProtoReader
(LWEInstance(..)
,LWESample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact, Factored)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types.Proto
import Challenges.ProtoReader (LWESecret(..))
import qualified Challenges.Proto.LWEInstance as P
import qualified Challenges.Proto.LWESample as P
import qualified Challenges.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection hiding (D)
import Data.Sequence as S (fromList, Seq)

-- | Corresponds to LWEInstance proto type.
data LWEInstance v t m zq rq = LWEInstance Int v [LWESample t m zq rq]
instance (NFData (LWESample t m zq rq), NFData v) => NFData (LWEInstance v t m zq rq) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss)
deriving instance (Read (LWESample t m zq rq), Read v) => Read (LWEInstance v t m zq rq)
deriving instance (Show (LWESample t m zq rq), Show v) => Show (LWEInstance v t m zq rq)
deriving instance (Eq (LWESample t m zq rq), Eq v) => Eq (LWEInstance v t m zq rq)
instance (Protoable (LWESample t m zq rq), Mod zq, ModRep zq ~ Int64, Fact m)
  => Protoable (LWEInstance Double t m zq rq) where
  type ProtoType (LWEInstance Double t m zq rq) = P.LWEInstance
  toProto (LWEInstance idx v samples) = 
    P.LWEInstance (fromIntegral idx) 
                  (fromIntegral (proxy valueFact (Proxy::Proxy m)))
                  (fromIntegral (proxy modulus (Proxy::Proxy zq)))
                  v
                  (S.fromList $ map toProto samples)
  fromProto (P.LWEInstance idx m q v samples) = LWEInstance (fromIntegral idx) v $ map fromProto $ S.toList samples

-- | Corresponds to LWESample proto type.
data LWESample t m zq rq = LWESample (Cyc t m zq) (UCyc t m D rq)
deriving instance (Read (Cyc t m zq), Read (UCyc t m D rq)) => Read (LWESample t m zq rq)
deriving instance (Show (Cyc t m zq), Show (UCyc t m D rq)) => Show (LWESample t m zq rq)
deriving instance (Eq (Cyc t m zq), Eq (UCyc t m D rq)) => Eq (LWESample t m zq rq)
instance (NFData (Cyc t m zq), NFData (UCyc t m D rq)) => NFData (LWESample t m zq rq) where
  rnf (LWESample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (Cyc t m zq) , Protoable (UCyc t m D rq)) 
  => Protoable (LWESample t m zq rq) where
  type ProtoType (LWESample t m zq rq) = P.LWESample
  toProto (LWESample a b) = P.LWESample (toProto a) (toProto b)
  fromProto (P.LWESample a b) = LWESample (fromProto a) (fromProto b)
