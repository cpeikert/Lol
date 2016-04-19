{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Crypto.Challenges.LWR.Proto
(RLWRInstance(..)
,RLWRSample(..)
,RLWESecret(..)) where

import Control.DeepSeq

import Crypto.Challenges.ContinuousLWE.Proto (RLWESecret(..))
import qualified Crypto.Challenges.Proto.RLWE.RLWRInstance as P
import qualified Crypto.Challenges.Proto.RLWE.RLWRSample as P
import qualified Crypto.Challenges.Proto.RLWE.RLWESecret as P
import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact)
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Rq

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)
import Data.Word

-- | Corresponds to RLWRInstance proto type.
data RLWRInstance t m zq zq' = RLWRInstance Word32 [RLWRSample t m zq zq']
instance (NFData (RLWRSample t m zq zq')) => NFData (RLWRInstance t m zq zq') where
  rnf (RLWRInstance idx ss) = (rnf idx) `seq` (rnf ss)
deriving instance (Show (RLWRSample t m zq zq')) => Show (RLWRInstance t m zq zq')
deriving instance (Eq (RLWRSample t m zq zq')) => Eq (RLWRInstance t m zq zq')
instance (Protoable (RLWRSample t m zq zq'), Mod zq, Mod zq', 
          ModRep zq ~ Int64, ModRep zq' ~ Int64, Fact m) 
  => Protoable (RLWRInstance t m zq zq') where
  type ProtoType (RLWRInstance t m zq zq') = P.RLWRInstance
  toProto (RLWRInstance idx samples) = 
    P.RLWRInstance 
      idx
      (fromIntegral (proxy valueFact (Proxy::Proxy m)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq')))
      (S.fromList $ map toProto samples)
  fromProto = 
    let mval = fromIntegral $ proxy valueFact (Proxy::Proxy m)
        qval = fromIntegral $ proxy modulus (Proxy::Proxy zq)
        q'val = fromIntegral $ proxy modulus (Proxy::Proxy zq')
    in \(P.RLWRInstance idx m q q' samples) ->
      if (m == mval) && (q == qval) && (q' == q'val)
      then RLWRInstance idx $ map fromProto $ S.toList samples
      else error "RLWRInstance ProtoType: m, q, or q' values don't match"

-- | Corresponds to RLWRSample proto type.
data RLWRSample t m zq zq' = RLWRSample (Cyc t m zq) (Cyc t m zq')
instance (Protoable (Cyc t m zq),
          Protoable (Cyc t m zq'),          
          ProtoType (Cyc t m zq) ~ Rq,
          ProtoType (Cyc t m zq') ~ Rq)
  => Protoable (RLWRSample t m zq zq') where
  type ProtoType (RLWRSample t m zq zq') = P.RLWRSample
  toProto (RLWRSample a b) = P.RLWRSample (toProto a) (toProto b)
  fromProto (P.RLWRSample a b) = RLWRSample (fromProto a) (fromProto b)
