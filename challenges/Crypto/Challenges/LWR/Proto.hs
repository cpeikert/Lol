{-# LANGUAGE FlexibleInstances, ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Translates unparameterized ProtoTypes into parameterized Haskell types.

module Challenges.LWR.Proto
(LWRInstance(..)
,LWRSample(..)
,LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, proxy, Proxy(..), valueFact, Int64, modulus, Mod(..), Fact)
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Rq
import Challenges.ContinuousLWE.Proto (LWESecret(..))
import qualified Challenges.Proto.LWRInstance as P
import qualified Challenges.Proto.LWRSample as P
import qualified Challenges.Proto.LWESecret as P
import Challenges.Proto.InstType

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)

-- | Corresponds to LWRInstance proto type.
data LWRInstance t m zq zq' = LWRInstance Int [LWRSample t m zq zq']
instance (NFData (LWRSample t m zq zq')) => NFData (LWRInstance t m zq zq') where
  rnf (LWRInstance idx ss) = (rnf idx) `seq` (rnf ss)
deriving instance (Show (LWRSample t m zq zq')) => Show (LWRInstance t m zq zq')
deriving instance (Eq (LWRSample t m zq zq')) => Eq (LWRInstance t m zq zq')
instance (Protoable (LWRSample t m zq zq'), Mod zq, Mod zq', 
          ModRep zq ~ Int64, ModRep zq' ~ Int64, Fact m) 
  => Protoable (LWRInstance t m zq zq') where
  type ProtoType (LWRInstance t m zq zq') = P.LWRInstance
  toProto (LWRInstance idx samples) = 
    P.LWRInstance 
      (fromIntegral idx) 
      (fromIntegral (proxy valueFact (Proxy::Proxy m)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq)))
      (fromIntegral (proxy modulus (Proxy::Proxy zq')))
      (S.fromList $ map toProto samples)
  fromProto (P.LWRInstance idx m q q' samples) = 
    LWRInstance (fromIntegral idx) $ map fromProto $ S.toList samples

-- | Corresponds to LWRSample proto type.
data LWRSample t m zq zq' = LWRSample (Cyc t m zq) (Cyc t m zq')
deriving instance (Show (Cyc t m zq), Show (Cyc t m zq')) => Show (LWRSample t m zq zq')
deriving instance (Eq (Cyc t m zq), Eq (Cyc t m zq')) => Eq (LWRSample t m zq zq')
instance (NFData (Cyc t m zq), NFData (Cyc t m zq')) => NFData (LWRSample t m zq zq') where
  rnf (LWRSample a b) = (rnf a) `seq` (rnf b)
instance (Protoable (Cyc t m zq),
          Protoable (Cyc t m zq'),          
          ProtoType (Cyc t m zq) ~ Rq,
          ProtoType (Cyc t m zq') ~ Rq)
  => Protoable (LWRSample t m zq zq') where
  type ProtoType (LWRSample t m zq zq') = P.LWRSample
  toProto (LWRSample a b) = P.LWRSample (toProto a) (toProto b)
  fromProto (P.LWRSample a b) = LWRSample (fromProto a) (fromProto b)
