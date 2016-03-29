{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, GADTs,
             ScopedTypeVariables,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module Challenges.ProtoReader
(-- type LWE data
 LWEInstance(..)
,LWESample(..)
,LWESecret(..)
 -- untyped LWE data for self-contained reading
,InstanceWithSecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, CElt, LiftOf, proxy, Proxy(..), reifyFactI, valueFact, Int64, Lift', ToInteger, modulus, Mod(..), Fact, CT, RT, ZqBasic)
import Crypto.Lol.Types.Proto
import qualified Crypto.Lol.Types.Proto.LWEInstance as P
import qualified Crypto.Lol.Types.Proto.LWESample as P
import qualified Crypto.Lol.Types.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Reflection
import Data.Sequence as S (fromList, Seq)


-- for untyped, self-contained reading
type UntypedConstraints t m z = 
  (NFData (Cyc t m z), Protoable (Cyc t m z), 
   Fact m, Show (Cyc t m z), CElt t z)

data InstanceWithSecret t where
  InstanceWithSecret :: (UntypedConstraints t m (LiftOf zp), UntypedConstraints t m zp, Lift' zp, ToInteger (LiftOf zp), Mod zp, ModRep zp ~ Int64)
    => Int -> Int -> Int -> Double -> Cyc t m (LiftOf zp) -> [LWESample t m zp] -> InstanceWithSecret t
deriving instance Show (InstanceWithSecret t)
instance NFData (InstanceWithSecret t) where
  rnf (InstanceWithSecret idx m p v secret samples) = (rnf idx) `seq` (rnf m) `seq` (rnf p) `seq` (rnf v) `seq` (rnf secret) `seq` (rnf samples)
instance Protoable (InstanceWithSecret RT) where
  type ProtoType (InstanceWithSecret RT) = (P.LWESecret, P.LWEInstance)
  toProto (InstanceWithSecret idx m p v secret samples) = 
    (toProto $ LWESecret idx secret, toProto $ LWEInstance idx v samples)
  fromProto (P.LWESecret idx m s, P.LWEInstance idx' m' p v inst) | (idx == idx') && (m == m') = 
    reify (fromIntegral p :: Int64) (\(_::Proxy p) -> 
      reifyFactI (fromIntegral m) (\(_::proxy m) -> 
        InstanceWithSecret (fromIntegral idx) 
                           (fromIntegral m)
                           (fromIntegral p)
                           v 
                           (fromProto s) 
                           (fromProtoSeq inst :: [LWESample RT m (ZqBasic p Int64)])))
instance Protoable (InstanceWithSecret CT) where
  type ProtoType (InstanceWithSecret CT) = (P.LWESecret, P.LWEInstance)
  toProto (InstanceWithSecret idx m p v secret samples) = 
    (toProto $ LWESecret idx secret, toProto $ LWEInstance idx v samples)
  fromProto (P.LWESecret idx m s, P.LWEInstance idx' m' p v inst) 
    | (idx == idx') && (m == m') = 
      reify (fromIntegral p :: Int64) (\(_::Proxy p) -> 
        reifyFactI (fromIntegral m) (\(_::proxy m) -> 
          InstanceWithSecret (fromIntegral idx) 
                             (fromIntegral m)
                             (fromIntegral p)
                             v 
                             (fromProto s) 
                             (fromProtoSeq inst :: [LWESample CT m (ZqBasic p Int64)])))
    | idx /= idx' = error $ "ID mismatch when reading serialized instance: instance ID is " ++ 
      (show idx') ++ ", secret ID is " ++ (show idx)
    | m /= m' = error $ "Cyclotomic index mismatch when reading serialzed instance: instance index is " ++ 
      (show m') ++ ", secret index is " ++ (show m)


-- for typed reading. To read serialized data, you must know the parameters *in advance*.

toProtoSeq :: (Protoable a) => [a] -> Seq (ProtoType a)
toProtoSeq = S.fromList . map toProto

fromProtoSeq :: (Protoable a) => Seq (ProtoType a) -> [a]
fromProtoSeq = map fromProto . S.toList

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
data LWEInstance v t m zp = LWEInstance Int v [LWESample t m zp]
instance (NFData (LWESample t m zp), NFData v) => NFData (LWEInstance v t m zp) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss)
deriving instance (Read (LWESample t m zp), Read v) => Read (LWEInstance v t m zp)
deriving instance (Show (LWESample t m zp), Show v) => Show (LWEInstance v t m zp)
deriving instance (Eq (LWESample t m zp), Eq v) => Eq (LWEInstance v t m zp)
instance (Protoable (Cyc t m zp), Mod zp, ModRep zp ~ Int64, Fact m) => Protoable (LWEInstance Double t m zp) where
  type ProtoType (LWEInstance Double t m zp) = P.LWEInstance
  toProto (LWEInstance idx v samples) = 
    P.LWEInstance (fromIntegral idx) 
                  (fromIntegral (proxy valueFact (Proxy::Proxy m)))
                  (fromIntegral (proxy modulus (Proxy::Proxy zp)))
                  v
                  (toProtoSeq samples)
  fromProto (P.LWEInstance idx m p v samples) = LWEInstance (fromIntegral idx) v $ fromProtoSeq samples

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
