{-# LANGUAGE ConstraintKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs,
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
--import qualified Crypto.Lol.Types.Proto.ChallSecrets as P
--import qualified Crypto.Lol.Types.Proto.LWEChallenge as P
import qualified Crypto.Lol.Types.Proto.LWEInstance as P
import qualified Crypto.Lol.Types.Proto.LWESample as P
import qualified Crypto.Lol.Types.Proto.LWESecret as P

import Data.Foldable as S (toList)
import Data.Map.Strict as M hiding (map)
import qualified Data.Map.Strict as M
import Data.Reflection
import Data.Serialize
import Data.Sequence as S (fromList, Seq)

import GHC.Generics (Generic)



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
  fromProto (P.LWESecret idx m s, P.LWEInstance idx' m' p v inst) | (idx == idx') && (m == m') = 
    reify (fromIntegral p :: Int64) (\(_::Proxy p) -> 
      reifyFactI (fromIntegral m) (\(_::proxy m) -> 
        InstanceWithSecret (fromIntegral idx) 
                           (fromIntegral m)
                           (fromIntegral p)
                           v 
                           (fromProto s) 
                           (fromProtoSeq inst :: [LWESample CT m (ZqBasic p Int64)])))



-- for typed reading. To read serialized data, you must know the parameters *in advance*.

toProtoSeq :: (Protoable a) => [a] -> Seq (ProtoType a)
toProtoSeq = S.fromList . map toProto

fromProtoSeq :: (Protoable a) => Seq (ProtoType a) -> [a]
fromProtoSeq = map fromProto . S.toList

-- corresponds to LWESecret proto type
data LWESecret t m z = LWESecret Int (Cyc t m z) deriving (Generic)
deriving instance (Read (Cyc t m z)) => Read (LWESecret t m z)
deriving instance (Show (Cyc t m z)) => Show (LWESecret t m z)
deriving instance (Eq (Cyc t m z)) => Eq (LWESecret t m z)
instance (NFData (Cyc t m z)) => NFData (LWESecret t m z) where
  rnf (LWESecret idx s) = (rnf idx) `seq` (rnf s)
instance (Serialize (Cyc t m z)) => Serialize (LWESecret t m z) -- use Generics
instance (Protoable (Cyc t m z), Fact m) => Protoable (LWESecret t m z) where
  type ProtoType (LWESecret t m z) = P.LWESecret
  toProto (LWESecret idx s) = 
    P.LWESecret (fromIntegral idx) (fromIntegral $ proxy valueFact (Proxy::Proxy m)) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    LWESecret (fromIntegral idx) $ fromProto s

-- corresponds to LWEInstance proto type
data LWEInstance v t m zp = LWEInstance Int v [LWESample t m zp] deriving (Generic)
instance (NFData (LWESample t m zp), NFData v) => NFData (LWEInstance v t m zp) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss)
deriving instance (Read (LWESample t m zp), Read v) => Read (LWEInstance v t m zp)
deriving instance (Show (LWESample t m zp), Show v) => Show (LWEInstance v t m zp)
deriving instance (Eq (LWESample t m zp), Eq v) => Eq (LWEInstance v t m zp)
instance (Serialize (LWESample t m zp), Serialize v) => Serialize (LWEInstance v t m zp) -- use Generics
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
data LWESample t m r = LWESample (Cyc t m r) (Cyc t m r) deriving (Generic)
deriving instance (Read (Cyc t m r)) => Read (LWESample t m r)
deriving instance (Show (Cyc t m r)) => Show (LWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (LWESample t m r)
instance (NFData (Cyc t m r)) => NFData (LWESample t m r) where
  rnf (LWESample a b) = (rnf a) `seq` (rnf b)
instance (Serialize (Cyc t m r)) => Serialize (LWESample t m r) -- use Generics
instance (Protoable (Cyc t m r)) => Protoable (LWESample t m r) where
  type ProtoType (LWESample t m r) = P.LWESample
  toProto (LWESample a b) = P.LWESample (toProto a) (toProto b)
  fromProto (P.LWESample a b) = LWESample (fromProto a) (fromProto b)

{-
data Secret t where
  Secret :: (UntypedConstraints t m z)
         => Int -> Int -> Cyc t m z -> Secret t
deriving instance Show (Secret t)
instance NFData (Secret t) where
  rnf (Secret idx m sk) = (rnf idx) `seq` (rnf m) `seq` (rnf sk)
instance Protoable (Secret RT) where
  type ProtoType (Secret RT) = P.LWESecret
  toProto (Secret idx m (s :: Cyc RT m z)) = 
    P.LWESecret (fromIntegral idx) (fromIntegral m) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    reifyFactI (fromIntegral m) (\(_::proxy m) -> 
      Secret (fromIntegral idx) (fromIntegral m) (fromProto s :: Cyc RT m Int64))
instance Protoable (Secret CT) where
  type ProtoType (Secret CT) = P.LWESecret
  toProto (Secret idx m (s :: Cyc CT m z)) = 
    P.LWESecret (fromIntegral idx) (fromIntegral m) $ toProto s
  fromProto (P.LWESecret idx m s) = 
    reifyFactI (fromIntegral m) (\(_::proxy m) -> 
      Secret (fromIntegral idx) (fromIntegral m) (fromProto s :: Cyc CT m Int64))

data Instance t where
  Instance :: (UntypedConstraints t m zp, Mod zp, ModRep zp ~ Int64) 
           => Int -> Int -> Int -> Double -> [LWESample t m zp] -> Instance t
deriving instance Show (Instance t)
instance NFData (Instance t) where
  rnf (Instance idx m p v samples) = (rnf idx) `seq` (rnf m) `seq` (rnf p) `seq` (rnf v) `seq` (rnf samples)
instance Protoable (Instance RT) where
  type ProtoType (Instance RT) = P.LWEInstance
  toProto (Instance idx m p v (samples :: [LWESample RT m zp])) = 
    P.LWEInstance (fromIntegral idx)
                  (fromIntegral m)
                  (fromIntegral p)
                  v
                  (toProtoSeq samples)
  fromProto (P.LWEInstance idx m p v inst) =
    reify (fromIntegral p :: Int64) (\(_::Proxy p) -> 
      reifyFactI (fromIntegral m) (\(_::proxy m) -> 
        Instance (fromIntegral idx) 
                 (fromIntegral m) 
                 (fromIntegral p) 
                 v 
                 (fromProtoSeq inst :: [LWESample RT m (ZqBasic p Int64)])))
instance Protoable (Instance CT) where
  type ProtoType (Instance CT) = P.LWEInstance
  toProto (Instance idx m p v (samples :: [LWESample CT m zp])) = 
    P.LWEInstance (fromIntegral idx)
                  (fromIntegral m)
                  (fromIntegral p)
                  v
                  (toProtoSeq samples)
  fromProto (P.LWEInstance idx m p v inst) =
    reify (fromIntegral p :: Int64) (\(_::Proxy p) -> 
      reifyFactI (fromIntegral m) (\(_::proxy m) -> 
        Instance (fromIntegral idx) 
                 (fromIntegral m) 
                 (fromIntegral p) 
                 v 
                 (fromProtoSeq inst :: [LWESample CT m (ZqBasic p Int64)])))
-}

{-
-- beaconTime, bitOffset, svar, insts
data LWEChallenge v t m zp = LWEChallenge BeaconPos v (Map Int (LWEInstance t m zp)) deriving (Generic)
newtype LWEInstance t m zp = LWEInstance {unLWEInst :: [LWESample t m zp]} deriving (Generic)
instance (NFData v, NFData (LWEInstance t m zp)) => NFData (LWEChallenge v t m zp) where
  rnf (LWEChallenge bp v insts) = (rnf bp) `seq` (rnf v) `seq` (rnf insts) `seq` ()
deriving instance (Read v, Read (LWEInstance t m zp)) => Read (LWEChallenge v t m zp)
deriving instance (Show v, Show (LWEInstance t m zp)) => Show (LWEChallenge v t m zp)
deriving instance (Eq v, Eq (LWEInstance t m zp)) => Eq (LWEChallenge v t m zp)
instance (Serialize (LWEInstance t m zp), Serialize v) => Serialize (LWEChallenge v t m zp) -- use Generics
instance (Protoable (Challenge Double t m zp)) => Protoable (LWEChallenge Double t m zp) where
  type ProtoType (LWEChallenge Double t m zp) = P.LWEChallenge
  toProto (LWEChallenge (BP beaconTime bitOffset) v instMap) = toProto $
    Challenge beaconTime bitOffset v $ map Instance $ M.toList $ M.map unLWEInst instMap
  fromProto x = 
    let (Challenge beaconTime bitOffset v insts) = fromProto x
    in LWEChallenge (BP beaconTime bitOffset) v $ M.map LWEInstance $ M.fromList $ map unInst insts

-- corresponds to LWEChallenge proto type
-- beaconTime, bitOffset, svar, insts
data Challenge v t m zp = Challenge Int Int v [Instance t m zp] deriving (Generic)
instance (NFData v, NFData (Instance t m zp)) => NFData (Challenge v t m zp) where
  rnf (Challenge a b v insts) = (rnf a) `seq` (rnf b) `seq` (rnf v) `seq` (rnf insts) `seq` ()
deriving instance (Read v, Read (Instance t m zp)) => Read (Challenge v t m zp)
deriving instance (Show v, Show (Instance t m zp)) => Show (Challenge v t m zp)
deriving instance (Eq v, Eq (Instance t m zp)) => Eq (Challenge v t m zp)
instance (Serialize (Instance t m zp), Serialize v) => Serialize (Challenge v t m zp) -- use Generics
instance (Protoable (Cyc t m zp)) => Protoable (Challenge Double t m zp) where
  type ProtoType (Challenge Double t m zp) = P.LWEChallenge
  toProto (Challenge beaconTime bitOffset v instances) = 
    P.LWEChallenge (fromIntegral beaconTime) (fromIntegral bitOffset) v $ S.fromList $ map toProto instances
  fromProto (P.LWEChallenge beaconTime bitOffset v instances) = 
    Challenge (fromIntegral beaconTime) (fromIntegral bitOffset) v $ map fromProto $ S.toList instances
-}

{-
newtype ChallengeSecrets t m z = ChallengeSecrets (Map Int (Cyc t m z)) deriving Generic
deriving instance (Read (Cyc t m z)) => Read (ChallengeSecrets t m z)
deriving instance (Show (Cyc t m z)) => Show (ChallengeSecrets t m z)
deriving instance (Eq (Cyc t m z)) => Eq (ChallengeSecrets t m z)
instance (NFData (Cyc t m z)) => NFData (ChallengeSecrets t m z) where
  rnf (ChallengeSecrets secs) = rnf secs
instance (Serialize (Cyc t m z)) => Serialize (ChallengeSecrets t m z) -- use Generics
instance (Protoable (ChallengeSecrets' t m z)) => Protoable (ChallengeSecrets t m z) where
  type ProtoType (ChallengeSecrets t m z) = P.ChallSecrets
  toProto (ChallengeSecrets secretMap) = 
    toProto $ ChallSecrets' $ map InstSecret $ M.toList secretMap
  fromProto x = 
    let (ChallSecrets' instSecrets) = fromProto x
    in ChallengeSecrets $ M.fromList $ map unInstSecret instSecrets

newtype ChallengeSecrets' t m z = ChallSecrets' [LWESecret t m z] deriving (Generic)
deriving instance (Read (Cyc t m z)) => Read (ChallengeSecrets' t m z)
deriving instance (Show (Cyc t m z)) => Show (ChallengeSecrets' t m z)
deriving instance (Eq (Cyc t m z)) => Eq (ChallengeSecrets' t m z)
instance (NFData (Cyc t m z)) => NFData (ChallengeSecrets' t m z) where
  rnf (ChallSecrets' secs) = rnf secs
instance (Serialize (Cyc t m z)) => Serialize (ChallengeSecrets' t m z) -- use Generics
instance (Protoable (Cyc t m z)) => Protoable (ChallengeSecrets' t m z) where
  type ProtoType (ChallengeSecrets' t m z) = P.ChallSecrets
  toProto (ChallSecrets' ss) = 
    P.ChallSecrets $ S.fromList $ map toProto ss
  fromProto (P.ChallSecrets ss) = 
    ChallSecrets' $ map fromProto $ S.toList ss
-}