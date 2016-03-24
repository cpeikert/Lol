{-# LANGUAGE DeriveGeneric, FlexibleContexts, FlexibleInstances, GADTs,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module Challenges.ProtoReader (LWEInstance(..), LWESample(..), LWESecret(..)) where

import Control.DeepSeq

import Crypto.Lol (Cyc, LiftOf)
import Crypto.Lol.Types.Proto
--import qualified Crypto.Lol.Types.Proto.ChallSecrets as P
import qualified Crypto.Lol.Types.Proto.InstSecret as P
--import qualified Crypto.Lol.Types.Proto.LWEChallenge as P
import qualified Crypto.Lol.Types.Proto.LWEInstance as P
import qualified Crypto.Lol.Types.Proto.LWESample as P

import Data.Foldable as S (toList)
import Data.Map.Strict as M hiding (map)
import qualified Data.Map.Strict as M
import Data.Serialize
import Data.Sequence as S (fromList)

import GHC.Generics (Generic)



-- corresponds to InstSecret proto type
data LWESecret t m z = LWESecret Int (Cyc t m z) deriving (Generic)
deriving instance (Read (Cyc t m z)) => Read (LWESecret t m z)
deriving instance (Show (Cyc t m z)) => Show (LWESecret t m z)
deriving instance (Eq (Cyc t m z)) => Eq (LWESecret t m z)
instance (NFData (Cyc t m z)) => NFData (LWESecret t m z) where
  rnf (LWESecret idx s) = (rnf idx) `seq` (rnf s) `seq` ()
instance (Serialize (Cyc t m z)) => Serialize (LWESecret t m z) -- use Generics
instance (Protoable (Cyc t m z)) => Protoable (LWESecret t m z) where
  type ProtoType (LWESecret t m z) = P.InstSecret
  toProto (LWESecret idx s) = 
    P.InstSecret (fromIntegral idx) $ toProto s
  fromProto (P.InstSecret idx s) = 
    LWESecret (fromIntegral idx) $ fromProto s

-- corresponds to LWEInstance proto type
data LWEInstance v t m zp = LWEInstance Int v [LWESample t m zp] deriving (Generic)
instance (NFData (LWESample t m zp), NFData v) => NFData (LWEInstance v t m zp) where
  rnf (LWEInstance idx v ss) = (rnf idx) `seq` (rnf v) `seq` (rnf ss) `seq` ()
deriving instance (Read (LWESample t m zp), Read v) => Read (LWEInstance v t m zp)
deriving instance (Show (LWESample t m zp), Show v) => Show (LWEInstance v t m zp)
deriving instance (Eq (LWESample t m zp), Eq v) => Eq (LWEInstance v t m zp)
instance (Serialize (LWESample t m zp), Serialize v) => Serialize (LWEInstance v t m zp) -- use Generics
instance (Protoable (Cyc t m zp)) => Protoable (LWEInstance Double t m zp) where
  type ProtoType (LWEInstance Double t m zp) = P.LWEInstance
  toProto (LWEInstance idx v samples) = P.LWEInstance (fromIntegral idx) v $ S.fromList $ map toProto samples
  fromProto (P.LWEInstance idx v samples) = LWEInstance (fromIntegral idx) v $ map fromProto $ S.toList samples

-- corresponds to LWESample proto type
data LWESample t m r = LWESample (Cyc t m r) (Cyc t m r) deriving (Generic)
deriving instance (Read (Cyc t m r)) => Read (LWESample t m r)
deriving instance (Show (Cyc t m r)) => Show (LWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (LWESample t m r)
instance (NFData (Cyc t m r)) => NFData (LWESample t m r) where
  rnf (LWESample a b) = (rnf a) `seq` (rnf b) `seq` ()
instance (Serialize (Cyc t m r)) => Serialize (LWESample t m r) -- use Generics
instance (Protoable (Cyc t m r)) => Protoable (LWESample t m r) where
  type ProtoType (LWESample t m r) = P.LWESample
  toProto (LWESample a b) = P.LWESample (toProto a) (toProto b)
  fromProto (P.LWESample a b) = LWESample (fromProto a) (fromProto b)

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