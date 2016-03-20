{-# LANGUAGE ConstraintKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, 
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module Challenges.LWE where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random hiding (fromList)

import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc as U hiding (errorRounded)
import Crypto.Lol.Types.Proto
import qualified Crypto.Lol.Types.Proto.ChallSecrets as P
import qualified Crypto.Lol.Types.Proto.InstSecret as P
import qualified Crypto.Lol.Types.Proto.LWEChallenge as P
import qualified Crypto.Lol.Types.Proto.LWEInstance as P
import qualified Crypto.Lol.Types.Proto.LWESample as P

import Data.Foldable (toList)
import Data.Serialize
import Data.Sequence (fromList)

import GHC.Generics (Generic)

removeSecrets :: SecretLWEChallenge v t m zp -> Int -> Int -> (ChallengeSecrets t m zp, LWEChallenge v t m zp)
removeSecrets (SLWEChallenge v sinsts) time offset = 
  let (sks, samples) = unzip $ map (\(SLWEInstance sk inst) -> (sk, inst)) sinsts
      sks' = zipWith InstSecret [1..] sks
      insts = zipWith LWEInstance [1..] samples
  in (ChallSecrets sks', LWEChallenge time offset v insts)

lweChallenge :: (LWECtx t m (LiftOf zp) zp v q, MonadRandom rnd, Random (LiftOf zp)) 
             => v -> Int -> Int -> TaggedT q rnd (SecretLWEChallenge v t m zp)
lweChallenge svar numSamples numInstances = do
  insts <- replicateM numInstances $ lweInstance svar numSamples
  return $ SLWEChallenge svar insts

lweInstance :: forall t m q zp v rnd . 
  (LWECtx t m (LiftOf zp) zp v q, MonadRandom rnd, Random (LiftOf zp))
  => v -> Int -> TaggedT q rnd (SecretLWEInstance t m zp)
lweInstance svar numSamples = do
  s :: Cyc t m (LiftOf zp) <- getRandom
  SLWEInstance s <$> replicateM numSamples (lweSample svar s)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: (MonadRandom rnd, LWECtx t m z zp v q)
          => v -> Cyc t m z -> TaggedT q rnd (LWESample t m zp)
lweSample svar s =
  let sq = reduce s
  in do
    e <- errorRounded svar
    a <- tagT $ adviseCRT <$> getRandom -- want entire hint to be in CRT form
    return $ LWESample a $ a * sq + reduce (e `asTypeOf` s)

errorRounded :: forall v rnd t m z q .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd, 
                 OrdFloat q, Random q, TElt t q, RealField q)
                => v -> TaggedT q rnd (Cyc t m z)
errorRounded svar = tagT $ cycDec <$>
  U.fmapDec (roundMult one) <$> (U.tGaussian svar :: rnd (UCyc t m D q))

type LWECtx t m z zp v q = 
  (ToInteger z, Reduce z zp, Ring zp, Random zp, Fact m, CElt t z, CElt t zp, 
   ToRational v,
   OrdFloat q, Random q, TElt t q, RealField q)

data ChallengeSecrets t m zp = ChallSecrets [InstanceSecret t m zp] deriving (Generic)
deriving instance (Read (Cyc t m (LiftOf zp))) => Read (ChallengeSecrets t m zp)
deriving instance (Show (Cyc t m (LiftOf zp))) => Show (ChallengeSecrets t m zp)
deriving instance (Eq (Cyc t m (LiftOf zp))) => Eq (ChallengeSecrets t m zp)
instance (Serialize (Cyc t m (LiftOf zp))) => Serialize (ChallengeSecrets t m zp) -- use Generics
instance (Protoable (Cyc t m (LiftOf zp))) => Protoable (ChallengeSecrets t m zp) where
  type ProtoType (ChallengeSecrets t m zp) = P.ChallSecrets
  toProto (ChallSecrets ss) = 
    P.ChallSecrets $ fromList $ map toProto ss
  fromProto (P.ChallSecrets ss) = 
    ChallSecrets $ map fromProto $ toList ss

data InstanceSecret t m zp = InstSecret Int (Cyc t m (LiftOf zp)) deriving (Generic)
deriving instance (Read (Cyc t m (LiftOf zp))) => Read (InstanceSecret t m zp)
deriving instance (Show (Cyc t m (LiftOf zp))) => Show (InstanceSecret t m zp)
deriving instance (Eq (Cyc t m (LiftOf zp))) => Eq (InstanceSecret t m zp)
instance (Serialize (Cyc t m (LiftOf zp))) => Serialize (InstanceSecret t m zp) -- use Generics
instance (Protoable (Cyc t m (LiftOf zp))) => Protoable (InstanceSecret t m zp) where
  type ProtoType (InstanceSecret t m zp) = P.InstSecret
  toProto (InstSecret id s) = 
    P.InstSecret (fromIntegral id) $ toProto s
  fromProto (P.InstSecret id s) = 
    InstSecret (fromIntegral id) $ fromProto s

data LWEChallenge v t m zp = LWEChallenge Int Int v [LWEInstance t m zp] deriving (Generic)
data SecretLWEChallenge v t m zp = SLWEChallenge v [SecretLWEInstance t m zp]
instance (NFData v, NFData (SecretLWEInstance t m zp)) => NFData (SecretLWEChallenge v t m zp) where
  rnf (SLWEChallenge v insts) = (rnf v) `seq` (rnf insts) `seq` ()
deriving instance (Read v, Read (LWEInstance t m zp)) => Read (LWEChallenge v t m zp)
deriving instance (Show v, Show (LWEInstance t m zp)) => Show (LWEChallenge v t m zp)
deriving instance (Eq v, Eq (LWEInstance t m zp)) => Eq (LWEChallenge v t m zp)
instance (Serialize (LWEInstance t m zp), Serialize v) => Serialize (LWEChallenge v t m zp) -- use Generics
instance (Protoable (Cyc t m zp)) => Protoable (LWEChallenge Double t m zp) where
  type ProtoType (LWEChallenge Double t m zp) = P.LWEChallenge
  toProto (LWEChallenge beaconTime bitOffset v instances) = 
    P.LWEChallenge (fromIntegral beaconTime) (fromIntegral bitOffset) v $ fromList $ map toProto instances
  fromProto (P.LWEChallenge beaconTime bitOffset v instances) = 
    LWEChallenge (fromIntegral beaconTime) (fromIntegral bitOffset) v $ map fromProto $ toList instances


data LWEInstance t m zp = LWEInstance Int [LWESample t m zp] deriving (Generic)
data SecretLWEInstance t m zp = SLWEInstance (Cyc t m (LiftOf zp)) [LWESample t m zp]
instance (NFData (Cyc t m (LiftOf zp)), NFData (LWESample t m zp)) => NFData (SecretLWEInstance t m zp) where
  rnf (SLWEInstance s ss) = (rnf s) `seq` (rnf ss) `seq` ()
deriving instance (Read (LWESample t m zp)) => Read (LWEInstance t m zp)
deriving instance (Show (LWESample t m zp)) => Show (LWEInstance t m zp)
deriving instance (Eq (LWESample t m zp)) => Eq (LWEInstance t m zp)
instance (Serialize (LWESample t m zp)) => Serialize (LWEInstance t m zp) -- use Generics
instance (Protoable (Cyc t m zp)) => Protoable (LWEInstance t m zp) where
  type ProtoType (LWEInstance t m zp) = P.LWEInstance
  toProto (LWEInstance id samples) = P.LWEInstance (fromIntegral id) $ fromList $ map toProto samples
  fromProto (P.LWEInstance id samples) = LWEInstance (fromIntegral id) $ map fromProto $ toList samples


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
