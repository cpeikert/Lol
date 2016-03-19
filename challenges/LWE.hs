{-# LANGUAGE ConstraintKinds, DeriveGeneric, FlexibleContexts, FlexibleInstances,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, 
             StandaloneDeriving, TupleSections, TypeFamilies, UndecidableInstances #-}

module LWE where

import TGaussian

import Control.Applicative
import Control.Monad
import Control.Monad.Random hiding (fromList)
import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor
import Data.Foldable (toList)
import Data.Serialize
import Data.Sequence (fromList)
import GHC.Generics (Generic)

import Crypto.Lol.Types.Proto
import qualified Crypto.Lol.Types.Proto.LWEInstance as P
import qualified Crypto.Lol.Types.Proto.LWEPair as P

average xs = 
  let n = length xs
  in (sum xs) / (fromIntegral n)

avgInstErr :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, Algebraic v,
   Lift zp (LiftOf zp), CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => Cyc t m (LiftOf zp) -> LWEInstance v t m zp -> v
avgInstErr sk (LWEInstance v pairs) = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in average $ map (sampleRatio bound sk) pairs


sampleRatio :: forall v t m zp . 
  (Ord v, Absolute (LiftOf zp), Ord (LiftOf zp), Fact m,
   Lift zp (LiftOf zp), CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp), Field v) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleRatio bound sk (LWESample a b) = 
  let e' = b-a*reduce sk :: Cyc t m zp
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in bound / (fromIntegral norm)




checkInstance :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, Algebraic v,
   Lift zp (LiftOf zp), CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => Cyc t m (LiftOf zp) -> LWEInstance v t m zp -> Bool
checkInstance sk (LWEInstance v pairs) = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all (checkSample bound sk) pairs

checkSample :: forall v t m zp . 
  (Ord v, Absolute (LiftOf zp), Ord (LiftOf zp), Fact m,
   Lift zp (LiftOf zp), CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp), Ring v) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk (LWESample a b) = 
  let e' = b-a*reduce sk :: Cyc t m zp
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm < bound

type LWECtx t m z zp v q = 
  (ToInteger z, Reduce z zp, Ring zp, Random zp, Fact m, CElt t z, CElt t zp, 
   ToRational v,
   OrdFloat q, Random q, TElt t q, RealField q)

data LWEInstance v t m zp = LWEInstance v [LWESample t m zp] deriving (Generic)
deriving instance (Read v, Read (LWESample t m zp)) => Read (LWEInstance v t m zp)
deriving instance (Show v, Show (LWESample t m zp)) => Show (LWEInstance v t m zp)
deriving instance (Eq v, Eq (LWESample t m zp)) => Eq (LWEInstance v t m zp)
instance (Serialize (LWESample t m zp), Serialize v) => Serialize (LWEInstance v t m zp) -- use Generics

instance (Protoable (Cyc t m zp)) => Protoable (LWEInstance Double t m zp) where
  type ProtoType (LWEInstance Double t m zp) = P.LWEInstance
  toProto (LWEInstance v samples) = P.LWEInstance v $ fromList $ map toProto samples
  fromProto (P.LWEInstance v samples) = LWEInstance v $ map fromProto $ toList samples

lweInstance :: forall t m q zp v rnd . 
  (LWECtx t m (LiftOf zp) zp v q, MonadRandom rnd, Random (LiftOf zp))
  => v -> Int -> TaggedT q rnd ((Cyc t m (LiftOf zp), LWEInstance v t m zp))
lweInstance svar numSamples = do
  s :: Cyc t m (LiftOf zp) <- getRandom
  (s,) <$> LWEInstance svar <$> replicateM numSamples (lweSample svar s)

data LWESample t m r = LWESample (Cyc t m r) (Cyc t m r) deriving (Generic)
deriving instance (Read (Cyc t m r)) => Read (LWESample t m r)
deriving instance (Show (Cyc t m r)) => Show (LWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (LWESample t m r)
instance (Serialize (Cyc t m r)) => Serialize (LWESample t m r) -- use Generics

instance (Protoable (Cyc t m r)) => Protoable (LWESample t m r) where
  type ProtoType (LWESample t m r) = P.LWEPair
  toProto (LWESample a b) = P.LWEPair (toProto a) (toProto b)
  fromProto (P.LWEPair a b) = LWESample (fromProto a) (fromProto b)

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