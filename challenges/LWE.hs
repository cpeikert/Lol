{-# LANGUAGE ConstraintKinds, DeriveGeneric, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, 
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

module LWE where

import TGaussian

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor
import Data.Serialize
import GHC.Generics (Generic)

checkInstance :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, Algebraic v,
   Lift zp (LiftOf zp), CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => LWEInstance v t m zp -> Bool
checkInstance (LWEInstance v sk pairs) = 
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

data LWEInstance v t m zp = LWEInstance v (Cyc t m (LiftOf zp)) [LWESample t m zp] deriving (Generic)
deriving instance (Read (Cyc t m (LiftOf zp)), Read v, Read (LWESample t m zp)) => Read (LWEInstance v t m zp)
deriving instance (Show (Cyc t m (LiftOf zp)), Show v, Show (LWESample t m zp)) => Show (LWEInstance v t m zp)
deriving instance (Eq (Cyc t m (LiftOf zp)), Eq v, Eq (LWESample t m zp)) => Eq (LWEInstance v t m zp)
instance (Serialize (LWESample t m zp), Serialize (Cyc t m (LiftOf zp)), Serialize v) => Serialize (LWEInstance v t m zp) -- use Generics

lweInstance :: forall t m q zp v rnd . 
  (LWECtx t m (LiftOf zp) zp v q, MonadRandom rnd, Random (LiftOf zp))
  => v -> Int -> TaggedT q rnd (LWEInstance v t m zp)
lweInstance svar numSamples = do
  s :: Cyc t m (LiftOf zp) <- getRandom
  LWEInstance svar s <$> replicateM numSamples (lweSample svar s)

data LWESample t m r = LWESample (Cyc t m r) (Cyc t m r) deriving (Generic)
deriving instance (Read (Cyc t m r)) => Read (LWESample t m r)
deriving instance (Show (Cyc t m r)) => Show (LWESample t m r)
deriving instance (Eq (Cyc t m r)) => Eq (LWESample t m r)
instance (Serialize (Cyc t m r)) => Serialize (LWESample t m r) -- use Generics

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