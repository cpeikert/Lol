{-# LANGUAGE ConstraintKinds, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, TypeFamilies, ScopedTypeVariables #-}

module Challenges.ContinuousLWE 
(lweInstance
,LWECtx
,module Challenges.ProtoReader) where

import Challenges.ProtoReader

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, Random, getRandom)

import Crypto.Lol
import Crypto.Lol.Cyclotomic.UCyc as U hiding (tGaussian)

-- | Generate an LWE instance with a random secret, and the given scaled variance.
lweInstance :: forall t m z zp v rnd rp .  
  (LWECtx t m z zp v rp, MonadRandom rnd, Random z)
  => v -> Int -> TaggedT zp rnd (Cyc t m z, [LWESample t m rp])
lweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (s, samples)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)

-- EAC: Would like to enforce the same modulus for Z_q and RR_q: how?
lweSample :: forall rnd t m z zp v rp . 
  (MonadRandom rnd, LWECtx t m z zp v rp)
  => v -> Cyc t m z -> TaggedT zp rnd (LWESample t m rp)
lweSample svar s = do
  let sq = reduce s :: Cyc t m zp
  e <- tGaussian svar
  a <- tagT $ adviseCRT <$> getRandom -- want entire hint to be in CRT form
  let asq = a * sq
      as = cycPow $ fmapPow fromIntegral $ (lift $ uncycPow asq) :: Cyc t m (LiftOf rp)
      a' = cycPow $ fmapPow fromIntegral $ (lift $ uncycPow a) :: Cyc t m (LiftOf rp)
  return $ LWESample (reduce a') $ reduce $ as + e

type LWECtx t m z zp v rp = 
  (ToInteger z, Reduce z zp, Ring zp, Random zp, Fact m, CElt t z, CElt t zp, 
   ToRational v,
   OrdFloat (LiftOf rp), Random (LiftOf rp), RealField (LiftOf rp),
   Lift' zp, ToInteger (LiftOf zp), 
   Reduce (LiftOf rp) rp, CElt t (LiftOf rp), CElt t rp, CElt t (LiftOf zp))
