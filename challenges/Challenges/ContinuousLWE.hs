{-# LANGUAGE ConstraintKinds, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, TypeFamilies, ScopedTypeVariables #-}

module Challenges.ContinuousLWE 
(lweInstance
,ULWECtx
,module Challenges.UProtoReader) where

import Challenges.UProtoReader

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, Random, getRandom)

import Crypto.Lol hiding (tGaussian)
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Generate an LWE instance with a random secret, and the given scaled variance.
lweInstance :: forall t m z zp v rnd rp .  
  (ULWECtx t m z zp v rp, MonadRandom rnd, Random z)
  => v -> Int -> TaggedT zp rnd (UCyc t m P z, [LWESample t m rp])
lweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (uncycPow s, samples)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)

-- EAC: Would like to enforce the same modulus for Z_q and RR_q: how?
lweSample :: forall rnd t m z zp v rp . 
  (MonadRandom rnd, ULWECtx t m z zp v rp)
  => v -> Cyc t m z -> TaggedT zp rnd (LWESample t m rp)
lweSample svar s = do
  let sq = reduce s :: Cyc t m zp
  e <- tGaussian svar
  a <- tagT getRandom
  let as = fmap fromIntegral $ lift $ uncycDec $ a * sq
      aq = reduce $ (fmap fromIntegral $ lift $ uncycDec a :: UCyc t m D (LiftOf rp))
  return $ LWESample aq $ reduce $ as + (e :: UCyc t m D (LiftOf rp))

type ULWECtx t m z zp v rp = 
  (ToInteger z, Reduce z zp, Ring zp, Random zp, Fact m, CElt t z, CElt t zp, 
   ToRational v, ToInteger (LiftOf zp),
   OrdFloat (LiftOf rp), Random (LiftOf rp), RealField (LiftOf rp), Lift' zp,
   Reduce (LiftOf rp) rp, CElt t (LiftOf rp), CElt t (LiftOf zp),
   TElt t rp, ZeroTestable rp)
