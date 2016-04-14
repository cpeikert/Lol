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
  => v -> Int -> rnd (Cyc t m z, [LWESample t m zp rp])
lweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (s, samples)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: forall rnd t m z zp v rp . 
  (MonadRandom rnd, ULWECtx t m z zp v rp)
  => v -> Cyc t m z -> rnd (LWESample t m zp rp)
lweSample svar s = do
  let sq = reduce s :: Cyc t m zp
  e :: UCyc t m D (LiftOf rp) <- tGaussian svar
  a <- getRandom
  let as = fmap fromSubgroup $ uncycDec $ a * sq :: UCyc t m D rp
  return $ LWESample a $ as + (reduce e)

type ULWECtx t m  z zp v rp = 
  (Reduce z zp, Ring zp, Random zp, Fact m, CElt t z,
   TElt t rp, Reduce (LiftOf rp) rp, OrdFloat (LiftOf rp),
   CElt t (LiftOf rp), ToRational v, Random (LiftOf rp),
   CElt t z, CElt t zp, Reduce z zp, Subgroup zp rp)