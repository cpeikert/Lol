{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, ScopedTypeVariables #-}

module Crypto.Challenges.RLWE.Continuous where

import Crypto.Lol                   hiding (gSqNorm, tGaussian)
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Cyclotomic.UCyc

import Control.Monad
import Control.Monad.Random

-- | A continuous RLWE sample @(a,b) \in R_q \times K/qR@.
type Sample t m zq rrq = (Cyc t m zq, UCyc t m D rrq)

type RLWECtx t m zq rrq =
  (Fact m, Ring zq, CElt t zq, Subgroup zq rrq,
   Lift' rrq, OrdFloat (LiftOf rrq), TElt t rrq, TElt t (LiftOf rrq))

-- | Generate a continuous RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceN :: (RLWECtx t m zq rrq, Random zq, Random (LiftOf rrq),
              MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [Sample t m zq rrq])
instanceN svar num = do
  s <- getRandom
  samples <- replicateM num $ sample svar s
  return (s, samples)

-- | A continuous RLWE sample with the given scaled variance and
-- secret.
sample :: forall rnd v t m zq rrq .
  (RLWECtx t m zq rrq, Random zq, Random (LiftOf rrq),
   MonadRandom rnd, ToRational v)
  => v -> Cyc t m zq -> rnd (Sample t m zq rrq)
sample svar s = do
  a <- getRandom
  e :: UCyc t m D (LiftOf rrq) <- tGaussian svar
  let as = fmapDec fromSubgroup $ uncycDec $ a * s :: UCyc t m D rrq
  return (a, as + reduce e)

-- | Test if the 'gSqNorm' of the error for each RLWE sample in the
-- instance (given the secret) is less than the given bound.
validInstance :: (RLWECtx t m zq rrq)
  => LiftOf rrq -> Cyc t m zq -> [Sample t m zq rrq] -> Bool
validInstance bound s = all (validSample bound s)

-- | Test if the 'gSqNorm' of the error for an RLWE sample (given the
-- secret) is less than the provided bound.
validSample :: (RLWECtx t m zq rrq)
  => LiftOf rrq -> Cyc t m zq -> Sample t m zq rrq -> Bool
validSample bound s (a,b) = err < bound
  where err = let as = fmapDec fromSubgroup $ uncycDec $ a * s
              in gSqNorm $ lift $ b - as

