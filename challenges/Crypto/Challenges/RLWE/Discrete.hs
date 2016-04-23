{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, ScopedTypeVariables #-}

module Crypto.Challenges.RLWE.Discrete where

import Crypto.Lol

import Control.Monad
import Control.Monad.Random

-- | A discrete RLWE sample @(a,b) \in R_q \times R_q@.
type Sample t m zq = (Cyc t m zq, Cyc t m zq)

type RLWECtx t m zq =
  (Fact m, Ring zq, Lift' zq, ToInteger (LiftOf zq),
   CElt t zq, CElt t (LiftOf zq))

-- | Generate a discrete RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceN :: (RLWECtx t m zq, Random zq, MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [Sample t m zq])
instanceN svar num = do
  s <- getRandom
  samples <- replicateM num $ sample svar s
  return (s, samples)

-- | A discrete RLWE sample with the given scaled variance and
-- secret.
sample :: forall rnd v t m zq .
  (RLWECtx t m zq, Random zq, MonadRandom rnd, ToRational v)
  => v -> Cyc t m zq -> rnd (Sample t m zq)
sample svar s = do
  a <- getRandom
  e :: Cyc t m (LiftOf zq) <- errorRounded svar
  return (a, a * s + reduce e)

-- | Test if the 'gSqNorm' of the error for each RLWE sample in the
-- instance (given the secret) is less than the given bound.
validInstance :: (RLWECtx t m zq)
  => LiftOf zq -> Cyc t m zq -> [Sample t m zq] -> Bool
validInstance bound s = all (validSample bound s)

-- | Test if the 'gSqNorm' of the error for an RLWE sample (given the
-- secret) is less than the provided bound.
validSample :: (RLWECtx t m zq)
  => LiftOf zq -> Cyc t m zq -> Sample t m zq -> Bool
validSample bound s (a,b) = gSqNorm (liftDec $ b - a * s) < bound

-- | Outputs a bound such that the scaled, squared norm of an
-- error term generated with (scaled) variance v
-- will be less than the bound except with probability eps.
-- EAC: This bound might be looser than the continuous version
computeBound :: (Field v, Ord v, Transcendental v, Fact m) => v -> v -> Tagged m Int64
computeBound = error "TODO: Discrete.computeBound"
