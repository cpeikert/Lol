{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Crypto.Challenges.RLWE.Continuous where

import Crypto.Lol                   hiding (gSqNorm, tGaussian)
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Cyclotomic.UCyc

import Control.Monad
import Control.Monad.Random

import Data.Function (fix)

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

-- | A bound such that the 'gSqNorm' of a continuous error generated
-- by 'tGaussian' with scaled variance @v@ (over the @m@th cyclotomic
-- field) is less than the bound except with probability approximately
-- @eps@.
errorBound :: (Ord v, Transcendental v, Fact m)
              => v              -- ^ the scaled variance
              -> v              -- ^ @eps@
              -> Tagged m v
errorBound v eps = do
  n <- totientFact
  mhat <- valueHatFact
  let d = flip fix (1 / (2*pi)) $ \f x ->
        let x' = (1/2 + log (2 * pi * x)/2 - log eps/fromIntegral n)/pi
        in if x'-x < 0.0001
           then x'
           else f x'
  return $ fromIntegral (mhat*n)*v*d
