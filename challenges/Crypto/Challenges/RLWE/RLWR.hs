{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, ScopedTypeVariables #-}

module Crypto.Challenges.RLWE.RLWR where

import Crypto.Lol

import Control.Monad
import Control.Monad.Random

-- | An RLWR sample @(a,b) \in R_q \times R_p@.
type Sample t m zq zp = (Cyc t m zq, Cyc t m zp)

type RLWRCtx t m zq zp =
  (Fact m, Ring zq, RescaleCyc (Cyc t) zq zp, CElt t zq, CElt t zp)

-- | Generate a discrete RLWR instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceN :: (RLWRCtx t m zq zp, Random zq, MonadRandom rnd)
  => Int -> rnd (Cyc t m zq, [Sample t m zq zp])
instanceN num = do
  s <- getRandom
  samples <- replicateM num $ sample s
  return (s, samples)

-- | The @b@ component of an RLWR sample for secret @s@ and given @a@.
bRLWR :: (RLWRCtx t m zq zp) => Cyc t m zq -> Cyc t m zq -> Cyc t m zp
bRLWR s a = rescaleCyc Dec $ s * a

-- | An RLWR sample with the given secret.
sample :: (RLWRCtx t m zq zp, Random zq, MonadRandom rnd)
          => Cyc t m zq -> rnd (Sample t m zq zp)
sample s = let s' = adviseCRT s in do
  a <- getRandom
  return (a, bRLWR s' a)

-- | Test if the given RLWR instance is valid for the given secret.
validInstance :: (RLWRCtx t m zq zp, Eq zp)
  => Cyc t m zq -> [Sample t m zq zp] -> Bool
validInstance s = let s' = adviseCRT s in all (validSample s')

-- | Test if the given RLWR sample is valid for the given secret.
validSample :: (RLWRCtx t m zq zp, Eq zp)
  => Cyc t m zq -> Sample t m zq zp -> Bool
validSample s (a,b) = b == bRLWR s a

