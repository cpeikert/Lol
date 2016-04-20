{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Crypto.Challenges.LWR.Gen
( rlwrInstance
, module Crypto.Challenges.LWR.Proto
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Challenges.LWR.Proto
import Crypto.Lol

-- | Generate an LWR instance with a random secret.
rlwrInstance :: (LWRCtx t m z zq zp, MonadRandom rnd, Random z)
  => Int -> rnd (Cyc t m z, [RLWRSample t m zq zp])
rlwrInstance numSamples = do
  s <- getRandom
  samples <- replicateM numSamples $ rlwrSample s
  return (s, samples)

-- | An LWR sample for a given secret.
rlwrSample :: (MonadRandom rnd, LWRCtx t m z zq zp)
          => Cyc t m z -> rnd (RLWRSample t m zq zp)
rlwrSample s = do
  let sq = reduce s
  a <- getRandom
  return $ (a, rescaleCyc Dec $ a * sq)

type LWRCtx t m  z zq zp = 
  (Reduce z zq, Ring zq, Random zq, RescaleCyc (Cyc t) zq zp, Fact m, CElt t z, CElt t zq)
