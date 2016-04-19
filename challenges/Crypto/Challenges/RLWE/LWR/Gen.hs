{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Challenges.LWR.Gen
( lwrInstance
, module Challenges.LWR.Proto
) where

import Challenges.LWR.Proto

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol

-- | Generate an LWR instance with a random secret.
lwrInstance :: (LWRCtx t m z zq zp, MonadRandom rnd, Random z)
  => Int -> rnd (Cyc t m z, [LWRSample t m zq zp])
lwrInstance numSamples = do
  s <- getRandom
  samples <- replicateM numSamples $ lwrSample s
  return (s, samples)

-- | An LWR sample for a given secret.
lwrSample :: (MonadRandom rnd, LWRCtx t m z zq zp)
          => Cyc t m z -> rnd (LWRSample t m zq zp)
lwrSample s = do
  let sq = reduce s
  a <- getRandom
  return $ LWRSample a $ rescaleCyc Dec $ a * sq

type LWRCtx t m  z zq zp = 
  (Reduce z zq, Ring zq, Random zq, RescaleCyc (Cyc t) zq zp, Fact m, CElt t z, CElt t zq)
