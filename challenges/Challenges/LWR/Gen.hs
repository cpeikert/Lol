{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Challenges.LWR.Gen
(lwrInstance
,module Challenges.LWR.Proto) where

import Challenges.LWR.Proto

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol

-- | Generate an LWR instance with a random secret.
lwrInstance :: (LWRCtx t m z zq zq', MonadRandom rnd, Random z)
  => Int -> rnd (Cyc t m z, [LWRSample t m zq zq'])
lwrInstance numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lwrSample s)
  return (s, samples)

-- | An LWR sample for a given secret.
lwrSample :: (MonadRandom rnd, LWRCtx t m z zq zq')
          => Cyc t m z -> rnd (LWRSample t m zq zq')
lwrSample s = do
  let sq = reduce s
  a <- adviseCRT <$> getRandom
  return $ LWRSample a $ rescaleCyc Pow $ a * sq

type LWRCtx t m  z zq zq' = 
  (Ring zq, CElt t zq, Fact m, RescaleCyc (Cyc t) zq zq', Random zq, CElt t z, Reduce z zq)