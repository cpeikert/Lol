{-# LANGUAGE ConstraintKinds, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, TypeFamilies, ScopedTypeVariables #-}

module Challenges.ContinuousLWE.Gen 
(lweInstance
,module Challenges.ContinuousLWE.Proto) where

import Challenges.ContinuousLWE.Proto

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, Random, getRandom)

import Crypto.Lol hiding (tGaussian)
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Generate an LWE instance with a random secret, and the given scaled variance.
lweInstance ::(ULWECtx t m z zq v rq, MonadRandom rnd, Random z)
  => v -> Int -> rnd (Cyc t m z, [LWESample t m zq rq])
lweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (s, samples)

-- | An LWE sample for a given secret.
lweSample :: forall rnd t m z zq v rq . 
  (MonadRandom rnd, ULWECtx t m z zq v rq)
  => v -> Cyc t m z -> rnd (LWESample t m zq rq)
lweSample svar s = do
  let sq = reduce s :: Cyc t m zq
  e :: UCyc t m D (LiftOf rq) <- tGaussian svar
  a <- adviseCRT <$> getRandom
  let as = fmap fromSubgroup $ uncycDec $ a * sq :: UCyc t m D rq
  return $ LWESample a $ as + (reduce e)

type ULWECtx t m  z zq v rq = 
  (Reduce z zq, Ring zq, Random zq, Fact m, CElt t z,
   TElt t rq, Reduce (LiftOf rq) rq, OrdFloat (LiftOf rq),
   CElt t (LiftOf rq), ToRational v, Random (LiftOf rq),
   CElt t z, CElt t zq, Reduce z zq, Subgroup zq rq)