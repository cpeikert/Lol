{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables,
             TypeFamilies #-}

module Crypto.Challenges.ContinuousLWE.Gen
( rlweInstance
, module Crypto.Challenges.ContinuousLWE.Proto) where

import Crypto.Challenges.ContinuousLWE.Proto

import Control.Applicative
import Control.Monad        (replicateM)
import Control.Monad.Random (MonadRandom, Random, getRandom)

import Crypto.Lol                   hiding (tGaussian)
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Generate an RLWE instance with a random secret, and the given
-- scaled variance.
rlweInstance ::(ULWECtx t m z zq v rq, MonadRandom rnd, Random z)
  => v -> Int -> rnd (Cyc t m z, [RLWESampleCont t m zq rq])
rlweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (rlweSample svar s)
  return (s, samples)

-- | An RLWE sample for a given secret.
rlweSample :: forall rnd t m z zq v rq .
  (MonadRandom rnd, ULWECtx t m z zq v rq)
  => v -> Cyc t m z -> rnd (RLWESampleCont t m zq rq)
rlweSample svar s = do
  let sq = reduce s :: Cyc t m zq
  e :: UCyc t m D (LiftOf rq) <- tGaussian svar
  a <- adviseCRT <$> getRandom
  let as = fmap fromSubgroup $ uncycDec $ a * sq :: UCyc t m D rq
  return (a, as + reduce e)

type ULWECtx t m  z zq v rq =
  (Reduce z zq, Ring zq, Random zq, Fact m, CElt t z,
   TElt t rq, Reduce (LiftOf rq) rq, OrdFloat (LiftOf rq),
   CElt t (LiftOf rq), ToRational v, Random (LiftOf rq),
   CElt t z, CElt t zq, Reduce z zq, Subgroup zq rq)
