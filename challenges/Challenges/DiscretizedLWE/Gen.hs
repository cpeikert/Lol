{-# LANGUAGE ConstraintKinds, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Challenges.DiscretizedLWE.Gen
(lweInstance
,module Challenges.DiscretizedLWE.Proto) where

import Challenges.DiscretizedLWE.Proto

import Control.Applicative
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, Random, getRandom)

import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc as U hiding (errorRounded)

-- | Generate an LWE instance with a random secret, and the given scaled variance.
lweInstance :: (LWECtx t m z zq v q, MonadRandom rnd, Random z)
  => v -> Int -> TaggedT q rnd (Cyc t m z, [DiscLWESample t m zq])
lweInstance svar numSamples = do
  s <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (s, samples)

-- | An LWE sample for a given secret.
lweSample :: (MonadRandom rnd, LWECtx t m z zq v q)
          => v -> Cyc t m z -> TaggedT q rnd (DiscLWESample t m zq)
lweSample svar s = do
  let sq = reduce s
  e <- errorRounded svar
  a <- tagT $ adviseCRT <$> getRandom
  return $ DiscLWESample a $ a * sq + reduce (e `asTypeOf` s)

-- | Generate an LWE error term with given scaled variance,
-- deterministically rounded with respect to the decoding basis.
-- This is the same as 'Cyc.errorRounded' except that this version
-- is parameterized by the continuous type.
errorRounded :: forall v rnd t m z q .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd, 
                 OrdFloat q, Random q, TElt t q, RealField q)
                => v -> TaggedT q rnd (Cyc t m z)
errorRounded svar = tagT $ cycDec <$>
  U.fmapDec (roundMult one) <$> (U.tGaussian svar :: rnd (UCyc t m D q))

type LWECtx t m z zq v q = 
  (ToInteger z, Reduce z zq, Ring zq, Random zq, Fact m, CElt t z, CElt t zq, 
   ToRational v,
   OrdFloat q, Random q, TElt t q, RealField q)
