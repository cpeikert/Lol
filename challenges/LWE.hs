{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude, ScopedTypeVariables #-}

module LWE where

import TGaussian

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor

type LWESample t m zp = (Cyc t m zp, Cyc t m zp)

type LWECtx t m' z zq =
  (ToInteger z, Reduce z zq, Ring zq, Random zq, Fact m', CElt t z, CElt t zq)

lweSamples :: forall t m q zp v rnd . 
  (LWECtx t m (LiftOf zp) zp, MonadRandom rnd, ToRational v,
   OrdFloat q, Random q, TElt t q, RealField q, Random (LiftOf zp))
             => v -> Int -> TaggedT q rnd [LWESample t m zp]
lweSamples svar numSamples = do
  s :: Cyc t m (LiftOf zp) <- getRandom
  replicateM numSamples (lweSample svar s)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: (LWECtx t m z zp, MonadRandom rnd, ToRational v,
              OrdFloat q, Random q, TElt t q, RealField q)
             => v -> Cyc t m z -> TaggedT q rnd (LWESample t m zp)
lweSample svar s =
  -- adviseCRT because we call `replicateM (lweSample s)` below, but only want to do CRT once.
  let sq = adviseCRT $ negate $ reduce s
  in do
    e <- errorRounded svar
    c1 <- tagT $ adviseCRT <$> getRandom -- want entire hint to be in CRT form
    return $ (c1 * sq + reduce (e `asTypeOf` s), c1)