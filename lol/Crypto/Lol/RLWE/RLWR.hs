{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, ScopedTypeVariables #-}

-- | Functions and types for working with ring-LWR samples.

module Crypto.Lol.RLWE.RLWR where

import Crypto.Lol

import Control.Monad.Random

-- | An RLWR sample @(a,b) \in R_q \times R_p@.
type Sample t m zq zp = (Cyc t m zq, Cyc t m zp)

-- | Common constraints for working with RLWR.
type RLWRCtx t m zq zp =
  (Fact m, Ring zq, RescaleCyc (Cyc t) zq zp, CElt t zq, CElt t zp)

-- | An RLWR sample with the given secret.
sample :: (RLWRCtx t m zq zp, Random zq, MonadRandom rnd)
          => Cyc t m zq -> rnd (Sample t m zq zp)
sample s = let s' = adviseCRT s in do
  a <- getRandom
  return (a, roundedProd s' a)

-- | The @b@ component of an RLWR sample for secret @s@ and given @a@,
-- produced by rounding @a*s@ in the decoding basis.
roundedProd :: (RLWRCtx t m zq zp) => Cyc t m zq -> Cyc t m zq -> Cyc t m zp
{-# INLINABLE roundedProd #-}
roundedProd s = let s' = adviseCRT s in \a -> rescaleCyc Dec $ a * s'

