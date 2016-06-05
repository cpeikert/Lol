{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

-- | Functions and types for working with continuous ring-LWE samples.

module Crypto.Lol.RLWE.Continuous where

import Crypto.Lol.Cyclotomic.Cyc    as C
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Cyclotomic.UCyc   as U
import Crypto.Lol.Prelude

import Control.Applicative
import Control.Monad.Random

-- | A continuous RLWE sample @(a,b) \in R_q \times K/qR@.  (The
-- second component is a 'UCyc' because the base type @rrq@
-- representing @RR/qZ@, the reals modulo @qZ@, is an additive group
-- but not a ring, so we can't usefully work with a 'Cyc' over it.)
type Sample t m zq rrq = (Cyc t m zq, UCyc t m D rrq)

-- | Common constraints for working with continuous RLWE.
type RLWECtx t m zq rrq =
  (Fact m, Ring zq, CElt t zq, Subgroup zq rrq, Lift' rrq,
   TElt t rrq, TElt t (LiftOf rrq))

-- | A continuous RLWE sample with the given scaled variance and
-- secret.
sample :: forall rnd v t m zq rrq .
  (RLWECtx t m zq rrq, Random zq, Random (LiftOf rrq), OrdFloat (LiftOf rrq),
   MonadRandom rnd, ToRational v)
  => v -> Cyc t m zq -> rnd (Sample t m zq rrq)
{-# INLINABLE sample #-}
sample svar s = let s' = adviseCRT s in do
  a <- getRandom
  e :: UCyc t m D (LiftOf rrq) <- U.tGaussian svar
  let as = fmapDec fromSubgroup $ uncycDec $ a * s' :: UCyc t m D rrq
  return (a, as + reduce e)

-- | The error term of an RLWE sample, given the purported secret.
errorTerm :: (RLWECtx t m zq rrq)
             => Cyc t m zq -> Sample t m zq rrq -> UCyc t m D (LiftOf rrq)
{-# INLINABLE errorTerm #-}
errorTerm s = let s' = adviseCRT s
              in \(a,b) -> lift $ b - fmapDec fromSubgroup (uncycDec $ a * s')

-- | The 'gSqNorm' of the error term of an RLWE sample, given the
-- purported secret.
errorGSqNorm :: (RLWECtx t m zq rrq, Ring (LiftOf rrq))
                => Cyc t m zq -> Sample t m zq rrq -> LiftOf rrq
{-# INLINABLE errorGSqNorm #-}
errorGSqNorm s = U.gSqNorm . errorTerm s

-- | A bound such that the 'gSqNorm' of a continuous error generated
-- by 'tGaussian' with scaled variance @v@ (over the @m@th cyclotomic
-- field) is less than the bound except with probability approximately
-- @eps@.
errorBound :: (Ord v, Transcendental v, Fact m)
              => v              -- ^ the scaled variance
              -> v              -- ^ @eps@
              -> Tagged m v
errorBound v eps = do
  n <- fromIntegral <$> totientFact
  mhat <- fromIntegral <$> valueHatFact
  let stabilize x =
        let x' = (1/2 + log (2 * pi * x)/2 - log eps/n)/pi
        in if x'-x < 0.0001 then x' else stabilize x'
  return $ mhat * n * v * stabilize (1/(2*pi))
