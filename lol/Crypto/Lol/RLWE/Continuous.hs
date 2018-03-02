{-|
Module      : Crypto.Lol.RLWE.Continuous
Description : Functions and types for working with continuous ring-LWE samples.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\R{\mathbb{R}} \)

Functions and types for working with continuous ring-LWE samples.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Crypto.Lol.RLWE.Continuous where

import Crypto.Lol.Cyclotomic.Language
import Crypto.Lol.Prelude

import Control.Applicative
import Control.Monad.Random hiding (lift)

-- | A continuous RLWE sample \( (a,b) \in R_q \times K/(qR) \).  The
-- base type @rrq@ represents \( \R/q\Z \), the additive group of
-- reals modulo \( q \).
type Sample c (m :: Factored) zq rrq = (c m zq, c m rrq)

-- CJP: trying to avoid this problem:
-- (The second component is a 'CycRep' because the base type @rrq@,
-- representing \(\R/(q\Z)\), is an additive group but not a ring, so
-- we can't usefully work with a 'Cyc' over it.)

-- | Common constraints for working with continuous RLWE.
type RLWECtx c m zq rrq =
  (Fact m, Cyclotomic c zq, Subgroup zq rrq, Lift' rrq)

-- | A continuous RLWE sample with the given scaled variance and secret.
sample :: forall rnd v c m zq rrq .
  (RLWECtx c m zq rrq, Random zq, Random (LiftOf rrq), OrdFloat (LiftOf rrq),
   MonadRandom rnd, ToRational v)
  => v -> c m zq -> rnd (Sample c m zq rrq)
{-# INLINABLE sample #-}
sample svar s = let s' = adviseCRT s in do
  a <- getRandom
  e :: c m (LiftOf rrq) <- tweakedGaussian svar
  let as = fmapDec fromSubgroup (a * s')
  return (a, as + reduce e)

-- | The error term of an RLWE sample, given the purported secret.
errorTerm :: (RLWECtx c m zq rrq)
             => c m zq -> Sample c m zq rrq -> c m (LiftOf rrq)
{-# INLINABLE errorTerm #-}
errorTerm s = let s' = adviseCRT s
              in \(a,b) -> liftDec $ b - fmapDec fromSubgroup (a * s')

-- | The 'gSqNorm' of the error term of an RLWE sample, given the
-- purported secret.
errorGSqNorm :: (RLWECtx c m zq rrq, GSqNorm c (LiftOf rrq))
                => c m zq -> Sample c m zq rrq -> LiftOf rrq
{-# INLINABLE errorGSqNorm #-}
errorGSqNorm s = gSqNorm . errorTerm s

-- | A bound such that the 'gSqNorm' of a continuous error generated
-- by 'tweakedGaussian' with scaled variance \(v\) (over the \(m\)th
-- cyclotomic field) is less than the bound except with probability
-- approximately \( \varepsilon \).
errorBound :: (Ord v, Transcendental v, Fact m)
              => v              -- ^ the scaled variance
              -> v              -- ^ \( \varepsilon \)
              -> Tagged m v
errorBound v eps = do
  n <- fromIntegral <$> totientFact
  mhat <- fromIntegral <$> valueHatFact
  let stabilize x =
        let x' = (1/2 + log (2 * pi * x)/2 - log eps/n)/pi
        in if x'-x < 0.0001 then x' else stabilize x'
  return $ mhat * n * v * stabilize (1/(2*pi))
