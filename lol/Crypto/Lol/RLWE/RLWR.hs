{-|
Module      : Crypto.Lol.RLWE.RLWR
Description : Functions and types for working with ring-LWR samples.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2018
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Functions and types for working with ring-LWR samples.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Crypto.Lol.RLWE.RLWR where

import Crypto.Lol

import Control.Monad.Random

-- | An RLWR sample \( (a,b) \in R_q \times R_p\).
type Sample cm zq zp = (cm zq, cm zp)

-- | Common constraints for working with RLWR.
type RLWRCtx cm zq zp =
  (Cyclotomic (cm zq), Random (cm zq), Ring (cm zq), RescaleCyc cm zq zp)

-- | An RLWR sample with the given secret.
sample :: (RLWRCtx cm zq zp, MonadRandom rnd)
          => cm zq -> rnd (Sample cm zq zp)
sample s = let s' = adviseCRT s in do
  a <- getRandom
  return (a, roundedProd s' a)

-- | The \(b\) component of an RLWR sample for secret \(s\) and given \(a\),
-- produced by rounding \(a\cdot s\) in the decoding basis.
roundedProd :: (RLWRCtx cm zq zp) => cm zq -> cm zq -> cm zp
{-# INLINABLE roundedProd #-}
roundedProd s = let s' = adviseCRT s in \a -> rescaleCyc Dec $ a * s'

