{-|
Module      : Crypto.Lol.GaussRandom
Description : Gaussian sampling.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Functions for sampling from a continuous Gaussian distribution.
-}

{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.Lol.GaussRandom
( realGaussian, realGaussians ) where

import Crypto.Lol.Prelude

import qualified Data.Vector.Generic as V

import Control.Applicative
import Control.Monad
import Control.Monad.Random

-- | Using polar form of Box-Muller transform, returns a pair of
-- centered, Gaussian-distributed real numbers with scaled variance
-- @svar = true variance * (2*pi)@. See
-- <http://www.alpheratz.net/murison/Maple/GaussianDistribution/GaussianDistribution.pdf
-- this link> for details.
{-# INLINABLE realGaussian #-}
realGaussian :: forall v q m .
                (ToRational v, OrdFloat q, Random q, MonadRandom m)
                => v -> m (q,q)
realGaussian svar =
    let var = realToField svar / pi :: q -- twice true variance
    in do (u,v) <- iterateWhile uvGuard getUV
          let t = u*u+v*v
              com = sqrt (-var * log t / t)
          return (u*com,v*com)
    where getUV = do u <- getRandomR (-one,one)
                     v <- getRandomR (-one,one)
                     return (u,v)
          uvGuard (u,v) = (u*u+v*v >= one) || (u*u+v*v == zero)

-- | Generate @n@ real, independent gaussians of scaled variance @svar
-- = true variance * (2*pi)@.
realGaussians ::
    (ToRational svar, OrdFloat i, Random i, V.Vector v i, MonadRandom m)
    => svar -> Int -> m (v i)
{-# INLINABLE realGaussians #-}
realGaussians var n
    | odd n = V.tail <$> realGaussians var (n+1) -- O(1) tail
    | otherwise = (V.fromList . uncurry (++) . unzip) <$>
                  replicateM (n `div` 2) (realGaussian var)

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
iterateWhile :: (Monad m) => (a -> Bool) -> m a -> m a
{-# INLINE iterateWhile #-}
iterateWhile p x = go
  where go = do
          y <- x
          if p y then go else return y
