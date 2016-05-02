{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

-- | Functions for sampling from a continuous Gaussian distribution

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

realGaussian :: forall v q m .
                (ToRational v, OrdFloat q, Random q, MonadRandom m)
                => v -> m (q,q)
realGaussian svar =
    let var = realToField svar / pi :: q -- twice true variance
    in do (u,v) <- iterateWhile uvGuard getUV
          let t = u*u+v*v
              com = sqrt (-var * log t / t)
          -- we can either sample u,v from [-1,1]
          -- or generate sign bits for the outputs
          s1 <- getRandom
          s2 <- getRandom
          let u' = if s1 then u else -u
              v' = if s2 then v else -v
          return (u'*com,v'*com)
    where getUV = do u <- getRandomR (zero,one)
                     v <- getRandomR (zero,one)
                     return (u,v)
          uvGuard (u,v) = (u*u+v*v >= one) || (u*u+v*v == zero)

-- | Generate @n@ real, independent gaussians of scaled variance @svar
-- = true variance * (2*pi)@.
realGaussians ::
    (ToRational svar, OrdFloat i, Random i, V.Vector v i, MonadRandom m)
    => svar -> Int -> m (v i)
realGaussians var n
    | odd n = V.tail <$> (realGaussians var (n+1)) -- O(1) tail
    | otherwise = (V.fromList . uncurry (++) . unzip) <$>
                  replicateM (n `div` 2) (realGaussian var)






-- Taken from monad-loops-0.4.3

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
iterateWhile :: (Monad m) => (a -> Bool) -> m a -> m a
iterateWhile p x = x >>= iterateUntilM (not . p) (const x)

-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntilM p f

{-
-- | Returns a Gaussian-distributed sample over 'pZ' with given
-- (scaled) variance parameter @v=var/(2*pi)@ and center, using
-- rejection sampling

gaussRound :: (RealTranscendental v, Random v,
               RealRing c, ToRational c,
               Ring i, ToInteger i, Random i, MonadRandom m)
               => v -> c -> m i
gaussRound svar c =
    let dev = ceiling $ 6 * sqrt svar -- 6 gives stat dist < 2^-163
        lower = floor c - dev
        upper = ceiling c + dev
        sampler = do
           z <- getRandomR (lower, upper)
           u <- getRandomR (zero, one)
           let dist = fromIntegral z - realToField c
           let prob = exp (-pi * (dist*dist / svar))
           if u <= prob then return z else sampler
    in sampler
-}
