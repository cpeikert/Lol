{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Functions for sampling from a continuous Gaussian distribution
--
module Crypto.Lol.GaussRandom (

  realGaussian, realGaussians

) where

import Crypto.Lol.LatticePrelude

import qualified Data.Vector.Generic                                as G
import qualified Data.Vector.Unboxed                                as U

import Control.Applicative                                          ( (<$>), (<*>) )
import Control.Monad.Random


-- | Using polar form of Box-Muller transform, returns a pair of
-- centered, Gaussian-distributed real numbers with scaled variance
-- @svar = true variance * (2*pi)@.
--
-- See
-- <http://www.alpheratz.net/murison/Maple/GaussianDistribution/GaussianDistribution.pdf
-- this link> for details.
--
realGaussian
    :: forall v q m . (ToRational v, OrdFloat q, Random q, MonadRandom m)
    => v
    -> m (q,q)
realGaussian svar = do
  let
      new       = (,) <$> getRandomR (zero,one) <*> getRandomR (zero,one)
      bad (u,v) = let t = u*u+v*v
                  in  t >= one || t == zero
  --
  (u,v) <- iterateWhile bad new
  s1    <- getRandom
  s2    <- getRandom
  --
  let t   = u*u+v*v
      com = sqrt (-var * log t / t)
      var = realToField svar / pi :: q -- twice true variance

      -- We can either sample u,v from [-1,1], or generate sign bits for the
      -- outputs (which is what we do)
      u' = if s1 then u else negate u
      v' = if s2 then v else negate v
  --
  return (u'*com,v'*com)


-- | Generate @n@ real, independent gaussians of scaled variance
-- @svar = true variance * (2*pi)@.
--
{-# INLINEABLE realGaussians #-}
{-# SPECIALIZE realGaussians :: (ToRational svar, OrdFloat i, Random i, MonadRandom m, U.Unbox i) => svar -> Int -> m (U.Vector i) #-}
realGaussians
    :: (ToRational svar, OrdFloat i, Random i, G.Vector v (i,i), G.Vector v i, MonadRandom m)
    => svar
    -> Int
    -> m (v i)
realGaussians var n
  | odd n     = G.tail                   <$> realGaussians var (n+1)
  | otherwise = uncurry (G.++) . G.unzip <$> G.replicateM (n `div` 2) (realGaussian var)


-- Taken from monad-loops-0.4.3

-- | Execute an action repeatedly until its result fails to satisfy a predicate,
-- and return that result (discarding all others).
--
iterateWhile :: (Monad m) => (a -> Bool) -> m a -> m a
iterateWhile p x = x >>= iterateUntilM (not . p) (const x)

-- | Analogue of @('Prelude.until')@
-- Yields the result of applying f until p holds.
--
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
