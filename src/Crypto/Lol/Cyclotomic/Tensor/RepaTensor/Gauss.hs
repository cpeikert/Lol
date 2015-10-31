{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables #-}

-- | (Continuous) Gaussian sampling for Repa arrays

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Gauss
( tGaussianDec' ) where

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon
import Crypto.Lol.GaussRandom
import Crypto.Lol.LatticePrelude

import Control.Monad.Random

-- | A function tagged by the cyclotomic index which,
-- given a (scaled) variance, outputs a Gaussian-distributed
-- vector in the decoding basis
tGaussianDec' :: forall m v r rnd .
                 (Fact m, OrdFloat r, Random r, Unbox r, Elt r,
                  ToRational v, MonadRandom rnd)
                 => v -> rnd (Arr m r)
tGaussianDec' =
  let pm = Proxy::Proxy m
      m = proxy valueFact pm
      n = proxy totientFact pm
      rad = proxy radicalFact pm
  in \v -> do             -- rnd monad
    x <- realGaussians (v * fromIntegral (m `div` rad)) n
    let arr = Arr $ fromUnboxed (Z:.n) x
    return $ fD arr

-- | The fully tensored D transformation
fD :: (Fact m, Transcendental r, Unbox r, Elt r) => Arr m r -> Arr m r
fD = eval $ fTensor $ ppTensor pD

-- | The D transformation for a prime
pD :: forall p r . (NatC p, Transcendental r, Unbox r, Elt r)
      => Tagged p (Trans r)
pD = let pval = proxy valueNatC (Proxy::Proxy p)
     in tag $
        if pval==2
        then Id 1
        else trans (pval-1) $ mulMat $ force $
                            fromFunction (Z :. pval-1 :. pval-1)
                            (\(Z:.i:.j) ->
                              -- mtx is sqrt(2)*[ cos(2pi*i*(j+1)/p) | sin(same) ]
                              -- (signs of columns doesn't matter for our purposes.)
                              let theta = 2 * pi * fromIntegral (i*(j+1)) /
                                          fromIntegral pval
                              in sqrt 2 * if j < pval `div` 2
                                          then cos theta else sin theta)
