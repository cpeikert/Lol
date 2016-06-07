{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables #-}

-- | Linear transforms and operations related to the decoding basis.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Dec
( tGaussianDec', gSqNormDec' ) where

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon as R
import Crypto.Lol.GaussRandom
import Crypto.Lol.Prelude

import Control.Monad.Random

-- | Given @v=r^2@, yields the decoding-basis coefficients of a sample
-- from the tweaked Gaussian @t_m \cdot D_r@.
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
    return $ fE arr

-- | The @E_m@ transformation for an arbitrary @m@.
fE :: (Fact m, Transcendental r, Unbox r, Elt r) => Arr m r -> Arr m r
fE = eval $ fTensor $ ppTensor pE

-- | The @E_p@ transformation for a prime @p@.
pE :: forall p r . (Prime p, Transcendental r, Unbox r, Elt r)
      => Tagged p (Trans r)
pE = let pval = proxy valuePrime (Proxy::Proxy p)
     in tag $ if pval==2 then Id 1
              else trans (pval-1) $ mulMat $ force $
                   fromFunction (Z :. pval-1 :. pval-1)
              (\(Z:.i:.j) ->
               -- sqrt(2)*[ cos(2pi*i*(j+1)/p) | sin(same) ]
               -- (signs of columns doesn't matter for our purposes.)
               let theta = 2 * pi * fromIntegral (i*(j+1)) /
                           fromIntegral pval
               in sqrt 2 * if j < pval `div` 2
                           then cos theta else sin theta)

-- | Given coefficient tensor @e@ with respect to the decoding basis
-- of @R@, yield the (scaled) squared norm of @g_m \cdot e@ under
-- the canonical embedding, namely,
--  @\hat{m}^{ -1 } \cdot || \sigma(g_m -- \cdot e) ||^2@ .
gSqNormDec' :: (Fact m, Ring r, Unbox r, Elt r) => Arr m r -> r
gSqNormDec' e@(Arr ae) = let (Arr ae') = fGramDec' e
                         -- use sumAllP (define it in RTCommon)?
                         in sumAllS $ force $ R.zipWith (*) ae ae'

-- | Multiply by @\hat{m}@ times the Gram matrix of decoding basis of
-- @R^vee@.
fGramDec' :: (Fact m, Ring r, Unbox r, Elt r) => Arr m r -> Arr m r
fGramDec' = eval $ fTensor $ ppTensor pGramDec

-- | Multiply by (scaled) Gram matrix of decoding basis: (I_{p-1} + all-1s).
pGramDec :: forall p r . (Prime p, Ring r, Unbox r, Elt r) => Tagged p (Trans r)
pGramDec =
  let pval = proxy valuePrime (Proxy::Proxy p)
  in tag $ if pval==2 then Id 1
           else trans (pval-1) $
                    \arr -> let sums = sumS arr
                            in fromFunction (extent arr)
                                   (\sh@(sh' :. _) -> arr ! sh + sums ! sh')


