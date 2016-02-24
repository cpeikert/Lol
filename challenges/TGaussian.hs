{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module TGaussian where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc as U

errorRounded :: forall v rnd t m z q .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd, 
                 OrdFloat q, Random q, TElt t q, RealField q)
                => v -> TaggedT q rnd (Cyc t m z)
errorRounded svar = tagT $ cycDec <$>
  U.fmapDec (roundMult one) <$> (U.tGaussian svar :: rnd (UCyc t m D q))

