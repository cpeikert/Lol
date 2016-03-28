{-# LANGUAGE ConstraintKinds, FlexibleContexts, 
             NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Challenges.LWE 
(lweInstance
,LWECtx
, module Challenges.ProtoReader) where

import Challenges.ProtoReader

import Control.Applicative
import Control.Monad
import Control.Monad.Random hiding (fromList)

import Crypto.Lol hiding (errorRounded)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc as U hiding (errorRounded)

lweInstance :: forall t m q zp v rnd .  
  (LWECtx t m (LiftOf zp) zp v q, MonadRandom rnd, Random (LiftOf zp))
  => v -> Int -> TaggedT q rnd (Cyc t m (LiftOf zp), [LWESample t m zp])
lweInstance svar numSamples = do
  s :: Cyc t m (LiftOf zp) <- getRandom
  samples <- replicateM numSamples (lweSample svar s)
  return (s, samples)

-- | An LWE sample for a given secret (corresponding to a linear
-- ciphertext encrypting 0 in MSD form)
lweSample :: (MonadRandom rnd, LWECtx t m z zp v q)
          => v -> Cyc t m z -> TaggedT q rnd (LWESample t m zp)
lweSample svar s = do
  let sq = reduce s
  e <- errorRounded svar
  a <- tagT $ adviseCRT <$> getRandom -- want entire hint to be in CRT form
  return $ LWESample a $ a * sq + reduce (e `asTypeOf` s)

errorRounded :: forall v rnd t m z q .
                (ToInteger z, Tensor t, Fact m, TElt t z,
                 ToRational v, MonadRandom rnd, 
                 OrdFloat q, Random q, TElt t q, RealField q)
                => v -> TaggedT q rnd (Cyc t m z)
errorRounded svar = tagT $ cycDec <$>
  U.fmapDec (roundMult one) <$> (U.tGaussian svar :: rnd (UCyc t m D q))

type LWECtx t m z zp v q = 
  (ToInteger z, Reduce z zp, Ring zp, Random zp, Fact m, CElt t z, CElt t zp, 
   ToRational v,
   OrdFloat q, Random q, TElt t q, RealField q)
