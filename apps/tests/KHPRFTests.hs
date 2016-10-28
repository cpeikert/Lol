{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module KHPRFTests where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.UCyc

import MathObj.Matrix

import Tests
import qualified Test.Framework as TF

sheTests :: forall t m zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(m,zp,zq,gad) -> Proxy t -> [rnd TF.Test]
sheTests _ _ =
  let ptmr = Proxy::Proxy '(t,m,zp,zq,gad)
  in ($ ptmr) <$> [
   genTestArgs "PRF_3bits" (prop_KeyHomom 3),
   genTestArgs "PRF_5bits" (prop_KeyHomom 5)]

-- +/-1 in every coefficient of the rounding basis
prop_KeyHomom :: forall t m zp zq gad . (Fact m, CElt t zq, CElt t zp, _)
  => Int -> Test '(t,m,zp,zq,gad)
prop_KeyHomom size = testIO $ do
  family :: PRFFamily gad (Cyc t m zq) (Cyc t m zp) <- randomFamily size
  s1 <- getRandom
  s2 <- getRandom
  x <- ((`mod` (2^size)) . abs) <$> getRandom
  let s3 = s1+s2
      state = prfState family Nothing
      prf1 = fst $ ringPRF s1 x state
      prf2 = fst $ ringPRF s2 x state
      prf3 = fst $ ringPRF s3 x state
      prf3' = prf1+prf2 :: Matrix (Cyc t m zp)
      a = uncycPow <$> prf3
      b = uncycPow <$> prf3'
      c = concat $ rows $ a - b
      c' = map (maximum . fmapPow abs . lift) c
  return $ maximum c' <= 1