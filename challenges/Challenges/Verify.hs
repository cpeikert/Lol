{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Challenges.Verify where

import Challenges.LWE

import Crypto.Lol

checkInstance :: forall v t m zp . (CheckSample v t m zp)
  => v -> Cyc t m (LiftOf zp) -> [LWESample t m zp] -> Bool
checkInstance v sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all (checkSample bound sk) samples

type CheckSample v t m zp = 
  (CheckErr v t m zp, Ord v, Field v)

checkSample :: (CheckSample v t m zp) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound
type CheckErr v t m zp = 
  (Fact m, Ring v, Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp))

sampleError :: forall v t m zp . (CheckErr v t m zp) 
  => Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm