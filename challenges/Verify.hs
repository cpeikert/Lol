{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Verify where

import LWE

import Crypto.Lol

checkChallenge :: (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m,
   Lift' zp, CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => SecretLWEChallenge v t m zp -> Bool
checkChallenge (SLWEChallenge v insts) = all (checkInstance v) insts

checkInstance :: forall v t m zp . 
  (Absolute (LiftOf zp), Ord (LiftOf zp), Fact m, 
   Lift' zp, CElt t zp, CElt t (LiftOf zp), Ord v, 
   ToInteger (LiftOf zp), Ring v)
  => v -> SecretLWEInstance t m zp -> Bool
checkInstance v (SLWEInstance sk pairs) = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all (checkSample bound sk) pairs

checkSample :: forall v t m zp . 
  (Ord v, Absolute (LiftOf zp), Ord (LiftOf zp), Fact m,
   Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp), Ring v) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk (LWESample a b) = 
  let e' = b-a*reduce sk :: Cyc t m zp
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm < bound