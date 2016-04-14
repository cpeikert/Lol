{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude, ScopedTypeVariables #-}

module Challenges.DiscretizedLWE.Verify
(checkInstance
,module Challenges.DiscretizedLWE.Proto) where

import Challenges.DiscretizedLWE.Proto
import Crypto.Lol

-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @mhat*n*v@.
checkInstance :: forall t m zq . (CheckSample t m zq)
  => LiftOf zq -> Cyc t m (LiftOf zq) -> [LWESample t m zq] -> Bool
checkInstance bound sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
  in all (checkSample bound sk) samples

type CheckSample t m zq = 
  (CheckErr t m zq, Show (LiftOf zq))

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: forall t m zq . (CheckSample t m zq) 
  => LiftOf zq -> Cyc t m (LiftOf zq) -> LWESample t m zq -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound

type CheckErr t m zq = 
  (Fact m, Lift' zq, CElt t zq, CElt t (LiftOf zq), ToInteger (LiftOf zq))

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: forall t m zq . (CheckErr t m zq) 
  => Cyc t m (LiftOf zq) -> LWESample t m zq -> LiftOf zq
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zq)
  in gSqNorm e
