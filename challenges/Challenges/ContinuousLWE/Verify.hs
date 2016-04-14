{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude, ScopedTypeVariables #-}

module Challenges.ContinuousLWE.Verify
(checkInstance
,module Challenges.ContinuousLWE.Proto) where

import Challenges.ContinuousLWE.Proto

import Crypto.Lol hiding (gSqNorm)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @mhat*n*v@.
checkInstance :: forall t m zq rq . (CheckSample t m zq rq)
  => LiftOf rq -> Cyc t m (LiftOf zq) -> [LWESample t m zq rq] -> Bool
checkInstance bound sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
  in all (checkSample bound sk) samples

type CheckSample t m zq rq = 
  (CheckErr t m zq rq, Ord (LiftOf rq))

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: forall t m zq rq . (CheckSample t m zq rq) 
  => LiftOf rq -> Cyc t m (LiftOf zq) -> LWESample t m zq rq -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound

type CheckErr t m zq rq = 
  (Fact m, Lift' zq, CElt t zq, CElt t (LiftOf zq), 
   ToInteger (LiftOf zq), CElt t (LiftOf rq), Lift' rq, TElt t rq)

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: forall t m zq rq . (CheckErr t m zq rq) 
  => Cyc t m (LiftOf zq) -> LWESample t m zq rq -> LiftOf rq
sampleError sk (LWESample a b) = 
  let as = reduce (fmap fromIntegral $ lift $ uncycDec $ a * reduce sk :: UCyc t m D (LiftOf rq))
      e = lift $ (b - as :: UCyc t m D rq)
  in gSqNorm e