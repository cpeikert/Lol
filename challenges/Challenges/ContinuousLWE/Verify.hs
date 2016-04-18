{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Challenges.ContinuousLWE.Verify
(checkInstance
,CheckInst
,module Challenges.ContinuousLWE.Proto) where

import Challenges.ContinuousLWE.Proto

import Crypto.Lol hiding (gSqNorm)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @bound@.
checkInstance :: (CheckInst t m z zq rq)
  => LWESecret t m z -> ContLWEInstance (LiftOf rq) t m zq rq -> Bool
checkInstance (LWESecret _ s) (ContLWEInstance _ _ bound samples) = all (checkSample bound s) samples

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: (CheckInst t m z zq rq) 
  => LiftOf rq -> Cyc t m z -> ContLWESample t m zq rq -> Bool
checkSample bound sk pair@(ContLWESample a b) = (sampleError sk pair) < bound

type CheckInst t m z zq rq = 
  (Subgroup zq rq, Fact m, Ring zq,  CElt t zq,
   Reduce z zq, CElt t z,
   Lift' rq, TElt t rq, TElt t (LiftOf rq), 
   Ord (LiftOf rq),
   Ring (LiftOf rq),
   UCRTElt t (LiftOf rq))

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: (CheckInst t m z zq rq) 
  => Cyc t m z -> ContLWESample t m zq rq -> LiftOf rq
sampleError sk (ContLWESample a b) = 
  let as = fmap fromSubgroup $ uncycDec $ a * reduce sk
      e = lift $ b - as
  in gSqNorm e