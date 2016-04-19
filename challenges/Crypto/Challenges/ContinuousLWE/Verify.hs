{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses,
             NoImplicitPrelude #-}

module Crypto.Challenges.ContinuousLWE.Verify
( checkInstance
, CheckInst
, module Crypto.Challenges.ContinuousLWE.Proto) where

import Control.Applicative

import Crypto.Challenges.ContinuousLWE.Proto
import Crypto.Lol                   hiding (gSqNorm)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.UCyc

-- | Verify that the (scaled, squared) norm of the noise for each
-- sample in the instance is below @bound@.
checkInstance :: (CheckInst t m z zq rq)
  => RLWESecret t m z -> RLWEInstanceCont (LiftOf rq) t m zq rq -> Bool
checkInstance (RLWESecret _ s) (RLWEInstanceCont _ _ bound samples) = all (checkSample bound s) samples

-- | Verify that the (scaled, squared) norm of the noise for an LWE
-- sample is below the provided bound.
checkSample :: (CheckInst t m z zq rq)
  => LiftOf rq -> Cyc t m z -> RLWESampleCont t m zq rq -> Bool
checkSample bound sk pair@(RLWESampleCont a b) = sampleError sk pair < bound

type CheckInst t m z zq rq =
  (Subgroup zq rq, Fact m, Ring zq, CElt t zq,
   Reduce z zq, CElt t z,
   Lift' rq, TElt t rq, TElt t (LiftOf rq),
   Ord (LiftOf rq),
   Ring (LiftOf rq),
   UCRTElt t (LiftOf rq))

-- | Given an instance and corresponding secret, outputs the (scaled,
-- squared) norm of the error term.
sampleError :: (CheckInst t m z zq rq)
  => Cyc t m z -> RLWESampleCont t m zq rq -> LiftOf rq
sampleError sk (RLWESampleCont a b) =
  let as = fromSubgroup <$> (uncycDec $ a * reduce sk)
      e = lift $ b - as
  in gSqNorm e
