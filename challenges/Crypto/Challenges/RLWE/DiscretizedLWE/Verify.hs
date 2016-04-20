{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Crypto.Challenges.DiscretizedLWE.Verify
(checkInstance
,CheckInst
,module Crypto.Challenges.DiscretizedLWE.Proto) where

import Crypto.Challenges.DiscretizedLWE.Proto
import Crypto.Lol
import Data.Word

-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @bound@.
checkInstance :: (CheckInst t m z zq)
  => RLWESecret t m z -> RLWEInstanceDisc v t m zq -> Bool
checkInstance (RLWESecret _ s) (RLWEInstanceDisc _ _ bound samples) = 
  all (checkSample (fromIntegral bound) s) samples

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: (CheckInst t m z zq)
  => LiftOf zq -> Cyc t m z -> RLWESampleDisc t m zq -> Bool
checkSample bound sk pair@(RLWESampleDisc a b) = (sampleError sk pair) < bound

type CheckInst t m z zq = 
  (Fact m, CElt t zq, CElt t z, CElt t (LiftOf zq), Lift' zq, Reduce z zq, Ord (LiftOf zq))

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: (CheckInst t m z zq) 
  => Cyc t m z -> RLWESampleDisc t m zq -> LiftOf zq
sampleError sk (RLWESampleDisc a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e'
  in gSqNorm e
