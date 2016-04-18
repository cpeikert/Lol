{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Challenges.LWR.Verify 
( checkInstance
, CheckInst
, module Challenges.LWR.Proto
) where

import Challenges.LWR.Proto

import Crypto.Lol

-- | Verifies that [a*s]_q'=b for all samples in an LWR instance.
checkInstance :: (CheckInst t m z zq zq')
  => LWESecret t m z -> LWRInstance t m zq zq' -> Bool
checkInstance (LWESecret _ s) (LWRInstance _ samples) = all (checkSample s) samples

-- | Verifies that [a*s]_q'=b in an LWR sample.
checkSample :: (CheckInst t m z zq zq') => Cyc t m z -> LWRSample t m zq zq' -> Bool
checkSample s (LWRSample a b) = b == rescaleCyc Dec $ a * reduce s

type CheckInst t m z zq zq' = 
  (Fact m, CElt t zq, CElt t zq', CElt t z, Eq zq', Reduce z zq, RescaleCyc (Cyc t) zq zq')
