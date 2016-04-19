{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude #-}

module Crypto.Challenges.LWR.Verify 
( checkInstance
, CheckInst
, module Crypto.Challenges.LWR.Proto
) where

import Crypto.Challenges.LWR.Proto
import Crypto.Lol

-- | Verifies that [a*s]_q'=b for all samples in an LWR instance.
checkInstance :: (CheckInst t m z zq zq')
  => RLWESecret t m z -> RLWRInstance t m zq zq' -> Bool
checkInstance (RLWESecret _ s) (RLWRInstance _ samples) = all (checkSample s) samples

-- | Verifies that [a*s]_q'=b in an LWR sample.
checkSample :: (CheckInst t m z zq zq') => Cyc t m z -> RLWRSample t m zq zq' -> Bool
checkSample s (RLWRSample a b) = b == (rescaleCyc Dec $ a * reduce s)

type CheckInst t m z zq zq' = 
  (Fact m, CElt t zq, CElt t zq', CElt t z, Eq zq', Reduce z zq, RescaleCyc (Cyc t) zq zq')
