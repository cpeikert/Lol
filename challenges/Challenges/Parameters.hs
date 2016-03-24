
module Challenges.Parameters where

import Challenges.Beacon

beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0
{-
beaconBitsPerChallenge = 2 :: Int
-- during the verification purposes, we'll reveal secrets for all but one instance
instancesPerChallenge = 2^beaconBitsPerChallenge :: Int
samplesPerInstance = 10 :: Int
-}
hashOutputBits = 512 :: Int
hashPrettyPrintLineSize = 64 :: Int
