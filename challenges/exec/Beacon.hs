{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions for working with the
-- <http://www.nist.gov/itl/csd/ct/nist_beacon.cfm NIST Randomness
-- Beacon>.

module Beacon where

import Control.DeepSeq

import Data.Int
import Data.Time.Calendar    (fromGregorian)
import Data.Time.Clock       (UTCTime (..), secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime   (getCurrentTimeZone, timeZoneMinutes)

-- | The number of bytes (@512 / 8 = 64@) per beacon.
bytesPerBeacon :: Int32
bytesPerBeacon = 64

-- | The number of seconds (@60@) between beacon outputs.
beaconInterval :: Int64
beaconInterval = 60

-- | Represents a byte offset in a beacon output at a particular time.
data BeaconAddr = BA Int64 Int32 deriving (Eq, Show)
instance NFData BeaconAddr where rnf (BA a b) = rnf a `seq` rnf b

-- | Advances the beacon position by one byte, overflowing to the next
-- beacon if necessary.
nextBeaconAddr :: BeaconAddr -> BeaconAddr
nextBeaconAddr (BA time byteOffset) =
  if byteOffset == bytesPerBeacon
  then BA (time+beaconInterval) 0
  else BA time (byteOffset+1)

-- | The number of seconds elapsed from a given GMT time since the (GMT) epoch.
gmtDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> Int
gmtDateToSeconds month day year hour minute |
  hour >= 0 && hour < 24 && minute >= 0 && minute < 60 =
    round $ utcTimeToPOSIXSeconds $
    UTCTime (fromGregorian year month day)
    (secondsToDiffTime $ fromIntegral $ 3600*hour+60*minute)
gmtDateToSeconds _ _ _ _ _ = error "invalid date to gmtDateToSeconds"

-- | The number of seconds elapsed from a given local time since the
-- (GMT) epoch.
localDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> IO Int64
localDateToSeconds month day year hour minute = do
  let gmt = gmtDateToSeconds month day year hour minute
  minuteOffset <- timeZoneMinutes <$> getCurrentTimeZone
  return $ fromIntegral $ gmt - (minuteOffset*60)
