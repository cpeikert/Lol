
module Random where

import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime(..),secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds,POSIXTime)
import Data.Time.Calendar (fromGregorian)

import Net.Beacon

-- returns seconds since epoch at midnight on the day/month/year of input
dateToSeconds :: Int -> Int -> Integer -> POSIXTime
dateToSeconds month day year = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian year month day) (secondsToDiffTime 0)

beaconValue :: Int -> IO ByteString
beaconValue time = do
  (Just rec) <- getCurrentRecord time
  return $ outputValue rec