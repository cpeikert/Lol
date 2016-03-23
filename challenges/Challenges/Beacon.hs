{-# LANGUAGE DeriveGeneric #-}

module Challenges.Beacon
(getBeacon
,gmtDateToSeconds
,localDateToSeconds
,advanceBeaconPos
,BeaconPos(..)
,bitsPerBeacon
,beaconInterval) where

import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Serialize
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..),secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime
import GHC.Generics (Generic)
import Net.Beacon

-- becaonTime
-- bitOffset
bitsPerBeacon = 512 :: Int
beaconInterval = 60 :: Int
data BeaconPos = BP Int Int deriving (Eq, Generic, Read, Show)
instance NFData BeaconPos where rnf (BP a b) = (rnf a) `seq` (rnf b)
instance Serialize BeaconPos -- use Generics

advanceBeaconPos :: Int -> BeaconPos -> BeaconPos
advanceBeaconPos bits (BP time offset) =
  let nextOffset = offset+bits
  -- we will use the time/offset from the state, unless there aren't enough bits left in this beacon
  in if nextOffset > bitsPerBeacon 
     then BP (time+beaconInterval) 0
     else BP time nextOffset

-- returns seconds since epoch at midnight on the day/month/year/hour/minute of input
-- takes GMT!!
gmtDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> Int
gmtDateToSeconds month day year hour minute | hour >= 0 &&
                                           hour < 24 &&
                                           minute >= 0 &&
                                           minute < 60 = 
  convert $ utcTimeToPOSIXSeconds $ UTCTime (fromGregorian year month day) (secondsToDiffTime $ fromIntegral $ 3600*hour+60*minute)
  where convert ptime =
          -- there has got to be a better way to get the seconds since the epoch as an Int... 
          let itime = read $ init $ show ptime
          in if (show ptime) == (show itime ++ "s")
             then itime
             else error $ "Unexpected format when showing a POSIXTime: expected " ++ 
                   (show itime) ++ "s. received: " ++ (show ptime)
gmtDateToSeconds _ _ _ _ _ = error "invalid date to gmtDateToSeconds"

localDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> IO Int
localDateToSeconds month day year hour minute = do
  let gmt = gmtDateToSeconds month day year hour minute
  minuteOffset <- timeZoneMinutes <$> getCurrentTimeZone
  return $ gmt - (minuteOffset*60)

-- takes seconds since epoch (GMT)
getBeacon :: Int -> IO ByteString
getBeacon time = toStrict <$> do
  lastTime <- maybe 
    (error "Could not get last record from the Beacon. \
           \Possible cause: the XML format has changed.")
    timeStamp <$> getLastRecord
  if time > lastTime
  then error $ "The requested time (" ++ (show time) ++ " seconds GMT) is after the \
               \last record available (" ++ (show lastTime) ++ " seconds GMT). \
               \Sit tight for another " ++ (show (time-lastTime)) ++ " seconds."
  else maybe (error "Could not get requested record from the Beacon. \
                    \Possible causes: the XML format has changed, \
                    \or the frequency is not a divisor of 60.") 
             outputValue <$> getCurrentRecord time