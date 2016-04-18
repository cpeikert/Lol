
-- | Utility functions for working with the <http://www.nist.gov/itl/csd/ct/nist_beacon.cfm NIST Randomness Beacon>.

module Challenges.Beacon
( gmtDateToSeconds, localDateToSeconds
, advanceBeaconPos
, bytesPerBeacon
, beaconInterval
, BeaconPos(..)) where

import Control.DeepSeq
import Control.Monad.State
import Crypto.Hash.SHA512 (hash)
import Crypto.Lol (fromJust')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as SB (pack)
import qualified Data.ByteString.Builder as B

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..),secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.LocalTime (timeZoneMinutes, getCurrentTimeZone)
import Net.Beacon

-- | The number of bytes per beacon, equivalent to @512 / 8@.
bytesPerBeacon = 64 :: Int

-- | The number of seconds between beacon outputs.
beaconInterval = 60 :: Int

-- | Represents a byte offset in a beacon output at a particular time.
data BeaconPos = BP Int Int deriving (Eq, Show)
instance NFData BeaconPos where rnf (BP a b) = (rnf a) `seq` (rnf b)

-- | Advances the beacon position by one byte, overflowing to the next beacon in necessary.
advanceBeaconPos :: (Monad m) => StateT BeaconPos m ()
advanceBeaconPos = do
  (BP time byteOffset) <- get
  if byteOffset == bytesPerBeacon
  then put (BP (time+beaconInterval) 0)
  else put (BP time (byteOffset+1))

-- | The number of seconds elapsed from a given GMT time since the (GMT) epoch.
gmtDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> Int
gmtDateToSeconds month day year hour minute | hour >= 0 &&
                                           hour < 24 &&
                                           minute >= 0 &&
                                           minute < 60 = 
  round $ utcTimeToPOSIXSeconds $ UTCTime (fromGregorian year month day) (secondsToDiffTime $ fromIntegral $ 3600*hour+60*minute)
gmtDateToSeconds _ _ _ _ _ = error "invalid date to gmtDateToSeconds"

-- | The number of seconds elapsed from a given local time since the (GMT) epoch.
localDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> IO Int
localDateToSeconds month day year hour minute = do
  let gmt = gmtDateToSeconds month day year hour minute
  minuteOffset <- timeZoneMinutes <$> getCurrentTimeZone
  return $ gmt - (minuteOffset*60)

-- http://hackaday.com/2014/12/19/nist-randomness-beacon/
{-
verifySig :: X509 -> Record -> IO Bool
verifySig cert rec = do
  pk <- getPublicKey cert
  dgst <- fromJust' "Digest alg" <$> getDigestByName "sha512"
  -- the signature is stored in little-endian
  let sig = signatureValue rec
      revSig = B.toStrict $ B.pack $ reverse $ B.unpack sig
      msg = B.toStrict $ signedMsg rec
  vs <- verifyBS dgst revSig pk msg
  let vs' = case vs of
              VerifySuccess -> True
              VerifyFailure -> False
      expectedOutput = hash $ B.toStrict sig
      actualOutput = B.toStrict $ outputValue rec
  return $ vs' && (expectedOutput == actualOutput)

signedMsg :: Record -> B.ByteString
signedMsg rec = B.concat [
  SB.pack $ version rec,
  B.toLazyByteString $ B.word32BE $ fromIntegral $ frequency rec,
  B.toLazyByteString $ B.word64BE $ fromIntegral $ timeStamp rec,
  seedValue rec,
  previousOutputValue rec,
  B.toLazyByteString $ B.word32BE $ fromIntegral $ statusCode rec]
-}
