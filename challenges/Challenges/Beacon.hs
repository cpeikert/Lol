
module Challenges.Beacon
(getBeacon
,gmtDateToSeconds
,localDateToSeconds
,getBeaconPos
,advanceBeaconPos
,BeaconPos(..)
,bitsPerBeacon
,beaconInterval
,verifySig) where

import Control.DeepSeq
import Crypto.Hash.SHA512 (hash)
import Crypto.Lol (fromJust')
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as SB (pack)
import qualified Data.ByteString.Builder as B

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..),secondsToDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, POSIXTime)
import Data.Time.LocalTime
import Net.Beacon
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.EVP.PKey (toPublicKey)
import OpenSSL.EVP.Verify (verifyBS, VerifyStatus(..))
import OpenSSL.PEM (writePublicKey)
import OpenSSL.RSA
import OpenSSL.X509 (getPublicKey, getSubjectEmail, X509)

-- becaonTime
-- bitOffset
bitsPerBeacon = 512 :: Int
beaconInterval = 60 :: Int
data BeaconPos = BP Int Int deriving (Eq, Read, Show)
instance NFData BeaconPos where rnf (BP a b) = (rnf a) `seq` (rnf b)

-- returns the first available beacon position for k bits
getBeaconPos :: Int -> BeaconPos -> BeaconPos
getBeaconPos k bp@(BP time offset) = 
  if (offset+k) > bitsPerBeacon
  then BP (time+beaconInterval) 0
  else bp

advanceBeaconPos :: Int -> BeaconPos -> BeaconPos
advanceBeaconPos k (BP time offset) = BP time $ offset+k

-- returns seconds since epoch at midnight on the day/month/year/hour/minute of input
-- takes GMT!!
gmtDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> Int
gmtDateToSeconds month day year hour minute | hour >= 0 &&
                                           hour < 24 &&
                                           minute >= 0 &&
                                           minute < 60 = 
  round $ utcTimeToPOSIXSeconds $ UTCTime (fromGregorian year month day) (secondsToDiffTime $ fromIntegral $ 3600*hour+60*minute)
gmtDateToSeconds _ _ _ _ _ = error "invalid date to gmtDateToSeconds"

localDateToSeconds :: Int -> Int -> Integer -> Int -> Int -> IO Int
localDateToSeconds month day year hour minute = do
  let gmt = gmtDateToSeconds month day year hour minute
  minuteOffset <- timeZoneMinutes <$> getCurrentTimeZone
  return $ gmt - (minuteOffset*60)

-- takes seconds since epoch (GMT)
getBeacon :: Int -> IO ByteString
getBeacon time = B.toStrict <$> do
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

-- http://hackaday.com/2014/12/19/nist-randomness-beacon/
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
