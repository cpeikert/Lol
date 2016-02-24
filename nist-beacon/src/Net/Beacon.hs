{-|
Module      : Beacon
Description : Module for interacting with the NIST Randomness Beacon.
Copyright   : (c) Bryan St. Amour, 2015
License     : BSD
Maintainer  : bryan@bryanstamour.com
Stability   : experimental
Portability : POSIX

This module is for interacting with the NIST Randomness Beacon (prototype)
web API. The Randomness Beacon is designed to be a public source of random
information. It generates and publishes 512-bit blocks of random data collected
from a quantum mechanical process every 60 seconds.

For more information about the project, see

    https://beacon.nist.gov/home
-}


module Net.Beacon
       ( Timestamp
       , Record()
       , version
       , frequency
       , timeStamp
       , seedValue
       , previousOutputValue
       , signatureValue
       , outputValue
       , statusCode
       , getLastRecord
       , getCurrentRecord
       , getPreviousRecord
       , getNextRecord
       , getStartChainRecord
       ) where


import Control.Monad

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types

import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Numeric

-- | A single record: the random data plus some additional information.
data Record =
  Record
  { -- | A simple version string, e.g. "0.1.0".
    version :: String

    -- | The time interval, in seconds, between expected records.
  , frequency :: Int

    -- | The time the seed value was generated as the number of seconds since
    --   January 1, 1970.
  , timeStamp :: Int

    -- | A seed value represented as a 64 byte (512-bit) hex string value.
  , seedValue :: B.ByteString

    -- | The SHA-512 hash value for the previous record - 64 byte hex string.
  , previousOutputValue :: B.ByteString

    -- | A digital signature (RSA) computed over (in order):
    --   version, frequency, timeStamp, seedValue, previousHashValue,
    --   errorCode.
    --
    --   Note: Except for version, the hash is on the byte representations
    --   and not the string representations of the data values.
  , signatureValue :: B.ByteString

    -- | The SHA-512 hash of the signatureValue as a 64 byte hex string.
  , outputValue :: B.ByteString

    -- | The status code value:
    --     0 - Chain intact, values all good
    --     1 - Start of a new chain of values, previous hash value will be all
    --         zeroes
    --     2 - Time between values is greater than the frequency, but the
    --         chain is still intact
  , statusCode :: Int
  } deriving (Show, Eq)


type Timestamp = Int


-- | Last record published.
getLastRecord :: IO (Maybe Record)
getLastRecord = do
  x <- simpleHttp "https://beacon.nist.gov/rest/record/last"
  return $ getRecord x


-- | Current record, or closest to the timestamp.
getCurrentRecord :: Timestamp -> IO (Maybe Record)
getCurrentRecord ts = do
  x <- simpleHttp $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  return $ getRecord x


-- | Previous record.
getPreviousRecord :: Timestamp -> IO (Maybe Record)
getPreviousRecord ts = do
  x <- simpleHttp $ "https://beacon.nist.gov/rest/record/previous/" ++ (show ts)
  return $ getRecord x


-- | Next record.
getNextRecord :: Timestamp -> IO (Maybe Record)
getNextRecord ts = do
  x <- simpleHttp $ "https://beacon.nist.gov/rest/record/next/" ++ (show ts)
  return $ getRecord x


-- | Start chain record.
getStartChainRecord :: Timestamp -> IO (Maybe Record)
getStartChainRecord ts = do
  x <- simpleHttp $ "https://beacon.nist.gov/rest/record/start-chain/" ++ (show ts)
  return $ getRecord x


getRecord :: B.ByteString -> Maybe Record
getRecord stuff = do
  xml <- parseXMLDoc stuff
  let fc = findChild' xml
  Record
    <$>              fc "version"
    <*> (read    <$> fc "frequency")
    <*> (read    <$> fc "timeStamp")
    <*> (hexToBS <$> fc "seedValue")
    <*> (hexToBS <$> fc "previousOutputValue")
    <*> (hexToBS <$> fc "signatureValue")
    <*> (hexToBS <$> fc "outputValue")
    <*> (read    <$> fc "statusCode")
  where
    findChild' xml name = strContent <$> filterChildName ((name ==) . qName) xml

-- input: even-length string of hex characters
-- output: bytestring packed with the hex bits
-- e.g. B.unpack $ hexToBS "1011" = [16,17]
hexToBS :: String -> B.ByteString 
hexToBS = B.pack . go
  where go (a:b:xs) = 
          let parses = readHex [a,b]
          in case parses of
              [(val,"")] -> val:(go xs)
              _ -> error "parse error in hexToBS"
        go [] = []
        go _ = error "odd length input to hexToBS"