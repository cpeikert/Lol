{-|
Module      : Crypto.RLWE.Challenges.Suppress
Description : Suppress a secret from a challenge.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Suppresses the instance secret for the official challenge instance.
-}

{-# LANGUAGE FlexibleContexts #-}

module Crypto.RLWE.Challenges.Suppress
(suppressMain, getNistCert, writeBeaconXML, suppressChallenge) where

import Crypto.RLWE.Challenges.Beacon
import Crypto.RLWE.Challenges.Common

import Control.Exception (try)
import Control.Monad.Except
import Control.Monad.State

import Crypto.Lol.Types.Proto
import Crypto.Proto.RLWE.Challenges.Challenge

import Data.ByteString.Lazy (writeFile)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (isNothing)
import Data.Time.Clock.POSIX

import Data.Map             (Map, empty, insert, lookup)

import Net.Beacon
import Network.HTTP.Client hiding (path)
import Network.HTTP.Conduit (simpleHttp)

import Prelude hiding (lookup, writeFile)

import System.Directory (removeFile)
import System.Exit

-- | Deletes the secret indicated by NIST beacon for each challenge in
-- the tree, given the path to the root of the tree.
suppressMain :: FilePath -> IO ()
suppressMain path = do
  -- get list of challenges
  challs <- challengeList path

  -- suppress a secret from each challenge, collecting beacon records as we go
  recs <- flip execStateT empty $ mapM_ (suppressChallenge path) challs

  -- write all beacon records
  mapM_ (writeBeaconXML path) recs

  -- write the NIST certificate
  getNistCert path

-- | A map from beacon times to beacon records.
type RecordState = Map BeaconEpoch Record

-- | Lookup the secret index based on the randomness for this challenge,
-- then remove the corresponding secret.
suppressChallenge :: (MonadIO m, MonadState RecordState m)
                     => FilePath -> String -> m ()
suppressChallenge path name = do
  x <- printPassFail ("Deleting secret for challenge " ++ name ++ ":\n") "DONE" $ do
    -- read the beacon address of the randomness for this challenge
    let challFN = challFilePath path name
    challProto <- readProtoType challFN
    (BA time offset) <- parseBeaconAddr challProto
    let numInsts = fromIntegral $ numInstances challProto

    -- get the record, and compute the secret index
    rec <- retrieveRecord time
    let secID = suppressedSecretID numInsts rec offset
        secFile = secretFilePath path name secID

    -- delete the secret corresponding to the secret index
    checkFileExists secFile
    liftIO $ putStr $ "\tRemoving " ++ secFile ++ "\n\t"
    liftIO $ removeFile secFile
  when (isNothing x) $ liftIO $ die "To avoid publishing all instance secrets, we are dying early."
  return ()

-- | Attempt to find the record in the state, otherwise download it from NIST.
retrieveRecord :: (MonadIO m, MonadError String m, MonadState RecordState m)
                  => BeaconEpoch -> m Record
retrieveRecord t = do
  mrec <- gets (lookup t)
  case mrec of
    (Just r) -> return r
    Nothing -> do
      liftIO $ putStrLn $ "\tDownloading record " ++ show t
      trec <- liftIO $ try $ getCurrentRecord $ fromIntegral t
      rec <- case trec of
               Left e -> catchHttpException t e
               Right a -> return a
      rec' <- maybeThrowError rec $ "Couldn't parse XML for beacon at time " ++ show t
      modify (insert t rec')
      return rec'

catchHttpException :: (MonadIO m, MonadError String m)
  => BeaconEpoch -> HttpException -> m a
catchHttpException t (HttpExceptionRequest _ (StatusCodeException _ s)) = do
  currTime <- round <$> (liftIO getPOSIXTime)
  throwError $ case currTime < t of
    True -> "You are requesting a beacon that doesn't exist yet.\n" ++
            "Wait another " ++ show (t-currTime) ++ " seconds and try again."
    False -> "The beacon you are requesting should be available, " ++
             "but it just isn't there:\n" ++ unpack s
catchHttpException _ (HttpExceptionRequest _ (ConnectionFailure _)) =
  throwError "Failed to connect to NIST servers. They might be down for maintenance."
catchHttpException _ _ =
  throwError "An unexpected IO error occurred while downloading the beacon."

-- | Writes a beacon record to a file.
writeBeaconXML :: (MonadIO m) => FilePath -> Record -> m ()
writeBeaconXML path rec = do
  let beacon = toXML rec
      filePath = beaconFilePath path $ fromIntegral $ timeStamp rec
  liftIO $ writeFile filePath beacon

-- | Downloads the NIST certificate and saves it.
getNistCert :: (MonadIO m) => FilePath -> m ()
getNistCert path = liftIO $ do
  let certPath = certFilePath path
  putStrLn $ "Writing NIST certificate to " ++ certPath
  bs <- simpleHttp "https://beacon.nist.gov/certificate/beacon.cer"
  writeFile certPath bs
