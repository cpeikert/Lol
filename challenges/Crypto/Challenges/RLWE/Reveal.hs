module Crypto.Challenges.RLWE.Reveal (revealMain) where

import Control.Monad.Except
import Control.Monad.State

import Crypto.Challenges.RLWE.Beacon
import Crypto.Challenges.RLWE.Common
import Crypto.Challenges.RLWE.Proto.RLWE.Challenge

import Data.ByteString.Lazy (writeFile)
import Data.Map (Map, empty, insert, lookup)

import Net.Beacon

import Network.HTTP.Conduit (simpleHttp)

import Prelude hiding (lookup, writeFile)

import System.Directory (doesFileExist, removeFile)
import System.IO hiding (writeFile)

-- | Deletes the secret for each challenge in the tree, given the path
-- to the root of the tree.
revealMain :: FilePath -> IO ()
revealMain path = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering

  -- get a list of challenges to reveal
  challs <- challengeList path

  -- reveal each challenge, collecting beacon records as we go
  recs <- flip execStateT empty $ mapM_ (revealChallenge path) challs

  -- write all beacon records
  mapM_ (writeBeaconXML path) recs

  -- write the NIST certificate
  getNistCert path

-- EAC: Should we validate the challenge more?
-- We can check #instances, challID of secret, etc.

-- | A map from beacon times to beacon records.
type RecordState = Map Int Record

-- | Lookup the secret index based on the randomness for this challenge,
-- then remove the corresponding secret.
revealChallenge :: FilePath -> String -> StateT RecordState IO ()
revealChallenge path name =
  printPassFail ("Revealing challenge " ++ name ++ ":\n") "DONE" $ do
    -- read the beacon address of the randomness for this challenge
    let challFN = challFilePath path name
    challProto <- readProtoType challFN
    (BA time offset) <- parseBeaconAddr challProto
    let numInsts = fromIntegral $ numInstances challProto

    -- get the record, and compute the secret index
    rec <- retrieveRecord time
    let secIdx = secretIdx numInsts rec offset
        secFile = secretFilePath path name secIdx

    -- delete the secret corresponding to the secret index
    secFileExists <- liftIO $ doesFileExist secFile
    unless secFileExists $ throwError $ secFile ++ " does not exist."
    liftIO $ putStr $ "\tRemoving " ++ secFile ++ "\n\t"
    liftIO $ removeFile secFile

-- | Attempt to find the record in the state, otherwise download it from NIST.
retrieveRecord :: Int -> ExceptT String (StateT RecordState IO) Record
retrieveRecord t = do
  mrec <- gets (lookup t)
  case mrec of
    (Just r) -> return r
    Nothing -> do
      liftIO $ putStrLn $ "\tDownloading record " ++ (show t)
      -- make sure the beacon is available
      lastRec <- liftIO getLastRecord
      lastBeaconTime <- case lastRec of
        Nothing -> throwError "Failed to get last beacon."
        (Just r) -> return $ timeStamp r
      when (t > lastBeaconTime) $
        throwError $
          "Can't reveal challenge: it's time has not yet come. Please wait " ++
          (show $ t-lastBeaconTime) ++ " seconds for the assigned beacon."
      r <- liftIO $ getCurrentRecord t
      case r of
        (Just r') -> do
          modify (insert t r')
          return r'
        Nothing -> throwError $
          "Couldn't get record " ++ (show t) ++ "from NIST servers."

-- | Writes a beacon record to a file.
writeBeaconXML :: FilePath -> Record -> IO ()
writeBeaconXML path rec = do
  let beacon = toXML rec
      filePath = xmlFilePath path $ timeStamp rec
  writeFile filePath beacon

-- | Downloads the NIST certificate and saves it.
getNistCert :: FilePath -> IO ()
getNistCert path = do
  let certPath = certFilePath path
  putStrLn $ "Writing NIST certificate to " ++ certPath
  bs <- simpleHttp "https://beacon.nist.gov/certificate/beacon.cer"
  writeFile certPath bs

