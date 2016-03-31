
import Challenges.Beacon
import Challenges.Common
import Challenges.Verify

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.State

import Data.ByteString.Lazy (writeFile)
import Data.Map (Map, lookup, empty, insert)

import Net.Beacon

import Network.HTTP.Conduit (simpleHttp)

import Prelude hiding (lookup, writeFile)

import System.Directory (doesFileExist, removeFile, getDirectoryContents, doesDirectoryExist)
import System.IO hiding (writeFile)

main :: IO ()
main = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering

  abspath <- getPath
  let challDir = abspath </> challengeFilesDir
  challDirExists <- doesDirectoryExist challDir
  when (not challDirExists) $ error $ "Could not find " ++ challDir

  challs <- getChallengeList challDir

  -- reveal each challenge, collecting beacon records as we go
  recs <- flip execStateT empty $ mapM_ (revealChallengeMain abspath) challs
  -- write all beacon records
  mapM_ (writeBeaconXML abspath) recs
  -- write the NIST certificate
  getNistCert abspath

-- | Lookup the secret index based on the randomness for this challenge,
-- then remove the corresponding secret.
revealChallengeMain :: FilePath -> String -> StateT (Map Int Record) IO ()
revealChallengeMain abspath name = printPassFail ("Revealing challenge " ++ name ++ ":\n") "DONE" $ do
  let challDir = abspath </> challengeFilesDir </> name
  -- read the time/byteoffset of the randomness for this challenge
  (BP time offset) <- readRevealData challDir

  -- make sure it is past the reveal time
  lastRec <- liftIO getLastRecord
  lastBeaconTime <- case lastRec of
    Nothing -> throwError "Failed to get last beacon."
    (Just r) -> return $ timeStamp r
  when (time > lastBeaconTime) $
    throwError $ "Can't reveal challenge " ++ name ++ 
      ": it's time has not yet come. Please wait " ++ 
      (show $ time-lastBeaconTime) ++ " seconds for the assigned beacon."

  -- get the record, and compute the secret index
  rec <- retrieveRecord time
  let secretIdx = getSecretIdx rec offset
      secDir = abspath </> secretFilesDir </> name
      secFile = secDir </> (secretFileName name secretIdx)

  -- delete the secret corresponding to the secret index
  secFileExists <- liftIO $ doesFileExist secFile
  when (not secFileExists) $ throwError $ secFile ++ " does not exist."
  liftIO $ putStr $ "\tRemoving " ++ secFile ++ "\n\t"
  liftIO $ removeFile secFile

-- | Attempt to find the record in the state, otherwise download it from NIST.
retrieveRecord :: Int -> ExceptT String (StateT (Map Int Record) IO) Record
retrieveRecord t = do
  mrec <- gets (lookup t)
  case mrec of
    (Just r) -> return r
    Nothing -> do
      liftIO $ putStrLn $ "\tDownloading record " ++ (show t)
      r <- liftIO $ getCurrentRecord t
      case r of
        (Just r') -> do
          modify (insert t r')
          return r'
        Nothing -> throwError $ "Couldn't get record " ++ (show t) ++ "from NIST servers."

-- | Writes a beacon record to a file.
writeBeaconXML :: FilePath -> Record -> IO ()
writeBeaconXML path rec = do
  let beacon = toXML rec
  writeFile (path </> secretFilesDir </> (xmlFileName $ timeStamp rec)) beacon

-- | Downloads the NIST certificate and saves it.
getNistCert :: FilePath -> IO ()
getNistCert path = do
  let certPath = path </> secretFilesDir </> certFileName
  putStrLn $ "Writing NIST certificate to " ++ certPath
  bs <- simpleHttp "https://beacon.nist.gov/certificate/beacon.cer"
  writeFile certPath bs
  