{-# LANGUAGE FlexibleContexts, GADTs #-}

import Challenges.Beacon
import Challenges.Common
import Challenges.LWE
import Challenges.ProtoReader
import Challenges.Verify

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans

import Crypto.Lol (CT,RT,fromJust',intLog)
import Crypto.Lol.Types.Proto

import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe

import Net.Beacon

import OpenSSL (withOpenSSL)
import OpenSSL.PEM (readX509)

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.IO

import Text.ProtocolBuffers (messageGet)
import Text.ProtocolBuffers.Header (ReflectDescriptor, Wire)

-- Tensor type used to verify instances
type T = CT

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  checkChallDirExists

  abspath <- absPath
  let challDir = abspath </> challengeFilesDir
  challs <- filter (("chall" ==) . (take 5)) <$> (getDirectoryContents challDir)

  bps <- mapM (verifyChallenge abspath) challs
  when (all isJust bps) $ printPassFail "Checking random bits..." $ 
    when (not $ checkRandomBits $ map fromJust bps) $ throwError "Random bits overlap"

checkRandomBits :: [(BeaconPos, Int)] -> Bool
checkRandomBits bps =
  let expectedNumBits = sum $ map snd bps
      toBPList (BP t offset) numBits = map (BP t) $ take numBits $ [offset..]
      allBPs = nub $ concatMap (uncurry toBPList) bps
  in expectedNumBits == length allBPs

verifyChallenge :: FilePath -> String -> IO (Maybe (BeaconPos, Int))
verifyChallenge path name = do
  let challPath = path </> challengeFilesDir </> name

  printPassFail ("Verifying challenge " ++ name ++ ":\n") $ do
    numInsts <- lift $ length <$> filter (("instance" ==) . (take 8)) <$> (getDirectoryContents challPath)
    let numBits = intLog 2 numInsts
    bp@(BP time offset) <- readRevealData challPath

    rec <- readAndVerifyBeacon path time

    let secretIdx = getSecretIdx rec offset numBits
        instIDs = [0..(numInsts-1)]    

    mapM_ (verifyInstance path name secretIdx) instIDs
    return $ Just (bp,numBits)

verifyInstance :: FilePath -> String -> Int -> Int -> ExceptT String IO ()
verifyInstance path challName secretID instID 
  | secretID == instID = do
    let secDir = path </> secretFilesDir </> challName
        secretName = secretFileName secretID
    secExists <- lift $ doesFileExist (secDir </> secretName)
    lift $ putStrLn $ "\tInstance " ++ (show secretID) ++ " is secret"
    when secExists $ throwError $ "The secret index for challenge " ++ 
          challName ++ " is " ++ (show secretID) ++ ", but this secret is present!"
  | otherwise = do
    (InstanceWithSecret idx m p v secret samples) <- readInstance path challName instID
    lift $ putStrLn $ "\tChecking instance " ++ (show instID)
    when (not $ checkInstance v secret samples) $ 
      throwError $ "Some sample in instance " ++ (show instID) ++ " exceeded the noise bound."

readInstance :: FilePath -> String -> Int -> ExceptT String IO (InstanceWithSecret T)
readInstance path challName instID = do
  let instFile = path </> challengeFilesDir </> challName </> (instFileName instID)
      secFile = path </> secretFilesDir </> challName </> (secretFileName instID)
  instFileExists <- lift $ doesFileExist instFile
  when (not instFileExists) $ throwError $ instFile ++ " does not exist."
  secFileExists <- lift $ doesFileExist secFile
  when (not secFileExists) $ throwError $ secFile ++ " does not exist."
  pinst <- lift $ messageGet' <$> BS.readFile instFile
  psec <- lift $ messageGet' <$> BS.readFile secFile
  return $ fromProto (psec, pinst)

messageGet' :: (ReflectDescriptor a, Wire a) => BS.ByteString -> a
messageGet' bs = 
  case messageGet bs of
    (Left str) -> error $ "when getting protocol buffer. Got string " ++ str
    (Right (a,bs')) -> 
      if BS.null bs'
      then a
      else error $ "when getting protocol buffer. There were leftover bits!"

readAndVerifyBeacon :: FilePath -> Int -> ExceptT String IO Record
readAndVerifyBeacon path time = do
  lift $ putStrLn "\tVerifying beacon..."
  let file = path </> secretFilesDir </> xmlFileName time
  beaconExists <- lift $ doesFileExist file
  when (not beaconExists) $ throwError $ "Cannot find " ++ file
  rec <- lift $ fromJust' "NIST getCurrentRecord" <$> fromXML <$> BS.readFile file
  res <- lift $ withOpenSSL $ do
    cert <- readX509 =<< (readFile $ path </> secretFilesDir </> certFileName)
    verifySig cert rec
  when (not res) $ throwError $ "Signature verification of " ++ file ++ " failed."
  return rec
