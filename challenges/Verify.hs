{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

import Challenges.Beacon
import Challenges.Common
import Challenges.ProtoReader
import qualified Challenges.Proto.LWEInstance as P
import qualified Challenges.Proto.LWESample as P
import qualified Challenges.Proto.LWESecret as P
import Challenges.Verify

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans (lift)

import Crypto.Lol hiding (lift)
import Crypto.Lol.Types.Proto (fromProto)

import qualified Data.ByteString.Lazy as BS
import Data.List (nub)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Reflection

import Net.Beacon

import OpenSSL (withOpenSSL)
import OpenSSL.PEM (readX509)

import System.Directory (doesFileExist, getDirectoryContents)
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
  when (all isJust bps) $ printPassFail "Checking for distinct beacon positions..." "DISTINCT" $ 
    when ((length $ nub bps) /= length bps) $ throwError "Beacon positions overlap"

verifyChallenge :: FilePath -> String -> IO (Maybe BeaconPos)
verifyChallenge path name = do
  let challPath = path </> challengeFilesDir </> name

  printPassFail ("Verifying challenge " ++ name ++ ":\n") "VERIFIED" $ do
    bp@(BP time offset) <- readRevealData challPath

    rec <- readBeacon path time

    let secretIdx = getSecretIdx rec offset
        instIDs = [0..(numInstances-1)]    

    mapM_ (verifyInstance path name secretIdx) instIDs
    lift $ putStr "\t"
    return $ Just bp

verifyInstance :: FilePath -> String -> Int -> Int -> ExceptT String IO ()
verifyInstance path challName secretID instID 
  | secretID == instID = do
    let secDir = path </> secretFilesDir </> challName
        secretName = secretFileName challName secretID
    secExists <- lift $ doesFileExist (secDir </> secretName)
    lift $ putStrLn $ "\tSecret for instance " ++ (show secretID) ++ " is suppressed."
    when secExists $ throwError $ "The secret index for challenge " ++ 
          challName ++ " is " ++ (show secretID) ++ ", but this secret is present!"
  | otherwise = do
    lift $ putStrLn $ "\tChecking instance " ++ (show instID)
    checkInstanceErr path challName instID
    

checkInstanceErr :: FilePath -> String -> Int -> ExceptT String IO ()
checkInstanceErr path challName instID = do
  let instFile = path </> challengeFilesDir </> challName </> (instFileName challName instID)
      secFile = path </> secretFilesDir </> challName </> (secretFileName challName instID)
  instFileExists <- lift $ doesFileExist instFile
  when (not instFileExists) $ throwError $ instFile ++ " does not exist."
  secFileExists <- lift $ doesFileExist secFile
  when (not secFileExists) $ throwError $ secFile ++ " does not exist."
  inst@(P.LWEInstance idx m q v _) <- lift $ messageGet' <$> BS.readFile instFile
  sec@(P.LWESecret idx' m' s) <- lift $ messageGet' <$> BS.readFile secFile
  when (idx /= idx') $ throwError $ "Instance ID is " ++ (show idx) ++ ", but secret ID is " ++ (show idx')
  when (m /= m') $ throwError $ "Instance index is " ++ (show m) ++ ", but secret index is " ++ (show m')
  reifyFactI (fromIntegral m) (\(_::proxy m) -> 
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      let (LWEInstance _ _ samples) = fromProto inst :: LWEInstance Double T m (ZqBasic q Int64)
          (LWESecret _ secret) = fromProto sec
      when (not $ checkInstance v secret samples) $ 
        throwError $ "Some sample in instance " ++ 
          (show instID) ++ " exceeded the noise bound."
      ))

messageGet' :: (ReflectDescriptor a, Wire a) => BS.ByteString -> a
messageGet' bs = 
  case messageGet bs of
    (Left str) -> error $ "when getting protocol buffer. Got string " ++ str
    (Right (a,bs')) -> 
      if BS.null bs'
      then a
      else error $ "when getting protocol buffer. There were leftover bits!"

readBeacon :: FilePath -> Int -> ExceptT String IO Record
readBeacon path time = do
  let file = path </> secretFilesDir </> xmlFileName time
  beaconExists <- lift $ doesFileExist file
  when (not beaconExists) $ throwError $ "Cannot find " ++ file
  rec' <- lift $ fromXML <$> BS.readFile file
  when (isNothing rec') $ throwError $ "Could not parse " ++ file
  return $ fromJust rec'
