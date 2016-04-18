{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

import Challenges.Beacon
import Challenges.Common

import Challenges.ContinuousLWE.Proto
import qualified Challenges.ContinuousLWE.Verify as C
import Challenges.DiscretizedLWE.Proto
import qualified Challenges.DiscretizedLWE.Verify as D
import Challenges.LWR.Proto
import qualified Challenges.LWR.Verify as R
import qualified Challenges.Proto.ContLWEInstance as P
import qualified Challenges.Proto.DiscLWEInstance as P
import qualified Challenges.Proto.Instance as P
import qualified Challenges.Proto.InstType as P
import qualified Challenges.Proto.LWESecret as P
import qualified Challenges.Proto.LWRInstance as P

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans (lift)

import Crypto.Lol hiding (lift)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import qualified Crypto.Lol.Types.RRq as RRq

import qualified Data.ByteString.Lazy as BS
import Data.List (nub)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Reflection
import Data.Word

import Net.Beacon

import System.Directory (doesFileExist, getDirectoryContents, doesDirectoryExist)
import System.IO

import Text.ProtocolBuffers (messageGet)
import Text.ProtocolBuffers.Header (ReflectDescriptor, Wire)

-- Tensor type used to verify instances
type T = CT
type Zq q = ZqBasic (Reified q) Int64
type RRq q = RRq.RRq (RealMod (Reified q)) Double

main :: IO ()
main = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering

  abspath <- getPath
  let challDir = abspath </> challengeFilesDir
  challDirExists <- doesDirectoryExist challDir
  when (not challDirExists) $ error $ "Could not find " ++ challDir

  challs <- getChallengeList challDir

  -- verifies challenges and accumulates beacon positions for each challenge
  bps <- mapM (verifyChallenge abspath) challs
  -- verifies that all challenges use distinct random bits
  when (all isJust bps) $ printPassFail "Checking for distinct beacon positions..." "DISTINCT" $ 
    when ((length $ nub bps) /= length bps) $ throwError "Beacon positions overlap"

-- | Verifies all instances that have a secret and return the beacon position for the challenge.
verifyChallenge :: FilePath -> String -> IO (Maybe BeaconPos)
verifyChallenge path name = do
  let challPath = path </> challengeFilesDir </> name

  printPassFail ("Verifying challenge " ++ name ++ ":\n") "DONE" $ do
    bp@(BP time offset) <- readRevealData challPath
    -- read the beacon record from an xml file
    rec <- readBeacon path time

    let secretIdx = getSecretIdx rec offset
        instIDs = [0..(numInstances-1)]    
    -- verify all instances
    lift $ mapM_ (verifyInstance path name secretIdx) instIDs
    -- verifyInstance prints out several progress statements
    -- so we need to indent to print the status
    lift $ putStr "\t"
    return $ Just bp

-- | Verifies an instance with a particular ID.
verifyInstance :: FilePath -> String -> Int -> Int -> IO ()
verifyInstance path challName secretID instID 
  | secretID == instID = printPassFail ("\tSecret for instance " ++ (show secretID) ++ " is suppressed.\t") "" $ do
    let secDir = path </> secretFilesDir </> challName
        secretName = secretFileName challName secretID
    secExists <- lift $ doesFileExist (secDir </> secretName)
    when secExists $ throwError $ "The secret index for challenge " ++ 
          challName ++ " is " ++ (show secretID) ++ ", but this secret is present!"
  | otherwise = printPassFail ("\tChecking instance " ++ (show instID) ++ "\t") "VERIFIED" $ do
    inst <- readInstKeyPair path challName instID
    checkInstErr inst

data Instance where
  CLWE :: (Protoable (ContLWEInstance v t m zq rq), 
           ProtoType (ContLWEInstance v t m zq rq) ~ P.ContLWEInstance,
           C.CheckInst t m z zq rq, v ~ LiftOf rq) 
    => LWESecret t m z -> ContLWEInstance v t m zq rq -> Instance
  DLWE :: (Protoable (DiscLWEInstance v t m zq), 
           ProtoType (DiscLWEInstance v t m zq) ~ P.DiscLWEInstance,
           D.CheckInst t m z zq) 
    => LWESecret t m z -> DiscLWEInstance v t m zq -> Instance
  LWR :: (Protoable (LWRInstance t m zq zq'), 
          ProtoType (LWRInstance t m zq zq') ~ P.LWRInstance, 
          R.CheckInst t m z zq zq')
    => LWESecret t m z -> LWRInstance t m zq zq' -> Instance

readInstKeyPair :: FilePath -> String -> Int -> ExceptT String IO Instance
readInstKeyPair path challName instID = do
  let secFile = path </> secretFilesDir </> challName </> (secretFileName challName instID)
      instFile = path </> challengeFilesDir </> challName </> (instFileName challName instID)
  secFileExists <- lift $ doesFileExist secFile
  when (not secFileExists) $ throwError $ secFile ++ " does not exist."
  sec <- messageGet' =<< (lift $ BS.readFile secFile)
  instFileExists <- lift $ doesFileExist instFile
  when (not instFileExists) $ throwError $ instFile ++ " does not exist."
  inst <- messageGet' =<< (lift $ BS.readFile instFile)
  parseInstKeyPair sec inst

parseInstKeyPair :: (Monad m) => P.LWESecret -> P.Instance -> ExceptT String m Instance
parseInstKeyPair sk@(P.LWESecret idx' m' _) (P.Instance (Just (P.Clweinst inst@(P.ContLWEInstance idx m q _ _ _)))) = do
  checkParam "ContLWE" "index" m m'
  checkParam "ContLWE" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) -> 
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> return $ CLWE
      (fromProto sk :: LWESecret T m Int64)
      (fromProto inst :: ContLWEInstance Double T m (Zq q) (RRq q))))
parseInstKeyPair sk@(P.LWESecret idx' m' _) (P.Instance (Just (P.Dlweinst inst@(P.DiscLWEInstance idx m q _ _ _)))) = do
  checkParam "DiscLWE" "index" m m'
  checkParam "DiscLWE" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) -> 
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> return $ DLWE
      (fromProto sk :: LWESecret T m Int64)
      (fromProto inst :: DiscLWEInstance Double T m (Zq q))))
parseInstKeyPair sk@(P.LWESecret idx' m' _) (P.Instance (Just (P.Lwrinst inst@(P.LWRInstance idx m q q' _)))) = do
  checkParam "LWR" "index" m m'
  checkParam "LWR" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) -> 
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> 
      reify (fromIntegral q' :: Int64) (\(_::Proxy q') -> return $ LWR
        (fromProto sk :: LWESecret T m Int64)
        (fromProto inst :: LWRInstance T m (Zq q) (Zq q')))))

checkParam :: (Monad m, Show a, Eq a) => String -> String -> a -> a -> ExceptT String m ()
checkParam instType paramType instparam secparam = do
  when (instparam /= secparam) $ throwError $ "Parse error while reading " ++ 
    instType ++ " instance: instance " ++ paramType ++ " is " ++ 
    (show instparam) ++ " but secret index is " ++ (show secparam)

-- | Reads a serialized protobuffer file to the unparameterized proto type.
messageGet' :: (ReflectDescriptor a, Wire a, Monad m) => BS.ByteString -> ExceptT String m a
messageGet' bs = 
  case messageGet bs of
    (Left str) -> throwError $ "Error when reading from protocol buffer. Got string " ++ str
    (Right (a,bs')) -> 
      if BS.null bs'
      then return a
      else throwError $ "Error when reading from protocol buffer. There were leftover bits!"

-- | Verifies an instance that has a corresponding secret.
checkInstErr :: Instance -> ExceptT String IO ()
checkInstErr (CLWE s inst) = when (not $ C.checkInstance s inst) $
  throwError $ "A CLWE sample exceeded the noise bound."
checkInstErr (DLWE s inst) = when (not $ D.checkInstance s inst) $
  throwError $ "A DLWE sample exceeded the noise bound."
checkInstErr (LWR s inst) = when (not $ R.checkInstance s inst) $
  throwError $ "An LWR sample exceeded the noise bound."

-- | Read an XML file for the beacon corresponding to the provided time.
readBeacon :: FilePath -> Int -> ExceptT String IO Record
readBeacon path time = do
  let file = path </> secretFilesDir </> xmlFileName time
  beaconExists <- lift $ doesFileExist file
  when (not beaconExists) $ throwError $ "Cannot find " ++ file
  rec' <- lift $ fromXML <$> BS.readFile file
  when (isNothing rec') $ throwError $ "Could not parse " ++ file
  return $ fromJust rec'
