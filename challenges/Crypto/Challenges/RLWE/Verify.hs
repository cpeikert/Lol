{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Crypto.Challenges.RLWE.Verify where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans (lift)

import Crypto.Challenges.RLWE.Beacon
import Crypto.Challenges.RLWE.Common
import qualified Crypto.Challenges.RLWE.Continuous as C
import qualified Crypto.Challenges.RLWE.Discrete as D
import qualified Crypto.Challenges.RLWE.RLWR as R

import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont as P
import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc as P
import qualified Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR as P
import qualified Crypto.Challenges.RLWE.Proto.RLWE.Instance as P
import qualified Crypto.Challenges.RLWE.Proto.RLWE.Instance.InstType as P
import qualified Crypto.Challenges.RLWE.Proto.RLWE.Secret as P

import Crypto.Lol hiding (lift)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto

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
type RRq' q = RRq (RealMod (Reified q)) Double

main :: IO ()
main = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering

  abspath <- getPath
  let challDir = abspath </> challengeFilesDir
  challDirExists <- doesDirectoryExist challDir
  unless challDirExists $ error $ "Could not find " ++ challDir

  challs <- challengeList challDir

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

    let secretIdx = secretIdx rec offset
        instIDs = [0..(fromIntegral $ numInstances-1)]
    -- verify all instances
    lift $ mapM_ (verifyInstance path name secretIdx) instIDs
    -- verifyInstance prints out several progress statements
    -- so we need to indent to print the status
    lift $ putStr "\t"
    return $ Just bp

-- | Verifies an instance with a particular ID.
verifyInstance :: FilePath -> String -> Word32 -> Word32 -> IO ()
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
  CLWE :: (Protoable (RLWEInstanceCont v t m zq rq),
           ProtoType (RLWEInstanceCont v t m zq rq) ~ P.RLWEInstanceCont,
           C.CheckInst t m z zq rq, v ~ LiftOf rq)
    => RLWESecret t m z -> RLWEInstanceCont v t m zq rq -> Instance
  DLWE :: (Protoable (RLWEInstanceDisc v t m zq),
           ProtoType (RLWEInstanceDisc v t m zq) ~ P.RLWEInstanceDisc,
           D.CheckInst t m z zq)
    => RLWESecret t m z -> RLWEInstanceDisc v t m zq -> Instance
  LWR :: (Protoable (RLWRInstance t m zq zq'),
          ProtoType (RLWRInstance t m zq zq') ~ P.RLWRInstance,
          R.CheckInst t m z zq zq')
    => RLWESecret t m z -> RLWRInstance t m zq zq' -> Instance

readInstKeyPair :: FilePath -> String -> Word32 -> ExceptT String IO Instance
readInstKeyPair path challName instID = do
  let secFile = path </> secretFilesDir </> challName </> (secretFileName challName instID)
      instFile = path </> challengeFilesDir </> challName </> (instFileName challName instID)
  secFileExists <- lift $ doesFileExist secFile
  unless secFileExists $ throwError $ secFile ++ " does not exist."
  sec <- messageGet' =<< (lift $ BS.readFile secFile)
  instFileExists <- lift $ doesFileExist instFile
  unless instFileExists $ throwError $ instFile ++ " does not exist."
  inst <- messageGet' =<< (lift $ BS.readFile instFile)
  parseInstKeyPair sec inst

parseInstKeyPair :: (Monad m) => P.RLWESecret -> P.Instance -> ExceptT String m Instance
parseInstKeyPair sk@(P.RLWESecret idx' m' _) (P.Instance (Just (P.RlweInstCont inst@(P.RLWEInstanceCont idx m q _ _ _)))) = do
  checkParam "ContLWE" "index" m m'
  checkParam "ContLWE" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> return $ CLWE
      (fromProto sk :: RLWESecret T m Int64)
      (fromProto inst :: RLWEInstanceCont Double T m (Zq q) (RRq' q))))
parseInstKeyPair sk@(P.RLWESecret idx' m' _) (P.Instance (Just (P.RlweInstDisc inst@(P.RLWEInstanceDisc idx m q _ _ _)))) = do
  checkParam "DiscLWE" "index" m m'
  checkParam "DiscLWE" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> return $ DLWE
      (fromProto sk :: RLWESecret T m Int64)
      (fromProto inst :: RLWEInstanceDisc Double T m (Zq q))))
parseInstKeyPair sk@(P.RLWESecret idx' m' _) (P.Instance (Just (P.RlwrInst inst@(P.RLWRInstance idx m q q' _)))) = do
  checkParam "LWR" "index" m m'
  checkParam "LWR" "ID" idx idx'
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) ->
      reify (fromIntegral q' :: Int64) (\(_::Proxy q') -> return $ LWR
        (fromProto sk :: RLWESecret T m Int64)
        (fromProto inst :: RLWRInstance T m (Zq q) (Zq q')))))

checkParam :: (Monad m, Show a, Eq a) => String -> String -> a -> a -> ExceptT String m ()
checkParam instType paramType instparam secparam =
  unless (instparam == secparam) $ throwError $ "Parse error while reading " ++
    instType ++ " instance: instance " ++ paramType ++ " is " ++
    (show instparam) ++ " but secret index is " ++ (show secparam)



-- | Verifies an instance that has a corresponding secret.
checkInstErr :: Instance -> ExceptT String IO ()
checkInstErr (CLWE s inst) = unless (C.checkInstance s inst) $
  throwError $ "A CLWE sample exceeded the noise bound."
checkInstErr (DLWE s inst) = unless (D.checkInstance s inst) $
  throwError $ "A DLWE sample exceeded the noise bound."
checkInstErr (LWR s inst) = unless (R.checkInstance s inst) $
  throwError $ "An LWR sample exceeded the noise bound."

-- | Read an XML file for the beacon corresponding to the provided time.
readBeacon :: FilePath -> Int -> ExceptT String IO Record
readBeacon path time = do
  let file = path </> secretFilesDir </> xmlFileName time
  beaconExists <- lift $ doesFileExist file
  unless beaconExists $ throwError $ "Cannot find " ++ file
  rec' <- lift $ fromXML <$> BS.readFile file
  when (isNothing rec') $ throwError $ "Could not parse " ++ file
  return $ fromJust rec'
