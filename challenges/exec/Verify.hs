{-# LANGUAGE FlexibleContexts, GADTs, NoImplicitPrelude, PartialTypeSignatures,
             RebindableSyntax, ScopedTypeVariables #-}

module Verify where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Trans (lift)

import Beacon
import Common
import qualified Crypto.Challenges.RLWE.Continuous as C
import qualified Crypto.Challenges.RLWE.Discrete as D
import qualified Crypto.Challenges.RLWE.RLWR as R

import Crypto.Challenges.RLWE.Proto.RLWE.Challenge
import Crypto.Challenges.RLWE.Proto.RLWE.ChallengeType
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR
import Crypto.Challenges.RLWE.Proto.RLWE.SampleCont
import Crypto.Challenges.RLWE.Proto.RLWE.SampleDisc
import Crypto.Challenges.RLWE.Proto.RLWE.SampleRLWR
import Crypto.Challenges.RLWE.Proto.RLWE.Secret

import Crypto.Lol hiding (lift, RRq)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Lol.Kq
import Crypto.Lol.Types.Proto.Lol.Rq

import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.List (nub)
import Data.Maybe (fromJust, isNothing, isJust)
import Data.Reflection hiding (D)

import Net.Beacon

import System.Directory (doesFileExist, getDirectoryContents, doesDirectoryExist)

import Text.ProtocolBuffers (messageGet)
import Text.ProtocolBuffers.Header (ReflectDescriptor, Wire)

-- Tensor type used to verify instances
type T = CT

-- | Verifies all instances in the challenge tree, given the path to the
-- root of the tree.
verifyMain :: FilePath -> IO ()
verifyMain path = do
  -- get a list of challenges to reveal
  challs <- challengeList path

  -- verifies challenges and accumulates beacon positions for each challenge
  beaconAddrs <- mapM (verifyChallenge path) challs

  -- verifies that all challenges use distinct random bits
  when (all isJust beaconAddrs) $ printPassFail "Checking for distinct beacon positions..." "DISTINCT" $
    throwErrorIf ((length $ nub beaconAddrs) /= length beaconAddrs) "Beacon positions overlap"

-- | Reads a challenge and verifies all instances that have a secret.
-- Returns the beacon address for the challenge.
verifyChallenge :: FilePath -> String -> IO (Maybe BeaconAddr)
verifyChallenge path challName = printPassFail ("Verifying challenge " ++ challName ++ ":\n") "DONE" $ do
  (beacon, insts) <- readChallenge path challName
  mapM_ verifyInstanceU insts
  return $ Just beacon

-- | Read and validate a challenge from a file. Outputs the beacon address for this
-- challenge and a list of instances to be verified.
readChallenge :: (MonadIO m) => FilePath -> String -> ExceptT String m (BeaconAddr, [InstanceU])
readChallenge path challName = do
  let challFile = challFilePath path challName
  chall@(Challenge ccid numInsts time offset challType) <- readProtoType challFile
  let numInsts' = fromIntegral numInsts
  beacon <- readBeacon path time
  let deletedID = secretIdx numInsts beacon offset
  let delSecretFile = secretFilePath path challName deletedID
  delSecretExists <- liftIO $ doesFileExist delSecretFile
  throwErrorIf delSecretExists $
    "Secret " ++ (show deletedID) ++
    " should not exist, but it does! You may need to run the 'reveal' phase."
  insts <- mapM (readInstanceU challType path challName ccid) $
    filter (/= deletedID) $ take numInsts' [0..]
  checkParamsEq challName "numInstances" (numInsts'-1) (length insts)
  return (BA time offset, insts)

-- | Read and validate an 'InstanceU' from a file.
readInstanceU :: (MonadIO m)
  => ChallengeType -> FilePath -> String -> Int32 -> Int32 -> ExceptT String m InstanceU
readInstanceU challType path challName cid1 iid1 = do
  let secFile = secretFilePath path challName iid1
  sec@(Secret cid2 iid2 m q s) <- readProtoType secFile
  checkParamsEq secFile "challID" cid1 cid2
  checkParamsEq secFile "instID" iid1 iid2
  let instFile = instFilePath path challName iid1
      validateParams cid' iid' m' q' = do
        checkParamsEq instFile "challID" cid1 cid'
        checkParamsEq instFile "instID" iid1 iid'
        checkParamsEq instFile "m" m m'
        checkParamsEq instFile "q" q q'
  case challType of
    Cont -> do
      inst@(InstanceCont cid' iid' m' q' _ _ _) <- readProtoType instFile
      validateParams cid' iid' m' q'
      return $ IC sec inst
    Disc -> do
      inst@(InstanceDisc cid' iid' m' q' _ _ _) <- readProtoType instFile
      validateParams cid' iid' m' q'
      return $ ID sec inst
    RLWR -> do
      inst@(InstanceRLWR cid' iid' m' q' _ _) <- readProtoType instFile
      validateParams cid' iid' m' q'
      return $ IR sec inst

checkParamsEq :: (Monad m, Show a, Eq a)
  => String -> String -> a -> a -> ExceptT String m ()
checkParamsEq data' param expected actual =
  throwErrorIfNot (expected == actual) $ "Parse error while reading " ++
    data' ++ ": " ++ param ++ " mismatch. Expected " ++
    (show expected) ++ " but got " ++ (show actual)

-- | Verify an instance from unstructure data.
verifyInstanceU :: (Monad m) => InstanceU -> ExceptT String m ()
verifyInstanceU (IC (Secret cid' iid' m' q' s) (InstanceCont cid iid m q svar bound samples)) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' :: [C.Sample _ _ _ (RRq q)] <- fromProto $
        fmap (\(SampleCont a b) -> (a,b)) samples
      throwErrorIfNot (C.validInstance bound s' samples') $
        "A LWEC sample exceeded the noise bound."))
verifyInstDisc (ID (Secret cid' iid' m' q' s) (InstanceDisc cid iid m q svar bound samples)) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' <- fromProto $ fmap (\(SampleDisc a b) -> (a,b)) samples
      throwErrorIfNot (D.validInstance bound s' samples') $
        "A LWED sample exceeded the noise bound."))
verifyInstRLWR (IR (Secret cid' iid' m' q' s) (InstanceRLWR cid iid m q p samples)) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) ->
      reify (fromIntegral p :: Int64) (\(_::Proxy p) -> do
        s' :: Cyc T m (Zq q) <- fromProto s
        samples' :: [R.Sample _ _ _ (Zq p)] <- fromProto $
          fmap (\(SampleRLWR a b) -> (a,b)) samples
        throwErrorIfNot (R.validInstance s' samples') $
          "A RLWR sample was invalid.")))

-- | Read an XML file for the beacon corresponding to the provided time.
readBeacon :: (MonadIO m) => FilePath -> Int64 -> ExceptT String m Record
readBeacon path time = do
  let file = xmlFilePath path time
  checkFileExists file
  rec' <- liftIO $ fromXML <$> BS.readFile file
  maybeThrowError rec' $ "Could not parse " ++ file
