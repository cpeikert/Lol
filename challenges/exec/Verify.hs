{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
             NoImplicitPrelude, PartialTypeSignatures, RebindableSyntax,
             RecordWildCards, ScopedTypeVariables #-}

module Verify where

import           Beacon
import           Common
import qualified Crypto.Lol.RLWE.Continuous as C
import qualified Crypto.Lol.RLWE.Discrete   as D
import qualified Crypto.Lol.RLWE.RLWR       as R

import Crypto.Challenges.RLWE.Proto.RLWE.Challenge
import Crypto.Challenges.RLWE.Proto.RLWE.ChallengeType
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR
import Crypto.Challenges.RLWE.Proto.RLWE.SampleCont
import Crypto.Challenges.RLWE.Proto.RLWE.SampleDisc
import Crypto.Challenges.RLWE.Proto.RLWE.SampleRLWR
import Crypto.Challenges.RLWE.Proto.RLWE.Secret

import Crypto.Lol             hiding (RRq, lift)
import Crypto.Lol.Types.Proto

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import           Data.List            (nub)

import Data.Reflection hiding (D)

import Net.Beacon

import System.Directory (doesFileExist)

-- Tensor type used to verify instances
type T = CT

-- | Verifies all instances in the challenge tree, given the path to the
-- root of the tree.
verifyMain :: (MonadIO m, MonadError String m) => FilePath -> m ()
verifyMain path = do
  -- get a list of challenges to reveal
  challNames <- challengeList path

  beaconAddrs <- mapM (readAndVerifyChallenge path) challNames

  -- verify that all beacon addresses are distinct
  printPassFail "Checking for distinct beacon addresses..." "DISTINCT" $
    throwErrorIf (length (nub beaconAddrs) /= length beaconAddrs) "NOT DISTINCT"

-- | Reads a challenge and verifies all instances.
-- Returns the beacon address for the challenge.
readAndVerifyChallenge :: (MonadIO m, MonadError String m)
  => FilePath -> String -> m BeaconAddr
readAndVerifyChallenge path challName = do
  (ba, insts) <- readChallenge path challName
  verifyChallenge challName insts
  return ba

-- | Verifies all instances that have a secret.
verifyChallenge :: (MonadIO m, MonadError String m)
                   => String -> [InstanceU] -> m ()
verifyChallenge challName insts =
  printPassFail ("Verifying challenge " ++ challName ++ "...") "VERIFIED" $
  mapM_ verifyInstanceU insts

-- | Read a challenge from a file, outputting the beacon address and a
-- list of instances to be verified.
readChallenge :: (MonadIO m, MonadError String m)
  => FilePath -> String -> m (BeaconAddr, [InstanceU])
readChallenge path challName = do
  let challFile = challFilePath path challName
  c <- readProtoType challFile
  isAvail <- isBeaconAvailable $ beaconEpoch c

  let (msg, readChall) =
        if isAvail
        then ("Past the beacon time: expecting one missing secret.",
              readSuppChallenge)
        else ("Before the beacon time: verifying all instances.",
              readFullChallenge)

  liftIO $ putStrLn msg
  readChall path challName c

readSuppChallenge, readFullChallenge :: (MonadIO m, MonadError String m)
  => FilePath -> String -> Challenge -> m (BeaconAddr, [InstanceU])

readSuppChallenge path challName Challenge{..} = do
  let numInsts' = fromIntegral numInstances
  beacon <- readBeacon path beaconEpoch
  let deletedID = suppressedSecretID numInstances beacon beaconOffset
  let delSecretFile = secretFilePath path challName deletedID
  delSecretExists <- liftIO $ doesFileExist delSecretFile
  throwErrorIf delSecretExists $
    "Secret " ++ show deletedID ++
    " should not exist, but it does! You may need to run the 'suppress' command."
  insts <- mapM (readInstanceU challType path challName challengeID) $
    filter (/= deletedID) $ take numInsts' [0..]
  checkParamsEq challName "numInstances" (numInsts'-1) (length insts)
  return (BA beaconEpoch beaconOffset, insts)

readFullChallenge path challName Challenge{..} = do
  let numInsts' = fromIntegral numInstances
  insts <- mapM (readInstanceU challType path challName challengeID) $ take numInsts' [0..]
  checkParamsEq challName "numInstances" numInsts' (length insts)
  return (BA beaconEpoch beaconOffset, insts)

-- | Read an 'InstanceU' from a file.
readInstanceU :: (MonadIO m, MonadError String m)
                 => ChallengeType -> FilePath -> String
                 -> ChallengeID -> InstanceID -> m InstanceU
readInstanceU challType path challName cid1 iid1 = do
  let secFile = secretFilePath path challName iid1
  sec@(Secret cid2 iid2 m q _) <- readProtoType secFile
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

checkParamsEq :: (Monad m, MonadError String m, Show a, Eq a)
  => String -> String -> a -> a -> m ()
checkParamsEq data' param expected actual =
  throwErrorUnless (expected == actual) $ "Error while reading " ++
    data' ++ ": " ++ param ++ " mismatch. Expected " ++
    show expected ++ " but got " ++ show actual

-- | Verify an 'InstanceU'.
verifyInstanceU :: (MonadError String m) => InstanceU -> m ()

verifyInstanceU (IC (Secret _ _ _ _ s) InstanceCont{..}) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' :: [C.Sample _ _ _ (RRq q)] <- fromProto $
        fmap (\(SampleCont a b) -> (a,b)) samples
      throwErrorUnless (validInstanceCont bound s' samples')
        "A continuous RLWE sample exceeded the error bound."))

verifyInstanceU (ID (Secret _ _ _ _ s) InstanceDisc{..}) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' <- fromProto $ fmap (\(SampleDisc a b) -> (a,b)) samples
      throwErrorUnless (validInstanceDisc bound s' samples')
        "A discrete RLWE sample exceeded the error bound."))

verifyInstanceU (IR (Secret _ _ _ _ s) InstanceRLWR{..}) =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) ->
      reify (fromIntegral p :: Int64) (\(_::Proxy p) -> do
        s' :: Cyc T m (Zq q) <- fromProto s
        samples' :: [R.Sample _ _ _ (Zq p)] <- fromProto $
          fmap (\(SampleRLWR a b) -> (a,b)) samples
        throwErrorUnless (validInstanceRLWR s' samples')
          "An RLWR sample was invalid.")))

-- | Read an XML file for the beacon corresponding to the provided time.
readBeacon :: (MonadIO m, MonadError String m)
              => FilePath -> BeaconEpoch -> m Record
readBeacon path time = do
  let file = xmlFilePath path time
  checkFileExists file
  rec' <- liftIO $ fromXML <$> BS.readFile file
  maybeThrowError rec' $ "Could not parse " ++ file

-- | Test if the 'gSqNorm' of the error for each RLWE sample in the
-- instance (given the secret) is less than the given bound.
validInstanceCont ::
  (C.RLWECtx t m zq rrq, Ord (LiftOf rrq), Ring (LiftOf rrq))
  => LiftOf rrq -> Cyc t m zq -> [C.Sample t m zq rrq] -> Bool
validInstanceCont bound s = all ((bound > ) . C.errorGSqNorm s)

-- | Test if the 'gSqNorm' of the error for each RLWE sample in the
-- instance (given the secret) is less than the given bound.
validInstanceDisc :: (D.RLWECtx t m zq)
                     => LiftOf zq -> Cyc t m zq -> [D.Sample t m zq] -> Bool
validInstanceDisc bound s = all ((bound > ) . D.errorGSqNorm s)

-- | Test if the given RLWR instance is valid for the given secret.
validInstanceRLWR :: (R.RLWRCtx t m zq zp, Eq zp)
  => Cyc t m zq -> [R.Sample t m zq zp] -> Bool
validInstanceRLWR s = let s' = adviseCRT s in all (validSampleRLWR s')

-- | Test if the given RLWR sample is valid for the given secret.
validSampleRLWR :: (R.RLWRCtx t m zq zp, Eq zp)
  => Cyc t m zq -> R.Sample t m zq zp -> Bool
validSampleRLWR s (a,b) = b == R.roundedProd s a
