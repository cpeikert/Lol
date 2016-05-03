{-# LANGUAGE FlexibleContexts, GADTs, MultiParamTypeClasses,
             NoImplicitPrelude, PartialTypeSignatures, RebindableSyntax,
             RecordWildCards, ScopedTypeVariables #-}

module Verify where

import Beacon
import Common

import           Crypto.Lol                 hiding (RRq, lift)
import qualified Crypto.Lol.RLWE.Continuous as C
import qualified Crypto.Lol.RLWE.Discrete   as D
import qualified Crypto.Lol.RLWE.RLWR       as R
import           Crypto.Lol.Types.Proto

import Crypto.Proto.RLWE.Challenges.Challenge
import Crypto.Proto.RLWE.Challenges.Challenge.Params
import Crypto.Proto.RLWE.Challenges.ContParams
import Crypto.Proto.RLWE.Challenges.DiscParams
import Crypto.Proto.RLWE.Challenges.InstanceCont
import Crypto.Proto.RLWE.Challenges.InstanceDisc
import Crypto.Proto.RLWE.Challenges.InstanceRLWR
import Crypto.Proto.RLWE.Challenges.RLWRParams
import Crypto.Proto.RLWE.Challenges.Secret
import Crypto.Proto.RLWE.SampleCont
import Crypto.Proto.RLWE.SampleDisc
import Crypto.Proto.RLWE.SampleRLWR

import           Control.Applicative
import           Control.Monad.Except
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.List              (nub)
import           Data.Maybe
import           Data.Reflection hiding (D)

import Net.Beacon

import System.Directory (doesFileExist)

-- Tensor type used to verify instances
type T = CT

-- | Verifies all instances in the challenge tree, given the path to the
-- root of the tree.
verifyMain :: FilePath -> IO ()
verifyMain path = do
  -- get a list of challenges to reveal
  challNames <- challengeList path

  beaconAddrs <- sequence <$> mapM (readAndVerifyChallenge path) challNames

  -- verify that all beacon addresses are distinct
  case beaconAddrs of
    (Just addrs) -> do
      _ <- printPassFail "Checking for distinct beacon addresses... " "DISTINCT"
        $ throwErrorIf (length (nub addrs) /= length addrs) "NOT DISTINCT"
      return ()
    Nothing -> return ()

-- | Reads a challenge and verifies all instances.
-- Returns the beacon address for the challenge.
readAndVerifyChallenge :: (MonadIO m)
  => FilePath -> String -> m (Maybe BeaconAddr)
readAndVerifyChallenge path challName =
  printPassFail ("Verifying " ++ challName) "VERIFIED" $ do
    (ba, insts) <- readChallenge path challName
    mapM_ verifyInstanceU insts
    return ba

-- | Read a challenge from a file, outputting the beacon address and a
-- list of instances to be verified.
readChallenge :: (MonadIO m, MonadError String m)
  => FilePath -> String -> m (BeaconAddr, [InstanceU])
readChallenge path challName = do
  let challFile = challFilePath path challName
  c <- readProtoType challFile
  isAvail <- beaconAvailable path $ beaconEpoch c

  let (msg, readChall) =
        if isAvail
        then (" (expecting one missing secret)... ",
              readSuppChallenge)
        else (" (expecting all secrets)... ",
              readFullChallenge)

  liftIO $ putStr msg
  readChall path challName c

-- | Whether we have an XML file for the beacon at the given epoch.
beaconAvailable :: (MonadIO m) => FilePath -> BeaconEpoch -> m Bool
beaconAvailable path = liftIO . doesFileExist . beaconFilePath path

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
  throwErrorUnless (isJust params) $ "Challenge " ++ challName ++ " does not contain parameters."
  insts <- mapM (readInstanceU (fromJust params) path challName challengeID) $
    filter (/= deletedID) $ take numInsts' [0..]
  checkParamsEq challName "numInstances" (numInsts'-1) (length insts)
  return (BA beaconEpoch beaconOffset, insts)

readFullChallenge path challName Challenge{..} = do
  let numInsts' = fromIntegral numInstances
  throwErrorUnless (isJust params) $ "Challenge " ++ challName ++ " does not contain parameters."
  insts <- mapM (readInstanceU (fromJust params) path challName challengeID) $ take numInsts' [0..]
  checkParamsEq challName "numInstances" numInsts' (length insts)
  return (BA beaconEpoch beaconOffset, insts)

validateSecret :: (Monad m, MonadError String m)
  => String -> ChallengeID -> InstanceID -> Int32 -> Int64 -> Secret -> m ()
validateSecret sfile cid iid m q (Secret cid' iid' m' q' _) = do
  checkParamsEq sfile "challID" cid cid'
  checkParamsEq sfile "instID" iid iid'
  checkParamsEq sfile "m" m m'
  checkParamsEq sfile "q" q q'

validateInstance :: (Monad m, MonadError String m)
  => String -> ChallengeID -> InstanceID -> Params
            -> ChallengeID -> InstanceID -> Params -> m ()
validateInstance instFile cid iid params cid' iid' params' = do
  checkParamsEq instFile "challID" cid cid'
  checkParamsEq instFile "instID" iid iid'
  checkParamsEq instFile "params" params params'

-- | Read an 'InstanceU' from a file.
readInstanceU :: (MonadIO m, MonadError String m)
                 => Params -> FilePath -> String
                 -> ChallengeID -> InstanceID -> m InstanceU
readInstanceU params path challName cid iid = do
  let secFile = secretFilePath path challName iid
  s <- readProtoType secFile
  let instFile = instFilePath path challName iid
  case params of
    (Cparams ContParams{..}) -> do
      inst@(InstanceCont cid' iid' params' _) <- readProtoType instFile
      validateSecret secFile s m q cid iid
      validateInstance instFile cid iid params cid' iid' (Cparams params')
      return $ IC s inst
    (Dparams DiscParams{..}) -> do
      inst@(InstanceDisc cid' iid' params' _) <- readProtoType instFile
      validateSecret secFile s m q cid iid
      validateInstance instFile cid iid params cid' iid' (Dparams params')
      return $ ID s inst
    (Rparams RLWRParams{..}) -> do
      inst@(InstanceRLWR cid' iid' params' _) <- readProtoType instFile
      validateSecret secFile s m q cid iid
      validateInstance instFile cid iid params cid' iid' (Rparams params')
      return $ IR s inst

checkParamsEq :: (Monad m, MonadError String m, Show a, Eq a)
  => String -> String -> a -> a -> m ()
checkParamsEq data' param expected actual =
  throwErrorUnless (expected == actual) $ "Error while reading " ++
    data' ++ ": " ++ param ++ " mismatch. Expected " ++
    show expected ++ " but got " ++ show actual

-- | Verify an 'InstanceU'.
verifyInstanceU :: (MonadError String m) => InstanceU -> m ()

verifyInstanceU (IC (Secret _ _ _ _ s) inst@InstanceCont{..}) =
  let ContParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' :: [C.Sample _ _ _ (RRq q)] <- fromProto $
        fmap (\(SampleCont a b) -> (a,b)) samples
      throwErrorUnless (validInstanceCont bound s' samples')
        "A continuous RLWE sample exceeded the error bound."))

verifyInstanceU (ID (Secret _ _ _ _ s) inst@InstanceDisc{..}) =
  let DiscParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' <- fromProto $ fmap (\(SampleDisc a b) -> (a,b)) samples
      throwErrorUnless (validInstanceDisc bound s' samples')
        "A discrete RLWE sample exceeded the error bound."))

verifyInstanceU (IR (Secret _ _ _ _ s) inst@InstanceRLWR{..}) =
  let RLWRParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
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
  let file = beaconFilePath path time
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
