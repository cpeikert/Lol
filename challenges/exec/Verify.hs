{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Verify where

import Beacon
import Common
import Generate

import           Crypto.Lol
import           Crypto.Lol.Cyclotomic.UCyc
import qualified Crypto.Lol.RLWE.Continuous as C
import qualified Crypto.Lol.RLWE.Discrete   as D
import qualified Crypto.Lol.RLWE.RLWR       as R
import           Crypto.Lol.Types.Proto
import           Crypto.Lol.Types.Random

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

import Crypto.Random.DRBG

import           Control.Applicative
import           Control.Monad.Except hiding (lift)
import           Control.Monad.Random
import qualified Data.ByteString.Lazy as BS
import           Data.Int
import           Data.List            (nub)
import           Data.Maybe
import           Data.Reflection      hiding (D)
import qualified Data.Tagged          as T

import Net.Beacon

import System.Console.ANSI
import System.Directory    (doesFileExist)

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
      putStrLn "\nAttempting to regenerate challenges from random seeds. This will take awhile..."
      regens <- sequence <$> mapM (regenChallenge path) challNames
      when (isNothing regens) $ printANSI Yellow "NOTE: one or more instances could not be\n \
        \regenerated from the provided PRG seed. This is NON-FATAL,\n \
        \and is likely due to the use of a different compiler/platform\n \
        \than the one used to generate the challenges."
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

-- | Reads a challenge and attempts to regenerate all instances from the
-- provided seed.
-- Returns (Just ()) if regeneration succeeded for all instances.
regenChallenge :: (MonadIO m)
  => FilePath -> String -> m (Maybe ())
regenChallenge path challName = do
  printPassWarn ("Regenerating " ++ challName ++ "... ") "VERIFIED" $ do
    (_, insts) <- readChallenge path challName
    regens <- mapM regenInstance insts
    unless (and regens) $ throwError "UNSUCCESSFUL"

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
  _ <- parseBeaconAddr c -- verify that the beacon address is valid
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

validateSecret :: (MonadError String m)
  => String -> ChallengeID -> InstanceID -> Int32 -> Int64 -> Secret -> m ()
validateSecret sfile cid iid m q (Secret cid' iid' m' q' seed _) = do
  checkParamsEq sfile "challID" cid cid'
  checkParamsEq sfile "instID" iid iid'
  checkParamsEq sfile "m" m m'
  checkParamsEq sfile "q" q q'
  let minSeedLen = fromIntegral $ T.proxy genSeedLength (Proxy::Proxy InstDRBG)
      seedLen = length $ BS.unpack seed
  throwErrorIf (seedLen < minSeedLen) $ "Seed length is too short! Expected at least " ++
    show minSeedLen ++ " bytes, but only found " ++ show seedLen ++ " bytes."

validateInstance :: (MonadError String m)
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
      validateSecret secFile cid iid m q s
      validateInstance instFile cid iid params cid' iid' (Cparams params')
      return $ IC s inst
    (Dparams DiscParams{..}) -> do
      inst@(InstanceDisc cid' iid' params' _) <- readProtoType instFile
      validateSecret secFile cid iid m q s
      validateInstance instFile cid iid params cid' iid' (Dparams params')
      return $ ID s inst
    (Rparams RLWRParams{..}) -> do
      inst@(InstanceRLWR cid' iid' params' _) <- readProtoType instFile
      validateSecret secFile cid iid m q s
      validateInstance instFile cid iid params cid' iid' (Rparams params')
      return $ IR s inst

checkParamsEq :: (MonadError String m, Show a, Eq a)
  => String -> String -> a -> a -> m ()
checkParamsEq data' param expected actual =
  throwErrorUnless (expected == actual) $ "Error while reading " ++
    data' ++ ": " ++ param ++ " mismatch. Expected " ++
    show expected ++ " but got " ++ show actual

-- | Outputs whether or not we successfully regenerated this instance from the DRBG seed.
regenInstance :: (MonadError String m) => InstanceU -> m Bool
-- as always with floating point arithmetic, nothing is perfect (even
-- deterministic generation of instances).
-- the secret and a_i are discrete, so they should match exactly.
-- the b_i shouldn't be too far off.
regenInstance (IC (Secret _ _ _ _ seed s) InstanceCont{..}) =
  let ContParams {..} = params
      (Right (g :: CryptoRand InstDRBG)) = newGen $ BS.toStrict seed
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
      reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
        let (expectedS, expectedSamples :: [C.Sample T m (Zq q) (RRq q)]) =
              flip evalRand g $ instanceCont svar (fromIntegral numSamples)
            csampleEq (a,b) (a',b') =
              (a == a') && maximum (fmapDec abs $ lift $ b-b') < 2 ^- (-20)
        s' :: Cyc T m (Zq q) <- fromProto s
        samples' :: [C.Sample _ _ _ (RRq q)] <- fromProto $
          fmap (\(SampleCont a b) -> (a,b)) samples
        return $ (expectedS == s') && (and $ zipWith csampleEq expectedSamples samples')))

regenInstance (ID (Secret _ _ _ _ seed s) InstanceDisc{..}) =
  let DiscParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      g :: CryptoRand InstDRBG <- either (throwError . show) return $ newGen $ BS.toStrict seed
      let (expectedS, expectedSamples :: [D.Sample T m (Zq q)]) =
            flip evalRand g $ instanceDisc svar (fromIntegral numSamples)
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' <- fromProto $ fmap (\(SampleDisc a b) -> (a,b)) samples
      return $ (expectedS == s') && (expectedSamples == samples')))

regenInstance (IR (Secret _ _ _ _ seed s) InstanceRLWR{..}) =
  let RLWRParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) ->
      reify (fromIntegral p :: Int64) (\(_::Proxy p) -> do
        g :: CryptoRand InstDRBG <- either (throwError . show) return $ newGen $ BS.toStrict seed
        let (expectedS, expectedSamples :: [R.Sample T m (Zq q) (Zq p)]) =
              flip evalRand g $ instanceRLWR (fromIntegral numSamples)
        s' :: Cyc T m (Zq q) <- fromProto s
        samples' :: [R.Sample _ _ _ (Zq p)] <- fromProto $
          fmap (\(SampleRLWR a b) -> (a,b)) samples
        return $ (expectedS == s') && (expectedSamples == samples'))))


-- | Verify an 'InstanceU'.
verifyInstanceU :: (MonadError String m) => InstanceU -> m ()

verifyInstanceU (IC (Secret _ _ _ _ _ s) InstanceCont{..}) =
  let ContParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' :: [C.Sample _ _ _ (RRq q)] <- fromProto $
        fmap (\(SampleCont a b) -> (a,b)) samples
      throwErrorUnless (validInstanceCont bound s' samples')
        "A continuous RLWE sample exceeded the error bound."))

verifyInstanceU (ID (Secret _ _ _ _ _ s) InstanceDisc{..}) =
  let DiscParams {..} = params
  in reifyFactI (fromIntegral m) (\(_::proxy m) ->
    reify (fromIntegral q :: Int64) (\(_::Proxy q) -> do
      s' :: Cyc T m (Zq q) <- fromProto s
      samples' <- fromProto $ fmap (\(SampleDisc a b) -> (a,b)) samples
      throwErrorUnless (validInstanceDisc bound s' samples')
        "A discrete RLWE sample exceeded the error bound."))

verifyInstanceU (IR (Secret _ _ _ _ _ s) InstanceRLWR{..}) =
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
