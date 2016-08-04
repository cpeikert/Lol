{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Generate (generateMain) where

import Beacon
import Common
import Params as P

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.RLWE.Continuous           as C
import Crypto.Lol.RLWE.Discrete             as D
import Crypto.Lol.RLWE.RLWR                 as R
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Random

import Crypto.Proto.RLWE.Challenges.Challenge
import Crypto.Proto.RLWE.Challenges.Challenge.Params
import Crypto.Proto.RLWE.Challenges.ContParams
import Crypto.Proto.RLWE.Challenges.DiscParams
import Crypto.Proto.RLWE.Challenges.InstanceCont
import Crypto.Proto.RLWE.Challenges.InstanceDisc
import Crypto.Proto.RLWE.Challenges.InstanceRLWR
import Crypto.Proto.RLWE.Challenges.RLWRParams
import Crypto.Proto.RLWE.Challenges.Secret           as S
import Crypto.Proto.RLWE.SampleCont
import Crypto.Proto.RLWE.SampleDisc
import Crypto.Proto.RLWE.SampleRLWR

import Crypto.Random.DRBG

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random

import Data.ByteString.Lazy as BS (writeFile)
import Data.Reflection      hiding (D)

import Prelude ((^^))

import System.Directory (createDirectoryIfMissing)

import Text.ProtocolBuffers        (messagePut)
import Text.ProtocolBuffers.Header

-- Tensor type used to generate instances
type T = CT

-- | Generate and serialize challenges given the path to the root of the tree
-- and an initial beacon address.
generateMain :: FilePath -> BeaconAddr -> [ChallengeParams] -> IO ()
generateMain path beaconStart cps = do
  let len = length cps
      challIDs = take len [0..]
      beaconAddrs = take len $ iterate nextBeaconAddr beaconStart
  evalCryptoRandIO (sequence_ $
    zipWith3 (genAndWriteChallenge path) cps challIDs beaconAddrs
    :: RandT (CryptoRand HashDRBG) IO ())

genAndWriteChallenge :: (MonadRandom m, MonadIO m)
  => FilePath -> ChallengeParams -> ChallengeID -> BeaconAddr -> m ()
genAndWriteChallenge path cp challID ba@(BA _ _) = do
  let name = challengeName challID cp
  liftIO $ putStrLn $ "Generating challenge " ++ name

  -- CJP: not printing warning because it's annoying to implement
  -- correctly: dont want to trust local time, don't want to rely on
  -- network when generating

  -- isAvail <- isBeaconAvailable t
  -- when isAvail $ printANSI Red "Beacon is already available!"

  chall <- genChallengeU cp challID ba
  liftIO $ writeChallengeU path name chall

-- | The name for each challenge directory.
challengeName :: ChallengeID -> ChallengeParams -> FilePath
challengeName challID params =
  "chall-id" ++ show challID ++
  (case params of
     C{..} -> "-rlwec-m" ++ show m ++ "-q" ++ show q ++ "-v" ++ show (roundPrec svar 3)
     D{..} -> "-rlwed-m" ++ show m ++ "-q" ++ show q ++ "-v" ++ show (roundPrec svar 3)
     R{..} -> "-rlwr-m" ++ show m ++ "-q" ++ show q ++ "-p" ++ show p)
  ++ "-l" ++ show (P.numSamples params)
  where roundPrec :: Double -> Int -> Double
        roundPrec f n = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

-- | Generate a challenge with the given parameters.
genChallengeU :: (MonadRandom rnd)
  => ChallengeParams -> ChallengeID -> BeaconAddr -> rnd ChallengeU
genChallengeU cp challengeID (BA beaconEpoch beaconOffset) = do
  let params' = toProtoParams cp
      numInstances = P.numInstances cp
      chall = Challenge{params=Just params',..}
      instIDs = take (fromIntegral numInstances) [0..]
  insts <- mapM (genInstanceU params' challengeID) instIDs
  return $ CU chall insts

-- | Generate an instance for the given parameters.
genInstanceU :: (MonadRandom rnd)
  => Params -> ChallengeID -> InstanceID -> rnd InstanceU

genInstanceU (Cparams params@ContParams{..}) challengeID instanceID =
  reify q (\(_::Proxy q) ->
    reifyFactI (fromIntegral m) (\(_::proxy m) -> do
      (s', samples' :: [C.Sample T m (Zq q) (RRq q)]) <- instanceCont svar $ fromIntegral numSamples
      let s'' = Secret{s = toProto s', ..}
          samples = (uncurry SampleCont) <$> (toProto samples')
      return $ IC s'' InstanceCont{..}))

genInstanceU (Dparams params@DiscParams{..}) challengeID instanceID =
  reify q (\(_::Proxy q) ->
    reifyFactI (fromIntegral m) (\(_::proxy m) -> do
      (s', samples' :: [D.Sample T m (Zq q)]) <- instanceDisc svar $ fromIntegral numSamples
      let s'' = Secret{s = toProto s', ..}
          samples = (uncurry SampleDisc) <$> (toProto samples')
      return $ ID s'' InstanceDisc{..}))

genInstanceU (Rparams params@RLWRParams{..}) challengeID instanceID =
  reify q (\(_::Proxy q) -> reify p (\(_::Proxy p) ->
    reifyFactI (fromIntegral m) (\(_::proxy m) -> do
      (s', samples' :: [R.Sample T m (Zq q) (Zq p)]) <- instanceRLWR $ fromIntegral numSamples
      let s'' = Secret{s = toProto s', ..}
          samples = (uncurry SampleRLWR) <$> (toProto samples')
      return $ IR s'' InstanceRLWR{..})))

-- | Convert the parsed 'ChallengeParams' into serializable 'Params'
toProtoParams :: ChallengeParams -> Params
toProtoParams C{..} =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    let bound = proxy (C.errorBound svar eps) (Proxy::Proxy m)
    in Cparams $ ContParams {..})
toProtoParams D{..} =
  reifyFactI (fromIntegral m) (\(_::proxy m) ->
    let bound = proxy (D.errorBound svar eps) (Proxy::Proxy m)
    in Dparams $ DiscParams {..})
toProtoParams R{..} = Rparams $ RLWRParams {..}

-- | Writes a 'ChallengeU' to a file given a path to the root of the tree
-- and the name of the challenge.
writeChallengeU :: FilePath -> String -> ChallengeU -> IO ()
writeChallengeU path challName (CU c insts) = do
  let challDir = challengeFilesDir path challName
      challFN = challFilePath path challName
  createDirectoryIfMissing True challDir
  writeProtoType challFN c
  mapM_ (writeInstanceU path challName) insts

-- | Writes an 'InstanceU' to a file given a path to the root of the tree
-- and the name of the challenge.
writeInstanceU :: FilePath -> String -> InstanceU -> IO ()
writeInstanceU path challName iu = do
  let s = secret iu
      idx = S.instanceID s
      instFN = instFilePath path challName idx
      secretFN = secretFilePath path challName idx
  case iu of
    (IC _ inst) -> writeProtoType instFN inst
    (ID _ inst) -> writeProtoType instFN inst
    (IR _ inst) -> writeProtoType instFN inst
  writeProtoType secretFN s

-- | Writes any auto-gen'd proto object to path/filename.
writeProtoType :: (ReflectDescriptor a, Wire a) => FilePath -> a -> IO ()
writeProtoType fileName obj = BS.writeFile fileName $ messagePut obj

-- | Generate a continuous RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceCont :: (C.RLWECtx t m zq rrq, Random zq, Random (LiftOf rrq),
                 OrdFloat (LiftOf rrq), MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [C.Sample t m zq rrq])
instanceCont svar num = do
  s <- getRandom
  samples <- replicateM num $ C.sample svar s
  return (s, samples)

-- | Generate a discrete RLWE instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceDisc :: (D.RLWECtx t m zq, Random zq, MonadRandom rnd, ToRational v)
  => v -> Int -> rnd (Cyc t m zq, [D.Sample t m zq])
instanceDisc svar num = do
  s <- getRandom
  samples <- replicateM num $ D.sample svar s
  return (s, samples)

-- | Generate a discrete RLWR instance along with its (uniformly
-- random) secret, using the given scaled variance and number of
-- desired samples.
instanceRLWR :: (R.RLWRCtx t m zq zp, Random zq, MonadRandom rnd)
  => Int -> rnd (Cyc t m zq, [R.Sample t m zq zp])
instanceRLWR num = do
  s <- getRandom
  samples <- replicateM num $ R.sample s
  return (s, samples)

