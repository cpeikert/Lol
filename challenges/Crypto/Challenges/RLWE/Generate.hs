{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, RebindableSyntax, RecordWildCards, ScopedTypeVariables #-}

module Crypto.Challenges.RLWR.Generate where

import Crypto.Challenges.RLWE.Beacon
import Crypto.Challenges.RLWE.Common
import Crypto.Challenges.RLWE.Continuous as C
import Crypto.Challenges.RLWE.Discrete as D
import Crypto.Challenges.RLWE.RLWR as R

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.State
import Crypto.Random.DRBG

import Crypto.Lol hiding (lift)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.RRq

import Data.ByteString as BS (writeFile)
import Data.ByteString.Lazy as BS (toStrict)
import Data.Function (fix)
import Data.Reflection
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Maybe
import Data.Word

import DRBG (evalCryptoRandIO)

import qualified Prelude as P

import System.Console.ANSI
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.IO as IO

import Text.ProtocolBuffers.Header

-- the Tensor type to use to generate the instances
type T = CT

-- | The date/time when the randomness used to reveal the first challenge will be available.
-- Other challenges use different bytes of this beacon output, or of subsequent beacon ouputs.
beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0

-- | Information to generate a challenge.
data ChallengeParams = 
    DiscLWE {m::Int, q::Int, v::Double, numSamples::Int}
  | ContLWE {m::Int, q::Int, v::Double, numSamples::Int}
  | LWR     {m::Int, q::Int, q'::Int,   numSamples::Int}

main :: IO ()
main = do
  -- for nice printing when running executable
  hSetBuffering stdout NoBuffering

  let numSamples = 10

  -- temporary, should take command line argument
  path <- getPath

  -- remove any existing instances/secrets
  challDirExists <- doesDirectoryExist $ path </> challengeFilesDir
  when challDirExists $ removeDirectoryRecursive $ path </> challengeFilesDir
  secDirExists <- doesDirectoryExist $ path </> secretFilesDir
  when secDirExists $ removeDirectoryRecursive $ path </> secretFilesDir

  -- list of challenge params
  let cps = [
        ContLWE 512 7681 (1/(16^(2 :: Int))) numSamples,
        DiscLWE 512 7681 (1/(16^(2 :: Int))) numSamples,
        LWR 512 8192 2048 numSamples
        ]

  initTime <- beaconInit
  flip evalStateT (BP initTime 0) $ mapM_ (challengeMain path) cps

-- | The name for each challenge directory.
challengeName :: ChallengeParams -> FilePath
challengeName (DiscLWE m q v _) = "chall-dlwe-m" ++ (show m) ++ "-q" ++ (show q) ++ "-v" ++ (show v)
challengeName (ContLWE m q v _) = "chall-clwe-m" ++ (show m) ++ "-q" ++ (show q) ++ "-v" ++ (show v)
challengeName (LWR m q q' _) = "chall-lwr-m" ++ (show m) ++ "-q" ++ (show q) ++ "-q'" ++ (show q')

-- | Generate a challenge and write the reveal time file.
challengeMain :: FilePath -> ChallengeParams -> StateT BeaconPos IO ()
challengeMain path cp = do
  let name = challengeName cp
  lift $ makeChallenge cp path name
  stampChallenge path name

-- | Writes the beacon timestamp and byte offset data for this challenge.
stampChallenge :: FilePath -> String -> StateT BeaconPos IO ()
stampChallenge abspath name = do
  let path = abspath </> challengeFilesDir </> name
  (BP time offset) <- get
  advanceBeaconPos
  -- compare the state to the current time
  currTime <- P.round <$> liftIO getPOSIXTime
  if (currTime > time)
  then do
    liftIO $ setSGR [SetColor Foreground Vivid Red]
    liftIO $ putStrLn $ "Reveal time is in the past!"
    liftIO $ setSGR [SetColor Foreground Vivid Black]
  else when (currTime + 60*60*24*5 > time) $ do
    liftIO $ setSGR [SetColor Foreground Vivid Red]
    liftIO $ putStrLn $ "Reveal time is less than 5 days away!"
    liftIO $ setSGR [SetColor Foreground Vivid Black]
  let revealFile = path </> revealFileName
  lift $ IO.writeFile revealFile $ show time
  lift $ IO.appendFile revealFile $ "\n" ++ show offset

-- | Generate an LWE challenge with the given parameters.
makeChallenge :: ChallengeParams -> FilePath -> String -> IO ()
makeChallenge ContLWE{..} path challName = reify (fromIntegral q :: Int64) (\(proxyq::Proxy q) -> 
  reifyFactI m (\(proxym::proxy m) -> printPassFail 
    ("Generating continuous LWE challenge (m=" ++ (show m) ++ 
     ", q=" ++ (show q) ++ ", v=" ++ (show v) ++ ")") "DONE" $ do
      let idxs = take numInstances [0..]
      lift $ mapM_ (genContLWEInstance proxyq proxym challName path v numSamples) idxs
  ))
makeChallenge DiscLWE{..} path challName = reify (fromIntegral q :: Int64) (\(proxyq::Proxy q) -> 
  reifyFactI m (\(proxym::proxy m) -> printPassFail 
    ("Generating discretized LWE challenge (m=" ++ (show m) ++ 
     ", q=" ++ (show q) ++ ", v=" ++ (show v) ++ ")") "DONE" $ do
      let idxs = take numInstances [0..]
      lift $ mapM_ (genDiscLWEInstance proxyq proxym challName path v numSamples) idxs
  ))
makeChallenge LWR{..} path challName = reify (fromIntegral q :: Int64) (\(proxyq::Proxy q) -> 
 reify (fromIntegral q' :: Int64) (\(proxyq'::Proxy q') ->
  reifyFactI m (\(proxym::proxy m) -> printPassFail 
    ("Generating LWR challenge (m=" ++ (show m) ++ 
     ", q=" ++ (show q) ++ ", q'=" ++ (show q') ++ ")") "DONE" $ do
      let idxs = take numInstances [0..]
      lift $ mapM_ (genLWRInstance proxyq proxyq' proxym challName path numSamples) idxs
  )))

-- | Generate a continuous LWE instance and serialize the instance and secret.
genContLWEInstance :: forall q proxy m . (Fact m, Reifies q Int64) 
  => Proxy q -> proxy m -> String -> FilePath -> Double -> Int -> Word32 -> IO ()
genContLWEInstance _ _ challName path v numSamples idx = do 
  (secret' :: Cyc T m Int64, 
   samples :: [RLWESampleCont T m (ZqBasic (Reified q) Int64) (RRq (RealMod (Reified q)) Double)]) <-
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ C.rlweInstance v numSamples
  let secret = RLWESecret idx secret'
      eps = 1/(2^(40 :: Int))
      bound = proxy (computeBound v eps) (Proxy::Proxy m)
      inst = RLWEInstanceCont idx v bound samples
      secretFile = secretFileName challName idx
      instFile = instFileName challName idx
  writeProtoType (path </> challengeFilesDir </> challName) instFile inst
  writeProtoType (path </> secretFilesDir </> challName) secretFile secret
  putStr "."

-- | Generate a discretized LWE instance and serialize the instance and secret.
genDiscLWEInstance :: forall q proxy m . (Fact m, Reifies q Int64) 
  => Proxy q -> proxy m -> String -> FilePath -> Double -> Int -> Word32 -> IO ()
genDiscLWEInstance _ _ challName path v numSamples idx = do 
  (secret' :: Cyc T m Int64, 
   samples :: [RLWESampleDisc T m (ZqBasic (Reified q) Int64)]) <- 
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ proxyT (D.rlweInstance v numSamples) (Proxy::Proxy Double)
  let secret = RLWESecret idx secret'
      eps = 1/(2^(40 :: Int))
      bound = floor $ proxy (computeBound v eps) (Proxy::Proxy m)
      --bound = error "Need to figure out a valid bound for discretized samples"
      inst = RLWEInstanceDisc idx v bound samples
      secretFile = secretFileName challName idx
      instFile = instFileName challName idx
  writeProtoType (path </> challengeFilesDir </> challName) instFile inst
  writeProtoType (path </> secretFilesDir </> challName) secretFile secret
  putStr "."

-- | Generate an LWR instance and serialize the instance and secret.
genLWRInstance :: forall q q' proxy m . (Fact m, Reifies q Int64, Reifies q' Int64) 
  => Proxy q -> Proxy q' -> proxy m -> String -> FilePath -> Int -> Word32 -> IO ()
genLWRInstance _ _ _ challName path numSamples idx = do 
  (secret' :: Cyc T m Int64, 
   samples :: [R.RLWRSample T m (ZqBasic (Reified q) Int64) (ZqBasic (Reified q') Int64)]) <- 
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ proxyT (R.rlwrInstance numSamples) (Proxy::Proxy Double)
  let secret = RLWESecret idx secret'
      inst = RLWRInstance idx samples
      secretFile = secretFileName challName idx
      instFile = instFileName challName idx
  writeProtoType (path </> challengeFilesDir </> challName) instFile inst
  writeProtoType (path </> secretFilesDir </> challName) secretFile secret
  putStr "."

-- | Writes any 'Protoable' object to path/filename.
writeProtoType :: 
  (Protoable a,
   ReflectDescriptor (ProtoType a), 
   Wire (ProtoType a))
  => FilePath -> String -> a -> IO ()
writeProtoType path fileName obj = do
  createDirectoryIfMissing True path
  let instPath = path </> fileName
  BS.writeFile instPath $ toStrict $ msgPut obj

-- EAC: Note that this bound is correct for *continuous* LWE samples,
-- but an extra additive term may be necessary for discretized samples.

-- | Outputs a bound such that the scaled, squared norm of an 
-- error term generated with (scaled) variance v
-- will be less than the bound except with probability eps.
computeBound :: (Field v, Ord v, Transcendental v, Fact m) => v -> v -> Tagged m v
computeBound v eps = do
  n <- totientFact
  mhat <- valueHatFact
  let d = flip fix (1 / (2*pi)) $ \f d ->
        let d' = (1/2 + (log $ 2 * pi * d)/2 - (log eps)/(fromIntegral n))/pi
        in if ((d'-d) < 0.0001)
           then d'
           else f d'
  return $ (fromIntegral $ mhat*n)*v*d
