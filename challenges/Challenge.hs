{-# LANGUAGE DataKinds, GADTs, FlexibleContexts, NoImplicitPrelude, PackageImports, 
             RebindableSyntax, ScopedTypeVariables #-}

import DRBG
import Utils
import Challenges.LWE
import Challenges.Random
import Challenges.Verify

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Control.Monad.State

import Crypto.Hash.SHA3 (hash)
import Crypto.Lol hiding (readFile, writeFile, lift)
import qualified Crypto.Lol (writeFile)
import Crypto.Lol.Types.Proto

import Data.ByteString (writeFile,readFile)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict, fromStrict, null)
import Data.Char
import Data.List
import Data.List.Split (chunksOf)

import Net.Beacon

import System.Console.ANSI
import System.Directory

import Text.ProtocolBuffers.Header

-- PARAMETERS

beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0

beaconBitsPerChallenge = 2 :: Int
-- during the verification purposes, we'll reveal secrets for all but one instance
instancesPerChallenge = 2^beaconBitsPerChallenge :: Int
samplesPerInstance = 8 :: Int


data BeaconPos = BP {time::Int, offset::Int}

nextBeaconPos :: BeaconPos -> BeaconPos
nextBeaconPos (BP time offset) =
  let nextOffset = offset+beaconBitsPerChallenge
  -- we will use the time/offset from the state, unless there aren't enough bits left in this beacon
  in if nextOffset+beaconBitsPerChallenge> 512 
     then BP (time+60) 0
     else BP time nextOffset

-- This function serves the dual purpose of specifying the random generator
-- and keeping all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function, but it would be hard
-- to get timing any other way.
evalCryptoRandIO :: Rand (CryptoRand HashDRBG) a -> IO a
evalCryptoRandIO x = do
  gen <- newGenIO -- uses system entropy
  return $ evalRand x gen

-- generate the challenge, verify it, and write it out
makeChallenge :: forall v q t m zp . 
  (LWECtx t m (LiftOf zp) zp v q, Random (LiftOf zp), Lift' zp,
   Mod zp, Show v, Show (ModRep zp), Protoable (ChallengeSecrets t m zp),
   Protoable (LWEChallenge v t m zp), NFData v, NFData (Cyc t m zp), NFData (Cyc t m (LiftOf zp)),
   ReflectDescriptor (ProtoType (LWEChallenge v t m zp)), 
   Wire (ProtoType (LWEChallenge v t m zp))) 
              => Proxy q -> Proxy (Cyc t m zp) -> v -> StateT BeaconPos IO FilePath
makeChallenge _ _ svar = do  
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
  
  lift $ putStr $ "Generating instance (v=" ++ (show svar) ++ ", m=" ++ (show m) ++ ", q=" ++ (show q) ++ ")..."
  -- EAC: probably want to add some more randomness
  -- also not no sequencing occurs between calls
  chall <- lift $ evalCryptoRandIO $ proxyT (lweChallenge svar samplesPerInstance instancesPerChallenge) (Proxy::Proxy q)
  chall `deepseq` lift $ putStr $ "Verifying..."
  let result = checkChallenge (chall :: SecretLWEChallenge v t m zp)
  if result
  then do
    lift $ setSGR [SetColor Foreground Vivid Green]
    lift $ putStrLn "PASS"
  else do
    lift $ setSGR [SetColor Foreground Vivid Red]
    lift $ putStrLn "FAIL"
  lift $ setSGR [SetColor Foreground Vivid Black]

  -- get the position for this challenge
  BP time offset <- get
  -- update the state for the next challenge
  modify nextBeaconPos

  lift $ putStrLn $ "Beacon value for this challenge is " ++ (show time) ++ "+" ++ (show offset)
  let (sks, chall') = removeSecrets chall time offset

  let challName = challengeFileName chall'
  lift $ writeChallenge challName chall'
  lift $ writeSecrets challName sks
  return challName

challengeFileName :: forall v t m zp . (Fact m, Mod zp, Show (ModRep zp), Show v) 
  => LWEChallenge v t m zp -> FilePath
challengeFileName (LWEChallenge _ _ v _) = 
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
  in "lwe-" ++ (show m) ++ "-" ++ (show q) ++ "-" ++ (show v)

writeChallenge :: 
  (Protoable (LWEChallenge v t m zp),
   ReflectDescriptor (ProtoType (LWEChallenge v t m zp)), 
   Wire (ProtoType (LWEChallenge v t m zp))) 
  => FilePath -> LWEChallenge v t m zp -> IO ()
writeChallenge name chall = do
  createDirectoryIfMissing False challengePath
  let challFile = challengePath ++ "/" ++ name
  putStrLn $ "Writing challenge to " ++ challFile ++ "..."
  writeFile challFile $ toStrict $ msgPut chall

writeSecrets :: (Protoable (ChallengeSecrets t m zp)) => FilePath -> ChallengeSecrets t m zp -> IO ()
writeSecrets name secrets = do
  createDirectoryIfMissing False topSecretPath
  let secretFile = topSecretPath ++ "/" ++ name
  putStrLn $ "Writing secrets to " ++ secretFile ++ "..."
  writeFile secretFile $ toStrict $ msgPut secrets

-- SHA3-256
hashFile :: FilePath -> IO String
hashFile path = do
  bs <- readFile $ challengePath ++ "/" ++ path
  let hashLen = 512
      h = hash hashLen bs
      lineSize = 64
      lineBreak = (replicate lineSize '-') ++ "\n"
      header = "SHA3-" ++ (show hashLen) ++ " hash for challenge " ++ path ++ "\n" ++ lineBreak
      hashStr = intercalate "\n" $ chunksOf lineSize $ map toUpper $ tail $ init $ show $ toLazyByteString $ byteStringHex h
  return $ header ++ hashStr ++ "\n\n"

main = do
  initTime <- beaconInit
  currTime <- liftM (timeStamp . fromJust' "Failed to get last beacon") getLastRecord

  when (initTime < currTime + 24*60*60) $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "WARNING: The reveal date is less than one day from now!"
    setSGR [SetColor Foreground Vivid Black]

  -- add challenges here
  let challengeList = [
        makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F8 (Zq 129))) (1::Double),
        makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F8 (Zq 129))) (2::Double)
        ]

  names <- flip evalStateT (BP initTime 0) $ sequence challengeList

  -- write list of all challenges generated for easy parsing/verification
  let challListFile = challengePath ++ "/challenges.txt"
  putStrLn $ "Writing list of challenges to " ++ challListFile
  Crypto.Lol.writeFile challListFile $ intercalate "\n" names

  -- write file containing hashes of each challenge
  hashes <- concat <$> mapM hashFile names
  let hashFilePath = challengePath ++ "/hashes.txt"
  putStrLn $ "Writing hashes to " ++ hashFilePath
  Crypto.Lol.writeFile hashFilePath hashes

  -- EAC: TODO:
  -- run full verifier at the end
  -- investigate why I got a verification failure for m=8/q=129/v=1::Double
