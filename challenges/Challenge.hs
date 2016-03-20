{-# LANGUAGE DataKinds, GADTs, FlexibleContexts, NoImplicitPrelude, PackageImports, RebindableSyntax, ScopedTypeVariables #-}

module Challenge where

import DRBG
import LWE
import Random
import Utils
import Verify

import Control.DeepSeq
import Control.Monad.Random
import "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Control.Monad.State

import Crypto.Lol hiding (writeFile, lift)
import Crypto.Lol.Types.Proto

import Data.ByteString (writeFile)
import Data.ByteString.Lazy (toStrict, fromStrict, null)

import System.Console.ANSI
import System.Directory

import Text.ProtocolBuffers.Header

-- PARAMETERS

beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0

beaconBitsPerChallenge = 4 :: Int
-- during the verification purposes, we'll reveal secrets for all but one instance
instancesPerChallenge = 2^beaconBitsPerChallenge :: Int
samplesPerInstance = 128 :: Int


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
              => Proxy q -> Proxy (Cyc t m zp) -> v -> StateT BeaconPos IO ()
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

  lift $ writeChallenge svar chall'
  lift $ writeSecrets svar sks

writeChallenge :: forall v t m zp . 
  (Fact m, Mod zp, Show (ModRep zp), Show v, 
   Protoable (LWEChallenge v t m zp),
   ReflectDescriptor (ProtoType (LWEChallenge v t m zp)), 
   Wire (ProtoType (LWEChallenge v t m zp))) 
  => v -> LWEChallenge v t m zp -> IO ()
writeChallenge v chall = do
  let challDir = "challenge-files"
  createDirectoryIfMissing False challDir
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
      challFile  = challDir  ++ "/lwe-" ++ (show m) ++ "-" ++ (show q) ++ "-" ++ (show v)
  putStrLn $ "Writing challenge to " ++ challFile
  writeFile challFile $ toStrict $ msgPut chall

writeSecrets :: forall v t m zp . (Fact m, Mod zp, Show (ModRep zp), Show v, Protoable (ChallengeSecrets t m zp)) 
             => v -> ChallengeSecrets t m zp -> IO ()
writeSecrets v secrets = do
  let secretDir = "top-secret-files"
  createDirectoryIfMissing False secretDir
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
      secretFile = secretDir ++ "/lwe-" ++ (show m) ++ "-" ++ (show q) ++ "-" ++ (show v)
  putStrLn $ "Writing secrets to " ++ secretFile
  writeFile secretFile $ toStrict $ msgPut secrets

-- EAC TODO:
-- use good random source (currently using IO)
-- print out "Generating challenge for params blah..."  "done"
-- print out "Verifying challenge..." "done"
main = do
  initTime <- beaconInit
  flip evalStateT (BP initTime 0) $ sequence_ $ [
    makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F32 (Zq 129))) (1::Double),
    makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F32 (Zq 129))) (2::Double)
    ]