{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, FlexibleContexts, NoImplicitPrelude, PackageImports, 
             RebindableSyntax, ScopedTypeVariables #-}

import DRBG
import Utils
import Challenges.Beacon
import Challenges.LWE
import Challenges.MakeReader
import Challenges.Parameters
import Challenges.Verify
import Challenges.Writer

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
import Data.Char (toUpper)
import Data.List
import Data.List.Split (chunksOf)

import Net.Beacon

import System.Console.ANSI
import System.Directory

import Text.ProtocolBuffers.Header

-- generate the challenge, verify it, and write it out
makeChallenge :: forall v q t m zp . 
  (LWECtx t m (LiftOf zp) zp v q, Random (LiftOf zp),
   Mod zp, Protoable (ChallengeSecrets t m zp),
   Show v, Show (ModRep zp),
   NFData v, NFData (Cyc t m zp), NFData (Cyc t m (LiftOf zp)),
   CheckSample v t m zp,
   ProtoWriteChallCtx v t m zp) 
              => Proxy q -> Proxy (Cyc t m zp) -> v -> StateT BeaconPos IO FilePath
makeChallenge _ _ svar = do
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
  
  lift $ putStr $ "Generating instance (v=" ++ (show svar) ++ ", m=" ++ (show m) ++ ", q=" ++ (show q) ++ ")..."
  -- EAC: probably want to add some more randomness
  -- also not no sequencing occurs between calls
  chall <- lift $ evalCryptoRandIO (Proxy::Proxy HashDRBG) $ proxyT (lweChallenge svar samplesPerInstance instancesPerChallenge) (Proxy::Proxy q)
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
  BP time offset _ _ _ <- get
  -- update the state for the next challenge
  modify nextBeaconPos

  lift $ putStrLn $ "Beacon value for this challenge is " ++ (show time) ++ "+" ++ (show offset)
  let (sks, chall') = removeSecrets chall time offset

  let challName = challengeFileName chall'
  lift $ writeChallenge challName chall'
  lift $ writeSecrets topSecretPath challName sks
  return challName

challengeFileName :: forall v t m zp . (Fact m, Mod zp, Show (ModRep zp), Show v) 
  => LWEChallenge v t m zp -> FilePath
challengeFileName (LWEChallenge _ _ v _) = 
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
  in "lwe-" ++ (show m) ++ "-" ++ (show q) ++ "-" ++ (show v)

type ProtoWriteChallCtx v t m zp = 
  (Protoable (LWEChallenge v t m zp),
   ReflectDescriptor (ProtoType (LWEChallenge v t m zp)), 
   Wire (ProtoType (LWEChallenge v t m zp)))

writeChallenge :: (ProtoWriteChallCtx v t m zp) 
  => FilePath -> LWEChallenge v t m zp -> IO ()
writeChallenge name chall = do
  createDirectoryIfMissing False challengePath
  let challFile = challengePath ++ "/" ++ name
  putStrLn $ "Writing challenge to " ++ challFile ++ "..."
  writeFile challFile $ toStrict $ msgPut chall



-- SHA3
hashFile :: FilePath -> IO String
hashFile path = do
  bs <- readFile $ challengePath ++ "/" ++ path
  let h = hash hashOutputBits bs
      lineBreak = (replicate hashPrettyPrintLineSize '-') ++ "\n"
      header = "SHA3-" ++ (show hashOutputBits) ++ " hash for challenge " ++ path ++ "\n" ++ lineBreak
      hashStr = intercalate "\n" $ chunksOf hashPrettyPrintLineSize $ 
                  map toUpper $ tail $ init $ show $ toLazyByteString $ byteStringHex h
  return $ header ++ hashStr ++ "\n\n"

main :: IO ()
main = do
  initTime <- beaconInit
  currTime <- liftM (timeStamp . fromJust' "Failed to get last beacon") getLastRecord

  when (initTime < currTime + 24*60*60) $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "WARNING: The reveal date is less than one day from now!"
    setSGR [SetColor Foreground Vivid Black]

  -- add challenges here
  let challengeList = [
        makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F32 (Zq 257))) (1::Double),
        makeChallenge (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT F32 (Zq 257))) (2::Double)
        ]

  names <- flip evalStateT (BP initTime 0 beaconBitsPerChallenge bitsPerBeacon beaconInterval) $ sequence challengeList

  -- write list of all challenges generated for easy parsing/verification
  -- EAC: we don't need this, but it seems convenient for others to have
  let challListFile = challengePath ++ "/challenges.txt"
  putStrLn $ "Writing list of challenges to " ++ challListFile
  Crypto.Lol.writeFile challListFile $ intercalate "\n" names

  --create Parser.hs to reader challenges
  putStrLn $ "Creating reader for challenges"
  mkReader names

  -- write file containing hashes of each challenge
  hashes <- concat <$> mapM hashFile names
  let hashFilePath = challengePath ++ "/hashes.txt"
  putStrLn $ "Writing hashes to " ++ hashFilePath
  Crypto.Lol.writeFile hashFilePath hashes

  -- EAC: TODO:
  -- run full verifier at the end
