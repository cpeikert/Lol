{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, FlexibleContexts, KindSignatures, NoImplicitPrelude, PackageImports, 
             RebindableSyntax, ScopedTypeVariables #-}

import DRBG
import Utils
import Challenges.Beacon
import Challenges.LWE
--import Challenges.MakeReader
--import Challenges.Parameters
--import Challenges.Verify
--import Challenges.Writer

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Control.Monad.State

import Crypto.Hash.SHA3 (hash)
import Crypto.Lol hiding (readFile, writeFile, lift)
import qualified Crypto.Lol (writeFile)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto

import Data.ByteString (writeFile,readFile)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict, fromStrict, null)
import Data.Char (toUpper)
import Data.List
import Data.List.Split (chunksOf)
import Data.Reflection

import Net.Beacon

import System.Console.ANSI
import System.Directory

import Text.ProtocolBuffers.Header

main :: IO ()
main = do
  -- EAC: Read from command line
  let numInstances = 4 :: Int
      numSamples = 8 :: Int
      m = 32 :: Int
      p = 257 :: Int64
      v = 1.0 :: Double

  path <- do
    inTopLevelLol <- doesDirectoryExist "challenges"
    return $ if inTopLevelLol
      then "challenges/challenge-files"
      else "challenge-files"

  putStrLn $ "Generating instances (m=" ++ (show m) ++ ", p=" ++ (show p) ++ ", v=" ++ (show v) ++ ")..."
  let idxs = take numInstances [0..]

  reify p (\(p::Proxy p) -> 
    reifyFactI m (\(_::proxy m) -> 
      mapM_ (makeInstance (Proxy::Proxy Double) (Proxy::Proxy (Cyc RT m (ZqBasic p Int64))) v numSamples path) idxs))

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- generate the instance and write it out
makeInstance :: forall v q t m z zp . 
  (LWECtx t m z zp v q, Random z,
   v ~ Double, z ~ LiftOf zp,
   Show v, Show (ModRep zp),
   NFData v, NFData (Cyc t m zp), NFData (Cyc t m z),
   ProtoWriteCtx (LWEInstance v t m zp),
   ProtoWriteCtx (LWESecret t m z))
  => Proxy q -> Proxy (Cyc t m zp) -> v -> Int -> FilePath -> Int -> IO ()
makeInstance _ _ svar numSamples mainPath idx = do
  -- EAC: probably want to add some more randomness
  -- also not no sequencing occurs between calls
  (sk,samples) <- evalCryptoRandIO (Proxy::Proxy HashDRBG) $ 
    proxyT (lweInstance svar numSamples) (Proxy::Proxy q)
 
  let inst = LWEInstance idx svar samples :: LWEInstance v t m zp
      secret = LWESecret idx sk
      m = proxy valueFact (Proxy::Proxy m)
      q = proxy value (Proxy::Proxy m)
      challName = challengeDirName m q svar
      challDir = mainPath </> challName
      instFile = instFileName idx
      secretFile = secretFileName idx 

  putStrLn $ "Writing instance " ++ (show idx) ++ " to " ++ challDir ++ "..."
  writeProtoType challDir instFile inst
  writeProtoType challDir secretFile secret

challengeDirName :: Int -> Int -> Double -> FilePath
challengeDirName m p v = "chall-m" ++ (show m) ++ "-p" ++ (show p) ++ "-v" ++ (show v)

instFileName :: Int -> FilePath
instFileName idx = "instance" ++ (show idx) ++ ".bin"

secretFileName :: Int -> FilePath
secretFileName idx = "secret" ++ (show idx) ++ ".bin"

type ProtoWriteCtx a = 
  (Protoable a,
   ReflectDescriptor (ProtoType a), 
   Wire (ProtoType a))

writeProtoType :: (ProtoWriteCtx a)
  => FilePath -> String -> a -> IO ()
writeProtoType challPath instName inst = do
  createDirectoryIfMissing True challPath
  let instPath = challPath </> instName
  writeFile instPath $ toStrict $ msgPut inst




  

  

  

  
{-









-- generate the challenge, verify it, and write it out
makeChallenge :: forall v q t m zp . 
  (LWECtx t m (LiftOf zp) zp v q, Random (LiftOf zp),
   Mod zp, Protoable (ChallengeSecrets t m zp),
   Show v, Show (ModRep zp),
   NFData v, NFData (Cyc t m zp), NFData (Cyc t m (LiftOf zp)),
   CheckSample v t m zp,
   ProtoWriteChallCtx v t m zp) 
              => Proxy q -> Proxy (Cyc t m zp) -> v -> StateT BeaconPos IO FilePath
makeChallenge _ _ svar = undefined {-
  do
  let m = proxy valueFact (Proxy::Proxy m)
      q = proxy modulus (Proxy::Proxy zp)
      beaconBitsPerChallenge = intLog 2 instancePerChallenge
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
  modify $ advanceBeaconPos beaconBitsPerChallenge

  lift $ putStrLn $ "Beacon value for this challenge is " ++ (show time) ++ "+" ++ (show offset)
  let (sks, chall') = removeSecrets chall time offset

  let challName = challengeFileName chall'
  lift $ writeChallenge challName chall'
  lift $ writeSecrets topSecretPath challName sks
  return challName
-}




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

-- takes number of instancesPerChallenge and a "challenge" file" with a list of params
challMain :: IO ()
challMain = do
  initTime <- beaconInit
  currTime <- liftM (timeStamp . fromJust' "Failed to get last beacon") getLastRecord
  when (initTime < currTime + 24*60*60) $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "WARNING: The reveal date is less than one day from now!"
    setSGR [SetColor Foreground Vivid Black]
    names <- flip evalStateT (BP initTime 0) $ sequence challengeList

  -- write list of all challenges generated for easy parsing/verification
  -- EAC: we don't need this, but it seems convenient for others to have
  let challListFile = challengePath ++ "/challenges.txt"
  putStrLn $ "Writing list of challenges to " ++ challListFile
  Crypto.Lol.writeFile challListFile $ intercalate "\n" names

  -- write file containing hashes of each challenge
  hashes <- concat <$> mapM hashFile names
  let hashFilePath = challengePath ++ "/hashes.txt"
  putStrLn $ "Writing hashes to " ++ hashFilePath
  Crypto.Lol.writeFile hashFilePath hashes

-}