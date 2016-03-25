{-# LANGUAGE DataKinds, FlexibleContexts, RecordWildCards, ScopedTypeVariables #-}

import DRBG (evalCryptoRandIO)
import Challenges.Beacon
import Challenges.Common
import Challenges.LWE

import Control.Monad.Trans (lift)
import Control.Monad.State
import Crypto.Random.DRBG

import Crypto.Lol (Int64, ZqBasic, Proxy(..), CT, RT, reifyFactI, proxyT, Fact, intLog)
import Crypto.Lol.Types.Proto

import Data.ByteString as BS (writeFile)
import Data.ByteString.Lazy as BS (toStrict)
import Data.Reflection

import Prelude as P

import System.Directory (createDirectoryIfMissing)

import Text.ProtocolBuffers.Header

-- the Tensor type to use to generate the instances
type T = CT

beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0

data ChallengeParams = CP {m::Int, p::Int, v::Double, numSamples::Int, numInstances::Int}

main :: IO ()
main = do
  -- EAC: Read from command line
  let numInstances = 4 :: Int
      numSamples = 8

  path <- absPath

  let cps = [
        CP 32 257 1.0 numSamples numInstances,
        CP 64 257 1.1 numSamples numInstances
        ]

  initTime <- beaconInit
  flip evalStateT (BP initTime 0) $ mapM_ (challengeMain path) cps

challengeMain :: FilePath -> ChallengeParams -> StateT BeaconPos IO ()
challengeMain path cp@CP{..} = do
  let name = challengeName m p v
  lift $ makeChallenge cp path name
  stampChallenge name numSamples

stampChallenge :: String -> Int -> StateT BeaconPos IO ()
stampChallenge name numSamples = do
  let numBits = intLog 2 numSamples
  abspath <- lift absPath
  let path = abspath </> challengeFilesDir </> name
  -- advance to the next place we can use 'numBits' bits and return it
  (BP time offset) <- (modify $ getBeaconPos numBits) >> get
  let revealFile = path </> revealFileName
  lift $ P.writeFile revealFile name
  lift $ P.appendFile revealFile $ "\n" ++ show time
  lift $ P.appendFile revealFile $ "\n" ++ show offset
  -- advance the state by 'numBits'
  modify (advanceBeaconPos numBits)

-- outputs the challenge name and the number of instances for this challenge
makeChallenge :: ChallengeParams -> FilePath -> String -> IO ()
makeChallenge CP{..} path challName = reify (fromIntegral p :: Int64) (\(proxyp::Proxy p) -> 
  reifyFactI m (\(proxym::proxy m) -> do      
    putStrLn $ "Generating challenge (m=" ++ (show m) ++ ", p=" ++ (show p) ++ ", v=" ++ (show v) ++ ")..."
    let idxs = take numInstances [0..]
    mapM_ (genInstance proxyp proxym challName path v numSamples) idxs))

genInstance :: forall p proxy m . (Fact m, Reifies p Int64) 
  => Proxy p -> proxy m -> String -> FilePath -> Double -> Int -> Int -> IO ()
genInstance _ _ challName path v numSamples idx = do
  -- EAC: might want to add some more randomness, since this uses IO to get seed
  (secret', samples :: [LWESample T m (ZqBasic p Int64)]) <- 
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ 
      proxyT (lweInstance v numSamples) (Proxy::Proxy Double)
  let secret = LWESecret idx secret'
      inst = LWEInstance idx v samples
      secretFile = secretFileName idx
      instFile = instFileName idx
  writeProtoType (path </> challengeFilesDir </> challName) instFile inst
  writeProtoType (path </> secretFilesDir </> challName) secretFile secret

writeProtoType :: 
  (Protoable a,
   ReflectDescriptor (ProtoType a), 
   Wire (ProtoType a))
  => FilePath -> String -> a -> IO ()
writeProtoType challPath instName inst = do
  createDirectoryIfMissing True challPath
  let instPath = challPath </> instName
  BS.writeFile instPath $ toStrict $ msgPut inst



  
  

  
{-

  
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



{-
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