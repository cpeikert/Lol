{-# LANGUAGE FlexibleContexts, RecordWildCards, ScopedTypeVariables #-}

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
import Data.Time.Clock.POSIX (getPOSIXTime)

import Prelude as P

import System.Console.ANSI
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.IO

import Text.ProtocolBuffers.Header

-- the Tensor type to use to generate the instances
type T = CT

beaconInit :: IO Int
beaconInit = localDateToSeconds 2 24 2016 11 0

data ChallengeParams = CP {m::Int, p::Int, v::Double, numSamples::Int, numInstances::Int}

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- EAC: Read from command line
  let numInstances = 16 :: Int
      numSamples = 10

  path <- absPath

  challDirExists <- doesDirectoryExist $ path </> challengeFilesDir
  when challDirExists $ removeDirectoryRecursive $ path </> challengeFilesDir
  secDirExists <- doesDirectoryExist $ path </> secretFilesDir
  when secDirExists $ removeDirectoryRecursive $ path </> secretFilesDir

  let cps = [
        CP 256 257 1.0 numSamples numInstances,
        CP 256 257 1.1 numSamples numInstances
        ]

  initTime <- beaconInit
  flip evalStateT (BP initTime 0) $ mapM_ (challengeMain path) cps

challengeMain :: FilePath -> ChallengeParams -> StateT BeaconPos IO ()
challengeMain path cp@CP{..} = do
  let name = challengeName m p v
  lift $ makeChallenge cp path name
  stampChallenge name numInstances

stampChallenge :: String -> Int -> StateT BeaconPos IO ()
stampChallenge name numInstances = do
  let numBits = intLog 2 numInstances
  abspath <- lift absPath
  let path = abspath </> challengeFilesDir </> name
  -- advance to the next place we can use 'numBits' bits and return it
  (BP time offset) <- (modify $ getBeaconPos numBits) >> get
  currTime <- round <$> liftIO getPOSIXTime
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
  lift $ P.writeFile revealFile $ show time
  lift $ P.appendFile revealFile $ "\n" ++ show offset
  -- advance the state by 'numBits'
  modify (advanceBeaconPos numBits)

-- outputs the challenge name and the number of instances for this challenge
makeChallenge :: ChallengeParams -> FilePath -> String -> IO ()
makeChallenge CP{..} path challName = reify (fromIntegral p :: Int64) (\(proxyp::Proxy p) -> 
  reifyFactI m (\(proxym::proxy m) -> printPassFail 
    ("Generating challenge (m=" ++ (show m) ++ ", p=" ++ (show p) ++ ", v=" ++ (show v) ++ ")") $ do
      let idxs = take numInstances [0..]
      lift $ mapM_ (genInstance proxyp proxym challName path v numSamples) idxs
  ))

genInstance :: forall p proxy m . (Fact m, Reifies p Int64) 
  => Proxy p -> proxy m -> String -> FilePath -> Double -> Int -> Int -> IO ()
genInstance _ _ challName path v numSamples idx = do
  (secret', samples :: [LWESample T m (ZqBasic p Int64)]) <- 
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ 
      proxyT (lweInstance v numSamples) (Proxy::Proxy Double)
  let secret = LWESecret idx secret'
      inst = LWEInstance idx v samples
      secretFile = secretFileName idx
      instFile = instFileName idx
  writeProtoType (path </> challengeFilesDir </> challName) instFile inst
  writeProtoType (path </> secretFilesDir </> challName) secretFile secret
  putStr "."

writeProtoType :: 
  (Protoable a,
   ReflectDescriptor (ProtoType a), 
   Wire (ProtoType a))
  => FilePath -> String -> a -> IO ()
writeProtoType challPath instName inst = do
  createDirectoryIfMissing True challPath
  let instPath = challPath </> instName
  BS.writeFile instPath $ toStrict $ msgPut inst
