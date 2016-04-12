{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, RebindableSyntax, RecordWildCards, ScopedTypeVariables #-}

import DRBG (evalCryptoRandIO)
import Challenges.Beacon
import Challenges.Common
import Challenges.ContinuousLWE
--import Challenges.LWE

import Control.Applicative
import Control.Monad.Trans (lift)
import Control.Monad.State
import Crypto.Random.DRBG

import Crypto.Lol hiding (lift)
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.RealQ

import Data.ByteString as BS (writeFile)
import Data.ByteString.Lazy as BS (toStrict)
import Data.Reflection
import Data.Time.Clock.POSIX (getPOSIXTime)

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
data ChallengeParams = CP {m::Int, q::Int, v::Double, numSamples::Int}

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
        CP 512 7681 (1/(16^(2 :: Int))) numSamples
        ]

  initTime <- beaconInit
  flip evalStateT (BP initTime 0) $ mapM_ (challengeMain path) cps

-- | Generate a challenge and write the reveal time file.
challengeMain :: FilePath -> ChallengeParams -> StateT BeaconPos IO ()
challengeMain path cp@CP{..} = do
  let name = challengeName m q v
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
makeChallenge CP{..} path challName = reify (fromIntegral q :: Int64) (\(proxyq::Proxy q) -> 
  reifyFactI m (\(proxym::proxy m) -> printPassFail 
    ("Generating challenge (m=" ++ (show m) ++ ", q=" ++ (show q) ++ ", v=" ++ (show v) ++ ")")
    "DONE" $ do
      let idxs = take numInstances [0..]
      lift $ mapM_ (genInstance proxyq proxym challName path v numSamples) idxs
  ))

-- | Generate an LWE instance and serialize the instance and secret.
genInstance :: forall q proxy m . (Fact m, Reifies q Int64) 
  => Proxy q -> proxy m -> String -> FilePath -> Double -> Int -> Int -> IO ()
genInstance _ _ challName path v numSamples idx = do
  --(secret' :: Cyc T m Int64, samples :: [LWESample T m (ZqBasic q Int64)]) <- 
  (secret' :: Cyc T m Int64, samples :: [LWESample T m (ZqBasic (Reified q) Int64) (RealQ (RealMod q) Double)]) <- 
    evalCryptoRandIO (Proxy::Proxy HashDRBG) $ 
      --proxyT (lweInstance v numSamples) (Proxy::Proxy Double)
      lweInstance v numSamples
  let secret = LWESecret idx secret'
      inst = LWEInstance idx v samples
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
