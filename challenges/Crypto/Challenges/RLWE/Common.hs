{-# LANGUAGE RecordWildCards #-}

module Crypto.Challenges.RLWE.Common
( challengeFilesDir, secretFilesDir
, challFilePath, instFilePath, secretFilePath
, xmlFilePath, certFilePath
, printPassFail
, challengeList
, parseBeaconAddr
, readProtoType
, secretIdx
, checkFileExists
, throwErrorIf, throwErrorIfNot, maybeThrowError
, Zq, RRq, ChallengeU(..), InstanceU(..)
) where

import Crypto.Challenges.RLWE.Beacon
import Crypto.Challenges.RLWE.Proto.RLWE.Challenge
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc
import Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR
import Crypto.Challenges.RLWE.Proto.RLWE.Secret
import qualified Crypto.Lol as Lol
import Crypto.Lol.Reflects

import Control.Monad.Except

import           Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy as BS
import           Data.Default         (Default (..))
import           Data.Int
import           Data.Maybe

import Net.Beacon

import System.Console.ANSI
import System.Directory    (doesDirectoryExist, doesFileExist,
                            getDirectoryContents)
import System.FilePath     ((</>))

import Text.Printf
import Text.ProtocolBuffers        (messageGet)
import Text.ProtocolBuffers.Header (ReflectDescriptor, Wire)

{- Directory structure:

challengeFilesDir == secretFilesDir
-> beacon.cer
-> [beacon time 0].xml
-> [beacon time 1].xml
-> challengeName
   -> instFileName
      secretFileName
      ...
      instFileName
      secretFileName
      revealFileName
   challengeName
   -> ...
-}

data ChallengeU = CU Challenge [InstanceU]

data InstanceU = IC {secret::Secret, instc::InstanceCont}
               | ID {secret::Secret, instd::InstanceDisc}
               | IR {secret::Secret, instr::InstanceRLWR}

-- Types used to generate and verify instances
type Zq q = Lol.ZqBasic (Reified q) Int64
type RRq q = Lol.RRq (RealMod (Reified q)) Double

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath -> FilePath
challengeFilesDir path = path </> "challenge-files"

-- | The root directory for challenge secrets.
secretFilesDir :: FilePath -> FilePath
secretFilesDir = challengeFilesDir

-- | The name for a challenge file is some string
-- with a .challenge extension.
challFilePath :: FilePath -> String -> FilePath
challFilePath path name = (challengeFilesDir path) </> name ++ ".challenge"

-- | The name for an instance file is some string followed by a hex ID
-- with a .instance extension.
instFilePath :: FilePath -> String -> Int32 -> FilePath
instFilePath path name idx = (challengeFilesDir path) </> name ++ "-" ++ intToHex idx ++ ".instance"

-- | The name for a secret file is some string followed by a hex ID
-- with the .secret extension.
secretFilePath :: FilePath -> String -> Int32 -> FilePath
secretFilePath path name idx = (secretFilesDir path) </> name ++ "-" ++ intToHex idx ++ ".secret"

-- | The name of a beacon XML file.
xmlFilePath :: FilePath -> Int64 -> FilePath
xmlFilePath path t = (secretFilesDir path) </> show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFilePath :: FilePath -> FilePath
certFilePath path = (secretFilesDir path) </> "beacon.cer"

intToHex :: Int32 -> String
intToHex x | x < 0 || x > 15 = error "hex value out of range"
intToHex x = printf "%X" x

throwErrorIf :: (Monad m) => Bool -> String -> ExceptT String m ()
throwErrorIf b = when b . throwError

throwErrorIfNot :: (Monad m) => Bool -> String -> ExceptT String m ()
throwErrorIfNot b = unless b . throwError

maybeThrowError :: (Monad m) => Maybe a -> String -> ExceptT String m a
maybeThrowError m str = do
  throwErrorIf (isNothing m) $ str
  return $ fromJust m

-- | Pretty printing of error messages.
printPassFail :: (MonadIO m, Default a)
                 => String -> String -> ExceptT String m a -> m a
printPassFail str pass e = do
  liftIO $ putStr str
  res <- runExceptT e
  val <- case res of
    (Left str) -> liftIO $ do
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn $ "FAIL: " ++ str
      return def
    (Right a) -> do
      liftIO $ setSGR [SetColor Foreground Vivid Green]
      liftIO $ putStrLn pass
      return a
  liftIO $ setSGR [SetColor Foreground Vivid Black]
  return val

-- | Yield a list of challenge names by getting all directory contents
-- and filtering on all directories whose names start with "chall".
challengeList :: FilePath -> IO [String]
challengeList path = do
  let challDir = challengeFilesDir path
  challDirExists <- doesDirectoryExist challDir
  unless challDirExists $ error $ "Could not find " ++ challDir
  putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (challDir </>)) =<<
    filter (("chall" ==) . take 5) <$> getDirectoryContents challDir
  when (null names) $ error "No challenges found."
  return names

checkFileExists :: (MonadIO m) => FilePath -> ExceptT String m ()
checkFileExists file = do
  fileExists <- liftIO $ doesFileExist file
  throwErrorIfNot fileExists $
    "Error reading " ++ file ++ ": file does not exist."

-- | Read a serialized protobuffer from a file.
readProtoType :: (ReflectDescriptor a, Wire a, MonadIO m) => FilePath -> ExceptT String m a
readProtoType file = do
  checkFileExists file
  bs <- liftIO $ BS.readFile file
  case messageGet bs of
    (Left str) -> throwError $
      "Error when reading from protocol buffer. Got string " ++ str
    (Right (a,bs')) ->
      throwErrorIfNot (BS.null bs')
        "Error when reading from protocol buffer. There were leftover bits!"
      return a

-- | Parse the beacon time/offset used to reveal a challenge.
parseBeaconAddr :: (Monad m) => Challenge -> ExceptT String m BeaconAddr
parseBeaconAddr Challenge{..} = do
  let time = fromIntegral beaconTime
      offset = fromIntegral beaconOffset
  -- validate the time and offset
  throwErrorIf ((time `mod` beaconInterval /= 0) ||
                offset < 0 ||
                offset >= bytesPerBeacon) $
    "Invalid beacon address."
  return $ BA time offset

-- | Yield the secret index (for a challenge), given a beacon record
-- and a byte offset.
secretIdx :: Int32 -> Record -> Int32 -> Int32
secretIdx numInstances record byteOffset =
  let output = outputValue record
      byte = unpack output !! (fromIntegral byteOffset)
  in fromIntegral $ fromIntegral byte `mod` (fromIntegral numInstances)
