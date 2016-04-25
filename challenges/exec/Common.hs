{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Common
( ChallengeID, InstanceID, Zq, RRq, ChallengeU(..), InstanceU(..)
, challengeFilesDir, challFilePath, instFilePath, secretFilePath
, xmlFilePath, certFilePath
, challengeList
, suppressedSecretID
, parseBeaconAddr
, isBeaconAvailable
, readProtoType
, checkFileExists
, printPassFail
, throwErrorIf, throwErrorIfNot, maybeThrowError
) where

import           Beacon
import           Crypto.Challenges.RLWE.Proto.RLWE.Challenge
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceCont
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceDisc
import           Crypto.Challenges.RLWE.Proto.RLWE.InstanceRLWR
import           Crypto.Challenges.RLWE.Proto.RLWE.Secret
import qualified Crypto.Lol                                     as Lol
import           Crypto.Lol.Reflects

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

type ChallengeID = Int32
type InstanceID = Int32

data ChallengeU = CU Challenge [InstanceU]

data InstanceU = IC {secret::Secret, instc::InstanceCont}
               | ID {secret::Secret, instd::InstanceDisc}
               | IR {secret::Secret, instr::InstanceRLWR}

-- Types used to generate and verify instances
type Zq q = Lol.ZqBasic (Reified q) Int64
type RRq q = Lol.RRq (RealMod (Reified q)) Double

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath -> FilePath
challengeFilesDir path = path </> "rlwe-challenges"

-- | The name for a challenge file is some string
-- with a .challenge extension.
challFilePath :: FilePath -> String -> FilePath
challFilePath path name = challengeFilesDir path </> name ++ ".challenge"

-- | The name for an instance file is some string followed by a hex ID
-- with a .instance extension.
instFilePath :: FilePath -> String -> InstanceID -> FilePath
instFilePath path name instID = challengeFilesDir path </> name ++ "-" ++
  instIDString instID ++ ".instance"

-- | The name for a secret file is some string followed by a hex ID
-- with the .secret extension.
secretFilePath :: FilePath -> String -> InstanceID -> FilePath
secretFilePath path name instID = challengeFilesDir path </> name ++ "-" ++
  instIDString instID ++ ".secret"

-- | The name of a beacon XML file.
xmlFilePath :: FilePath -> BeaconEpoch -> FilePath
xmlFilePath path t = challengeFilesDir path </> show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFilePath :: FilePath -> FilePath
certFilePath path = challengeFilesDir path </> "beacon.cer"

instIDString :: InstanceID -> String
instIDString = printf "%2X"

throwErrorIf :: (MonadError String m) => Bool -> String -> m ()
throwErrorIf b = when b . throwError

throwErrorIfNot :: (MonadError String m) => Bool -> String -> m ()
throwErrorIfNot b = unless b . throwError

maybeThrowError :: (MonadError String m) => Maybe a -> String -> m a
maybeThrowError m str = do
  throwErrorIf (isNothing m) str
  return $ fromJust m

-- | Pretty printing of error messages.
printPassFail :: (MonadIO m, MonadError String m, Default a)
                 => String -> String -> ExceptT String m a -> m a
printPassFail str pass e = do
  liftIO $ putStr str
  res <- runExceptT e
  val <- case res of
    (Left st) -> liftIO $ do
      setSGR [SetColor Foreground Vivid Red]
      putStrLn $ "FAIL: " ++ st
      return def
    (Right a) -> liftIO $ do
      setSGR [SetColor Foreground Vivid Green]
      putStrLn pass
      return a
  liftIO $ setSGR [SetColor Foreground Vivid Black]
  return val

-- | Yield a list of challenge names by getting all directory contents
-- and filtering on all directories whose names start with "chall".
challengeList :: (MonadIO m) => FilePath -> m [String]
challengeList path = liftIO $ do
  let challDir = challengeFilesDir path
  challDirExists <- doesDirectoryExist challDir
  unless challDirExists $ error $ "Could not find " ++ challDir
  -- putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (challDir </>)) =<<
    filter (("chall" ==) . take 5) <$> getDirectoryContents challDir
  when (null names) $ error "No challenges found."
  return names

checkFileExists :: (MonadIO m, MonadError String m) => FilePath -> m ()
checkFileExists file = do
  fileExists <- liftIO $ doesFileExist file
  throwErrorIfNot fileExists $
    "Error reading " ++ file ++ ": file does not exist."

-- | Read a serialized protobuffer from a file.
readProtoType :: (ReflectDescriptor a, Wire a, MonadIO m, MonadError String m)
                 => FilePath -> m a
readProtoType file = do
  checkFileExists file
  bs <- liftIO $ BS.readFile file
  case messageGet bs of
    (Left str) -> throwError $
      "Error when reading from protocol buffer. Got string " ++ str
    (Right (a,bs')) -> do
      throwErrorIfNot (BS.null bs')
        "Error when reading from protocol buffer. There were leftover bits!"
      return a

-- | Parse the beacon time/offset used to reveal a challenge.
parseBeaconAddr :: (MonadError String m) => Challenge -> m BeaconAddr
parseBeaconAddr Challenge{..} = do
  let time = beaconTime
      offset = beaconOffset
  -- validate the time and offset
  throwErrorIf ((time `mod` beaconInterval /= 0) ||
                offset < 0 ||
                offset >= bytesPerBeacon)
    "Invalid beacon address."
  return $ BA time offset

-- | Yield the ID of the suppressed secret for a challenge, given a
-- beacon record and a byte offset.
suppressedSecretID :: InstanceID -> Record -> Int32 -> InstanceID
suppressedSecretID numInstances record byteOffset =
  let output = outputValue record
      byte = unpack output !! fromIntegral byteOffset
  in fromIntegral byte `mod` numInstances
