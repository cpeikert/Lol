{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Common where

import Beacon

import           Crypto.Lol.Cyclotomic.Tensor.CPP
import           Crypto.Lol.Types     hiding (RRq)
import qualified Crypto.Lol.Types as RRq

import Crypto.Proto.RLWE.Challenges.Challenge
import Crypto.Proto.RLWE.Challenges.InstanceCont
import Crypto.Proto.RLWE.Challenges.InstanceDisc
import Crypto.Proto.RLWE.Challenges.InstanceRLWR
import Crypto.Proto.RLWE.Challenges.Secret

import Crypto.Random.DRBG

import Control.Monad.Except

import           Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy as BS
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
type InstDRBG = GenBuffered CtrDRBG

-- | Tensor type used to generate and verify instances
type T = CT

data ChallengeU = CU !Challenge ![InstanceU]

data InstanceU = IC {secret :: !Secret, instc :: !InstanceCont}
               | ID {secret :: !Secret, instd :: !InstanceDisc}
               | IR {secret :: !Secret, instr :: !InstanceRLWR}

-- Types used to generate and verify instances
type Zq q = ZqBasic q Int64
type RRq q = RRq.RRq q Double

-- | Yield a list of challenge names by getting all directory contents
-- and filtering on all directories whose names start with "chall".
challengeList :: (MonadIO m) => FilePath -> m [String]
challengeList path = liftIO $ do
  challDirExists <- doesDirectoryExist path
  unless challDirExists $ error $ "Could not find " ++ path
  -- putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (path </>)) =<<
    filter (("chall" ==) . take 5) <$> getDirectoryContents path
  when (null names) $ error "No challenges found."
  return names

checkFileExists :: (MonadIO m, MonadError String m) => FilePath -> m ()
checkFileExists file = do
  fileExists <- liftIO $ doesFileExist file
  throwErrorUnless fileExists $
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
      throwErrorUnless (BS.null bs')
        "Error when reading from protocol buffer. There were leftover bits!"
      return a

-- | Parse the beacon time/offset used to reveal a challenge.
parseBeaconAddr :: (MonadError String m) => Challenge -> m BeaconAddr
parseBeaconAddr Challenge{..} = do
  let ba = BA beaconEpoch beaconOffset
  -- validate the time and offset
  throwErrorUnless (validBeaconAddr ba) $ "Invalid beacon address: " ++ show ba
  return ba

-- | Yield the ID of the suppressed secret for a challenge, given a
-- beacon record and a byte offset.
suppressedSecretID :: InstanceID -> Record -> Int32 -> InstanceID
suppressedSecretID numInstances record byteOffset =
  let output = outputValue record
      byte = unpack output !! fromIntegral byteOffset
  in fromIntegral byte `mod` numInstances

-- * Directory Structure

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath -> String -> FilePath
challengeFilesDir path name = path </> name

-- | The name for a challenge file is some string
-- with a .challenge extension.
challFilePath :: FilePath -> String -> FilePath
challFilePath path name = challengeFilesDir path name </> name ++ ".challenge"

-- | The name for an instance file is some string followed by a hex ID
-- with a .instance extension.
instFilePath :: FilePath -> String -> InstanceID -> FilePath
instFilePath path name instID = challengeFilesDir path name </> name ++ "-" ++
  instIDString instID ++ ".instance"

-- | The name for a secret file is some string followed by a hex ID
-- with the .secret extension.
secretFilePath :: FilePath -> String -> InstanceID -> FilePath
secretFilePath path name instID = challengeFilesDir path name </> name ++ "-" ++
  instIDString instID ++ ".secret"

-- | The name of a beacon XML file.
beaconFilePath :: FilePath -> BeaconEpoch -> FilePath
beaconFilePath path t = path </> "epoch-" ++ show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFilePath :: FilePath -> FilePath
certFilePath path = path </> "beacon.cer"

instIDString :: InstanceID -> String
instIDString = printf "%02X"

-- * Functions for easy exceptions.

-- | Throw an error if the condition is 'True'.
throwErrorIf :: (MonadError String m) => Bool -> String -> m ()
throwErrorIf b = when b . throwError

-- | Throw an error if the condition is 'False'.
throwErrorUnless :: (MonadError String m) => Bool -> String -> m ()
throwErrorUnless b = unless b . throwError

-- | Throw an error if the input is 'Nothing'.
maybeThrowError :: (MonadError String m) => Maybe a -> String -> m a
maybeThrowError m str = do
  throwErrorIf (isNothing m) str
  return $ fromJust m

-- | Pretty printing of error messages.
printPassFailGeneric :: (MonadIO m)
                 => Color -> String -> String -> ExceptT String m a -> m (Maybe a)
printPassFailGeneric failColor str pass e = do
  liftIO $ putStr str
  res <- runExceptT e
  case res of
    (Left st) -> do printANSI failColor st
                    return Nothing
    (Right a) -> do printANSI Green pass
                    return $ Just a

printPassFail, printPassWarn :: (MonadIO m)
  => String -> String -> ExceptT String m a -> m (Maybe a)
printPassFail = printPassFailGeneric Red
printPassWarn = printPassFailGeneric Yellow

printANSI :: (MonadIO m) => Color -> String -> m ()
printANSI sgr str = liftIO $ do
  setSGR [SetColor Foreground Vivid sgr]
  putStrLn str
  setSGR [Reset]
