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
) where

import Crypto.Challenges.RLWE.Beacon
import Crypto.Challenges.RLWE.Proto.RLWE.Challenge

import Control.Monad.Except

import           Data.ByteString.Lazy (unpack)
import qualified Data.ByteString.Lazy as BS
import           Data.Default         (Default (..))
import           Data.Int

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
xmlFilePath :: FilePath -> Int -> FilePath
xmlFilePath path t = (secretFilesDir path) </> show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFilePath :: FilePath -> FilePath
certFilePath path = (secretFilesDir path) </> "beacon.cer"

intToHex :: Int32 -> String
intToHex x | x < 0 || x > 15 = error "hex value out of range"
intToHex x = printf "%X" x

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

-- | Read a serialized protobuffer from a file.
readProtoType :: (ReflectDescriptor a, Wire a, MonadIO m) => FilePath -> ExceptT String m a
readProtoType file = do
  fileExists <- liftIO $ doesFileExist file
  unless fileExists $ throwError $
    "Error reading " ++ file ++ ": file does not exist."
  bs <- liftIO $ BS.readFile file
  case messageGet bs of
    (Left str) -> throwError $ "Error when reading from protocol buffer. Got string " ++ str
    (Right (a,bs')) ->
      if BS.null bs'
      then return a
      else throwError $ "Error when reading from protocol buffer. There were leftover bits!"

-- | Parse the beacon time/offset used to reveal a challenge.
parseBeaconAddr :: (Monad m) => Challenge -> ExceptT String m BeaconAddr
parseBeaconAddr Challenge{..} = do
  let time = fromIntegral beaconTime
      offset = fromIntegral beaconOffset
  -- validate the time and offset
  when ((time `mod` beaconInterval /= 0) ||
        offset < 0 ||
        offset >= bytesPerBeacon) $
    throwError "Invalid beacon address."
  return $ BA time offset

-- | Yield the secret index (for a challenge), given a beacon record
-- and a byte offset.
secretIdx :: Int -> Record -> Int -> Int32
secretIdx numInstances record byteOffset =
  let output = outputValue record
      byte = unpack output !! byteOffset
  in fromIntegral $ fromIntegral byte `mod` numInstances
