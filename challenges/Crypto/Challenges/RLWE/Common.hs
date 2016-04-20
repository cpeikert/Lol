module Crypto.Challenges.RLWE.Common
( numInstances
, challengeFilesDir, secretFilesDir
, instFileName, secretFileName, revealFileName
, xmlFileName, certFileName
, (</>)
, getPath
, printPassFail
, challengeList
, readRevealData
, secretIdx
) where

import Crypto.Challenges.RLWE.Beacon

import Control.Monad.Except
import Data.ByteString.Lazy (unpack)
import Data.Default         (Default (..))
import Data.Word

import Net.Beacon

import System.Console.ANSI
import System.Directory    (doesDirectoryExist, doesFileExist,
                            getDirectoryContents)
import System.Environment  (getArgs)
import System.FilePath     ((</>))

import Text.Printf

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

-- | The number of instances to generate per challenge (parameter
-- set).
numInstances :: Int
numInstances = 16               --  must be <= 256

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath
challengeFilesDir = "challenge-files"

-- | The root directory for challenge secrets.
secretFilesDir :: FilePath
secretFilesDir = challengeFilesDir

-- | The name for an instance file is some string followed by a hex ID
-- with a .instance extension.
instFileName :: String -> Word32 -> FilePath
instFileName name idx = name ++ "-" ++ intToHex idx ++ ".instance"

-- | The name for a secret file is some string followed by a hex ID
-- with the .secret extension.
secretFileName :: String -> Word32 -> FilePath
secretFileName name idx = name ++ "-" ++ intToHex idx ++ ".secret"

-- | The filename that contains the beacon information for each
-- challenge.
revealFileName :: FilePath
revealFileName = "beaconTime.txt"

-- | The name of a beacon XML file.
xmlFileName :: Int -> FilePath
xmlFileName t = show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFileName :: FilePath
certFileName = "beacon.cer"

-- | Read command line args, guess a path, or print the help message.
getPath :: IO FilePath
getPath = do
  args <- getArgs
  case args of
    [] -> do
      path <- absPath
      putStrLn $ "No path provided. Guessing path is \"" ++ path ++ "\""
      return path
    ["-p",path] -> do
      dirExists <- doesDirectoryExist path
      if dirExists
      then return $ "." </> path
      else error $ ("." </> path) ++ " does not exist."
    _ -> error $
      "Valid args: [-p path] where 'path' is relative to './'." ++
      "If no path is provided, the program will guess a path."

intToHex :: Word32 -> String
intToHex x | x < 0 || x > 15 = error "hex value out of range"
intToHex x = printf "%X" x

-- for testing purposes
absPath :: IO FilePath
absPath = do
  inTopLevelLol <- doesDirectoryExist "challenges"
  return $ if inTopLevelLol
    then "./challenges"
    else "."

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
challengeList challDir = do
  putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (challDir </>)) =<<
    filter (("chall" ==) . take 5) <$> getDirectoryContents challDir
  when (null names) $ error "No challenges found."
  return names

-- | Parse the beacon time/offset used to reveal a challenge.
readRevealData :: (MonadIO m) => FilePath -> ExceptT String m BeaconPos
readRevealData path = do
  let revealPath = path </> revealFileName
  revealExists <- liftIO $ doesFileExist revealPath
  unless revealExists $ throwError $ revealPath ++ " does not exist."
  [timeStr, offsetStr] <- liftIO $ lines <$> readFile revealPath
  let time = read timeStr
      offset = read offsetStr
  -- validate the time and offset
  when ((time `mod` beaconInterval /= 0) || offset < 0 || offset >= bytesPerBeacon) $
    throwError "Invalid beacon position."
  return $ BP time offset

-- | Yield the secret index (for a challenge), given a beacon record
-- and a byte offset.
secretIdx :: Record -> Int -> Word32
secretIdx record byteOffset =
  let output = outputValue record
      byte = unpack output !! byteOffset
  in fromIntegral $ fromIntegral byte `mod` numInstances
