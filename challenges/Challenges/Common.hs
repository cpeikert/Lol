
module Challenges.Common
(numInstances
,challengeFilesDir
,secretFilesDir
,instFileName
,secretFileName
,revealFileName
,xmlFileName
,certFileName
,(</>)
,getPath
,printPassFail
,getChallengeList
,readRevealData
,getSecretIdx) where

import Challenges.Beacon

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Lazy (ByteString, toStrict, unpack)
import Data.ByteString.Builder
import Data.Char (toUpper)
import Data.Default (Default(..))

import Net.Beacon

import System.Console.ANSI
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)

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
    otherwise -> error $
      "Valid args: [-p path] where 'path' is relative to './'." ++ 
      "If no path is provided, the program will guess a path."

-- | The number of instances to generate per challenge.
numInstances :: Int
numInstances = 
  if numInsts > 256
  then error "numInstances must be <= 256"
  else numInsts
  where numInsts = 16

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath
challengeFilesDir = "challenge-files"

-- | The root directory for challenge secrets.
secretFilesDir :: FilePath
secretFilesDir = challengeFilesDir

-- | The name for instance files is some string followed by a hex ID with a .instance extension.
instFileName :: String -> Int -> FilePath
instFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".instance"

-- | The name for secret files is some string followed by a hex ID with the .secret extension.
secretFileName :: String -> Int -> FilePath
secretFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".secret"

-- | The name for the file that contains the beacon information for each challenge.
revealFileName :: FilePath
revealFileName = "beaconTime.txt"

-- | The name for beacon xml files.
xmlFileName :: Int -> FilePath
xmlFileName t = (show t) ++ ".xml"

-- | The name for the NIST X509 certificate.
certFileName :: FilePath
certFileName = "beacon.cer"

intToHex :: Int -> String
intToHex x | x < 0 || x > 15 = error "hex value out of range"
intToHex x = printf "%X" x

showHexBS :: ByteString -> String
showHexBS = map toUpper . tail . init . show . toLazyByteString . byteStringHex . toStrict

-- | @"a" </> "b"@ is the string @"a/b"@.
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- for testing purposes
absPath :: IO FilePath
absPath = do
  inTopLevelLol <- doesDirectoryExist "challenges"
  return $ if inTopLevelLol
    then "./challenges"
    else "."

-- | Pretty printing of error messages.
printPassFail :: (MonadIO m, Default a) => String -> String -> ExceptT String m a -> m a
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


-- | Get a list of challenge names by getting all directory contents and filtering
-- on all directories whose first five characters are "chall".
getChallengeList :: FilePath -> IO [String]
getChallengeList challDir = do
  putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (challDir </>)) =<< 
    filter (("chall" ==) . (take 5)) <$> getDirectoryContents challDir
  when (length names == 0) $ error "No challenges found."
  return names

-- | Parse the beacon time/offset used to reveal a challenge.
readRevealData :: (MonadIO m) => FilePath -> ExceptT String m BeaconPos
readRevealData path = do
  let revealPath = path </> revealFileName
  revealExists <- liftIO $ doesFileExist revealPath
  when (not revealExists) $ throwError $ revealPath ++ " does not exist."
  [timeStr, offsetStr] <- liftIO $ lines <$> readFile revealPath
  let time = read timeStr
      offset = read offsetStr
  -- validate the time and offset
  when ((time `mod` beaconInterval /= 0) || offset < 0 || offset >= bytesPerBeacon) $ 
    throwError "Invalid beacon position."
  return $ BP time offset

-- | Given a beacon record and a byte offset, return the secret index for this challenge.
getSecretIdx :: Record -> Int -> Int
getSecretIdx record byteOffset =
  let output = outputValue record
      byte = (unpack output) !! byteOffset
  in (fromIntegral byte) `mod` numInstances
