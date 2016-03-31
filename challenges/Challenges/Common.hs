
module Challenges.Common
(numInstances
,challengeFilesDir
,secretFilesDir
,challengeName
,instFileName
,secretFileName
,revealFileName
,xmlFileName
,certFileName
,(</>)
,absPath
,printPassFail) where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Builder
import Data.Char (toUpper)
import Data.Default (Default(..))

import System.Console.ANSI
import System.Directory (doesDirectoryExist)

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

-- | The name for each challenge directory.
challengeName :: Int -> Int -> Double -> FilePath
challengeName m q v = "chall-m" ++ (show m) ++ "-q" ++ (show q) ++ "-v" ++ (show v)

-- | The name for instance files is some string followed by a hex ID with a .instance extension.
instFileName :: String -> Int -> FilePath
instFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".instance"

-- | The name for secret files is some string followed by a hex ID with the .secret extension.
secretFileName :: String -> Int -> FilePath
secretFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".secret"

-- | The name for the file that contains the beacon information for each challenge.
revealFileName :: FilePath
revealFileName = "revealData.txt"

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
    then "challenges"
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
