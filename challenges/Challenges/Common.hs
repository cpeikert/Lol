

module Challenges.Common where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.ByteString.Lazy (ByteString, toStrict)
import Data.ByteString.Builder
import Data.Char (toUpper)
import Data.Default

import System.Console.ANSI
import System.Directory

{- Directory structure:

challengeFilesDir
-> challengeName
   -> instFileName
      ...
      instFileName
      revealFileName
   challengeName
   -> ...
secretFilesDir
-> beacon.cer
   [beacon time 0].xml
   [beacon time 1].xml
   ...
   challengeName
   -> secretFileName
      ...
      secretFileName
   challengeName
   -> ...
-}

challengeFilesDir :: FilePath
challengeFilesDir = "challenge-files"

secretFilesDir :: FilePath
secretFilesDir = "secret-files"

challengeName :: Int -> Int -> Double -> FilePath
challengeName m p v = "chall-m" ++ (show m) ++ "-p" ++ (show p) ++ "-v" ++ (show v)

instFileName :: Int -> FilePath
instFileName idx = "instance" ++ (show idx) ++ ".bin"

secretFileName :: Int -> FilePath
secretFileName idx = "secret" ++ (show idx) ++ ".bin"

revealFileName :: FilePath
revealFileName = "revealData.txt"

xmlFileName :: Int -> FilePath
xmlFileName t = (show t) ++ ".xml"

certFileName :: FilePath
certFileName = "beacon.cer"

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- for testing purposes
absPath :: IO FilePath
absPath = do
  inTopLevelLol <- doesDirectoryExist "challenges"
  return $ if inTopLevelLol
    then "challenges"
    else "."

checkChallDirExists :: IO ()
checkChallDirExists = do
  abspath <- absPath
  let challDir = abspath </> challengeFilesDir
  challDirExists <- doesDirectoryExist challDir
  when (not challDirExists) $ error $ "Could not find " ++ challDir

showHexBS :: ByteString -> String
showHexBS = map toUpper . tail . init . show . toLazyByteString . byteStringHex . toStrict

printPassFail :: (MonadIO m, Default a) => String -> ExceptT String m a -> m a
printPassFail str e = do
  liftIO $ putStr str
  res <- runExceptT e
  val <- case res of
    (Left str) -> liftIO $ do
      liftIO $ setSGR [SetColor Foreground Vivid Red]
      liftIO $ putStrLn $ "FAIL: " ++ str
      return def
    (Right a) -> do
      liftIO $ setSGR [SetColor Foreground Vivid Green]
      liftIO $ putStrLn "DONE"
      return a
  liftIO $ setSGR [SetColor Foreground Vivid Black]
  return val
