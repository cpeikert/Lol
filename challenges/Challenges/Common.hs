
module Challenges.Common where

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

numInstances :: Int
numInstances = 16

challengeFilesDir :: FilePath
challengeFilesDir = "challenge-files"

secretFilesDir :: FilePath
secretFilesDir = challengeFilesDir

challengeName :: Int -> Int -> Double -> FilePath
challengeName m q v = "chall-m" ++ (show m) ++ "-q" ++ (show q) ++ "-v" ++ (show v)

instFileName :: String -> Int -> FilePath
instFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".instance"

secretFileName :: String -> Int -> FilePath
secretFileName name idx = name ++ "-" ++ (intToHex idx) ++ ".secret"

revealFileName :: FilePath
revealFileName = "revealData.txt"

xmlFileName :: Int -> FilePath
xmlFileName t = (show t) ++ ".xml"

certFileName :: FilePath
certFileName = "beacon.cer"

intToHex :: Int -> String
intToHex x | x < 0 || x > 15 = error "hex value out of range"
intToHex x = printf "%X" x

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
