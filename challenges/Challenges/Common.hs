

module Challenges.Common where

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
-> challengeName
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

(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

-- for testing purposes
absPath :: IO FilePath
absPath = do
  inTopLevelLol <- doesDirectoryExist "challenges"
  return $ if inTopLevelLol
    then "challenges"
    else "."