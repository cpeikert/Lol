
import Challenges.Beacon
import Challenges.Common

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Trans (lift)

import Crypto.Lol (fromJust', intLog)

import Data.BooleanList (byteStringToBooleanList)
import Data.ByteString.Lazy (toStrict, writeFile)
import Data.Map (Map, lookup, empty, insert)

import Net.Beacon

import Network.HTTP.Conduit (simpleHttp)

import Prelude hiding (lookup, writeFile)

import System.Console.ANSI
import System.Directory (doesFileExist, removeFile, getDirectoryContents)
--import System.IO

main :: IO ()
main = do
  abspath <- absPath
  challs <- filter (("chall" ==) . (take 5)) <$> (getDirectoryContents $ abspath </> challengeFilesDir)
  flip evalStateT empty $ mapM_ (revealChallengeMain abspath) challs
  return ()

revealChallengeMain :: FilePath -> String -> StateT (Map Int Record) IO ()
revealChallengeMain abspath name = do
  lift $ putStrLn $ "Revealing challenge " ++ name ++ "..."

  let challDir = abspath </> challengeFilesDir </> name
      revealPath = challDir </> revealFileName

  revealExists <- lift $ doesFileExist revealPath
  when (not revealExists) $ error $ revealPath ++ "does not exist."

  [name, timeStr, offsetStr] <- lift $ lines <$> readFile revealPath
  let time = read timeStr :: Int
      offset = read offsetStr :: Int
  numInsts <- lift $ length <$> filter ((".bin" ==) . lastK 4) <$> getDirectoryContents challDir
  let numBits = intLog 2 numInsts

  lastBeaconTime <- lift $ (timeStamp . fromJust' "Failed to get last beacon") <$> getLastRecord

  when ((time `mod` beaconInterval /= 0) || offset < 0 || offset+numBits > bitsPerBeacon) $ lift $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Invalid beacon position."
    setSGR [SetColor Foreground Vivid Black]
    error ""

  when (time > lastBeaconTime) $ lift $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Can't reveal challenge " ++ name ++ ": it's time has not yet come. Please wait " ++ 
      (show $ time-lastBeaconTime) ++ " seconds for the assigned beacon."
    setSGR [SetColor Foreground Vivid Black]
    error ""

  rec <- retrieveRecord time
  let secretIdx = getSecretIdx rec offset numBits
      secDir = abspath </> secretFilesDir </> name
      secFile = secDir </> (secretFileName secretIdx)

  secFileExists <- lift $ doesFileExist secFile
  when (not secFileExists) $ error $ secFile ++ " does not exist."

  lift $ putStrLn $ "Removing " ++ secFile
  lift $ removeFile secFile

  lift $ writeBeaconXML time (abspath </> secretFilesDir </> name)

-- attempt to find the record in the state, otherwise read it from NIST
retrieveRecord :: Int -> StateT (Map Int Record) IO Record
retrieveRecord t = do
  mrec <- gets (lookup t)
  case mrec of
    (Just r) -> return r
    Nothing -> do
      lift $ putStrLn $ "Downloading record " ++ (show t)
      r <- lift $ getCurrentRecord t
      case r of
        (Just r') -> do
          modify (insert t r')
          return r'
        Nothing -> error $ "Couldn't get record " ++ (show t) ++ "from NIST servers."

getSecretIdx :: Record -> Int -> Int -> Int
getSecretIdx record offset numBits =
  let output = outputValue record
      bits = take numBits $ drop offset $ byteStringToBooleanList $ toStrict output
      parseBitString [] = 0
      parseBitString (True:xs) = 1+(2*(parseBitString xs))
      parseBitString (False:xs) = 2*(parseBitString xs)
  in parseBitString bits

-- returns last k characters in a string
lastK :: Int -> String -> String
lastK k s =
  let l = length s
  in if l > k
     then drop (l-k) s
     else s

writeBeaconXML :: Int -> FilePath -> IO ()
writeBeaconXML t path = do
  beacon <- simpleHttp $ "http://beacon.nist.gov/rest/record/" ++ (show t)
  writeFile (path ++ "/" ++ (show t) ++ ".xml") beacon
