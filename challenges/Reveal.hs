{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
import Challenges.LWE
import Challenges.MakeReader
import Challenges.Parameters
import Challenges.Random
import Challenges.Reader
import Challenges.Verify
import Challenges.Writer

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

import Crypto.Lol (fromJust',RT,CT,Proxy(..))

import Data.BooleanList (byteStringToBooleanList)
import qualified Data.ByteString.Lazy as B
import Data.List (nub, partition)
import Data.Map (fromList, (!), Map)
import Data.Maybe (catMaybes)

import Net.Beacon

import Network.HTTP.Conduit (simpleHttp)

import System.Console.ANSI
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, removeDirectoryRecursive)
import System.IO

main :: IO ()
main = do

  challDirExists <- doesDirectoryExist challengePath
  tsDirExists <- doesDirectoryExist topSecretPath

  when (not challDirExists) $ error $ challengePath ++ " does not exist."
  when (not tsDirExists) $ error $ topSecretPath ++ " does not exist."

  lastBeaconTime <- (timeStamp . fromJust' "Failed to get last beacon") <$> getLastRecord

  putStrLn $ "Reading challenges and corresponding " ++ topSecretPath ++ "..."
  -- EAC: has the unforunate disadvantage of reading all the challenges into memory at once
  allChalls <- readChallenges topSecretPath (Proxy::Proxy RT)

  let (challs,notReady) = partition ((lastBeaconTime >=) . beaconTime) allChalls
  flip mapM_ notReady $ \(Challenge {..}) -> do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Can't reveal challenge " ++ name ++ ": it's time has not yet come. Please wait " ++ 
     (show $ beaconTime-lastBeaconTime) ++ " seconds for the assigned beacon."
    setSGR [SetColor Foreground Vivid Black]

  let allPos = concatMap allBeaconPos challs
      distinctPos = nub allPos
  when (length allPos > length distinctPos) $ error $
    "Some challenges repeat random bits!\n" ++ (show allPos)

  let beaconTimes = nub $ map beaconTime challs

  -- verify that beacon times are well-formed
  flip mapM_ beaconTimes $ \time -> do
    setSGR [SetColor Foreground Dull Yellow]
    when (time `mod` beaconInterval /= 0) $ 
      putStrLn $ "WARNING: beacon time " ++ (show time) ++ " is not a multiple of " ++ (show beaconInterval)
    setSGR [SetColor Foreground Vivid Black]

  if (null notReady)
  then putStrLn "All challenges look well-formed"
  else putStrLn "All remaining challenges look well-formed"

  putStrLn $ "Grabbing " ++ (show $ length beaconTimes) ++ " beacon values for these challenges"
  records <- mapM ((fromJust' "current beacon" <$>) . getCurrentRecord) beaconTimes
  let beaconMap = fromList $ zip beaconTimes records

  -- remove secret instance
  challs' <- mapM (setSecretIdx beaconMap) challs

  putStrLn $ "Writing publishable challenge secrets to directory " ++ revealPath
  mapM_ writeChallengeSecrets challs'

  -- write out beacon xml files
  putStrLn "Writing beacon XML files..."  
  mapM_ writeBeaconXML beaconTimes

  putStrLn $ "Deleting " ++ topSecretPath
  removeDirectoryRecursive topSecretPath

getKeys :: SecretLWEChallenge v t m zp -> ChallengeSecrets t m zp
getKeys (SLWEChallenge _ sinsts) = ChallSecrets $ map (\(SLWEInstance idx sk _) -> InstSecret idx sk) sinsts

  --EAC: todo: verify signature on XML files
--removeSecrets :: SecretLWEChallenge v t m zp -> Int -> Int -> (ChallengeSecrets t m zp, LWEChallenge v t m zp)
writeChallengeSecrets :: Challenge -> IO ()
writeChallengeSecrets (Challenge{..}) = writeSecrets revealPath name $ getKeys challenge


setSecretIdx :: Map Int Record -> Challenge -> IO Challenge
setSecretIdx recordMap chall@(Challenge{..}) = do
  let idx = getSecretIdx recordMap chall
      removeInstance (x@(SLWEInstance id _ _):xs) idx | id == idx = xs
                                                      | otherwise = x:(removeInstance xs idx)
      updateSLWEChallenge x@(SLWEChallenge v insts) idx = SLWEChallenge v $ removeInstance insts idx
  putStrLn $ "Removing secret key " ++ (show idx) ++ " from challenge " ++ name
  return $ Challenge {secretIdx = Just idx, 
                challenge = updateSLWEChallenge challenge idx, 
                name, numInsts, numBits, beaconTime, bitOffset, svar}

getSecretIdx :: Map Int Record -> Challenge -> Int
getSecretIdx recordMap chall@(Challenge{..}) =
  let output = outputValue $ recordMap ! beaconTime
      bits = take numBits $ drop bitOffset $ byteStringToBooleanList $ B.toStrict output
      parseBitString [] = 0
      parseBitString (True:xs) = 1+(2*(parseBitString xs))
      parseBitString (False:xs) = 2*(parseBitString xs)
  in parseBitString bits

writeBeaconXML :: Int -> IO ()
writeBeaconXML ts = do
  createDirectoryIfMissing False revealPath
  beacon <- simpleHttp $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  let path = revealPath ++ "/" ++ (show ts) ++ ".xml"
  putStrLn $ "\t" ++ path
  B.writeFile path beacon


allBeaconPos :: Challenge -> [BeaconPos]
allBeaconPos (Challenge {..}) = map (\offset -> BP beaconTime offset 0 0 0) $ take numBits $ [bitOffset..]