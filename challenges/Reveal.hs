
import Challenges.LWE
import Challenges.MakeReader
import Challenges.Reader
import Challenges.Verify

import Control.Applicative ((<$>))
import Control.Monad (when)

import Crypto.Lol (fromJust',RT,CT,Proxy(..))


import qualified Data.ByteString.Lazy as B
import Data.List (nub)
import Data.Maybe (catMaybes)

import Net.Beacon

import Network.HTTP.Conduit (simpleHttp)

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO

main :: IO ()
main = do
  lastBeaconTime <- (timeStamp . fromJust' "Failed to get last beacon") <$> getLastRecord
  print lastBeaconTime

  challs <- readChallenges (Proxy::Proxy RT)

  -- EAC: has the unforunate disadvantage of reading all the challenges into memory at once
  beaconTimes <- mapM revealChallenge challs

  -- write out becaon xml files
  putStrLn "Writing beacon XML files..."
  let distinctTimes = nub $ catMaybes beaconTimes
  mapM_ writeBeaconXML distinctTimes


  --EAC: todo: verify signature on XML files


writeBeaconXML :: Int -> IO ()
writeBeaconXML ts = do
  createDirectoryIfMissing False revealPath
  beacon <- simpleHttp $ "http://beacon.nist.gov/rest/record/" ++ (show ts)
  let path = revealPath ++ "/" ++ (show ts) ++ ".xml"
  putStrLn $ "\t" ++ path
  B.writeFile path beacon


--returns the beacon timestamp used by this challenge
revealChallenge :: Challenge -> IO (Maybe Int)
revealChallenge (Challenge _ x) = return $ Just $ beaconTime x