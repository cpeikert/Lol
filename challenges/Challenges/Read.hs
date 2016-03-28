
module Challenges.Read where

import Challenges.Beacon
import Challenges.Common

import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class

import Data.BooleanList (byteStringToBooleanList)
import Data.ByteString.Lazy (toStrict)

import Net.Beacon

import System.Directory

readRevealData :: (MonadIO m) => FilePath -> ExceptT String m BeaconPos
readRevealData path = do
  let revealPath = path </> revealFileName
  revealExists <- liftIO $ doesFileExist revealPath
  when (not revealExists) $ throwError $ revealPath ++ "does not exist."
  [timeStr, offsetStr] <- liftIO $ lines <$> readFile revealPath
  return $ BP (read timeStr) (read offsetStr)

getSecretIdx :: Record -> Int -> Int -> Int
getSecretIdx record offset numBits =
  let output = outputValue record
      bits = take numBits $ drop offset $ byteStringToBooleanList $ toStrict output
      parseBitString [] = 0
      parseBitString (True:xs) = 1+(2*(parseBitString xs))
      parseBitString (False:xs) = 2*(parseBitString xs)
  in parseBitString bits