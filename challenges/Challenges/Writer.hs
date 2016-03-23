{-# LANGUAGE FlexibleContexts #-}

module Challenges.Writer where

import Challenges.LWE

import Crypto.Lol.Types.Proto

import Data.ByteString (writeFile)
import Data.ByteString.Lazy (toStrict)

import Prelude hiding (writeFile)

import System.Directory

writeSecrets :: (Protoable (ChallengeSecrets t m zp)) => FilePath -> FilePath -> ChallengeSecrets t m zp -> IO ()
writeSecrets secretDir name secrets = do
  createDirectoryIfMissing False secretDir
  let secretFile = secretDir ++ "/" ++ name
  putStrLn $ "Writing secrets to " ++ secretFile ++ "..."
  writeFile secretFile $ toStrict $ msgPut secrets