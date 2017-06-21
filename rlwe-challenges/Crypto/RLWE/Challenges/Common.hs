{-|
Module      : Crypto.RLWE.Challenges.Common
Description : Utility functions for handling exceptions and creating file paths.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Utility functions for handling exceptions and creating file paths.
-}

{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Crypto.RLWE.Challenges.Common where

import Crypto.RLWE.Challenges.Beacon

import           Crypto.Lol (Fact)
import           Crypto.Lol.Types     hiding (RRq)
import qualified Crypto.Lol.Types as RRq
import           Crypto.Lol.Types.Proto

import Crypto.Proto.RLWE.Challenges.Challenge
import Crypto.Proto.RLWE.Challenges.InstanceContProduct
import Crypto.Proto.RLWE.Challenges.InstanceDiscProduct
import Crypto.Proto.RLWE.Challenges.InstanceRLWRProduct
import Crypto.Proto.RLWE.Challenges.SecretProduct

import Crypto.Proto.Lol.KqProduct
import Crypto.Proto.Lol.RqProduct
import Crypto.Lol.Cyclotomic.Tensor

import Crypto.Random.DRBG

import Control.Monad.Except

import Data.ByteString.Lazy (unpack)
import Data.Constraint
import Data.Int
import Data.Maybe
import Data.Reflection hiding (D)
import Data.Functor.Trans.Tagged

import Net.Beacon

import System.Console.ANSI
import System.Directory    (doesDirectoryExist, doesFileExist,
                            getDirectoryContents)
import System.FilePath     ((</>))

import Text.Printf

type ChallengeID = Int32
type InstanceID = Int32
type InstDRBG = GenBuffered CtrDRBG

-- | Holds an (untyped) proto-buf Ring-LWE/LWR challenge.
data ChallengeU = CU !Challenge ![InstanceU] deriving (Show)

-- | Holds an (untyped) proto-buf Ring-LWE/LWR instance.
data InstanceU = IC {secret :: !SecretProduct, instc :: !InstanceContProduct}
               | ID {secret :: !SecretProduct, instd :: !InstanceDiscProduct}
               | IR {secret :: !SecretProduct, instr :: !InstanceRLWRProduct}
               deriving (Show)

-- | Concrete type used to generate and verify instances
type Zq q = ZqBasic q Int64
-- | Concrete type used to generate and verify instances
type RRq q = RRq.RRq q Double

-- | Contains the necessary entailments to allow generation and verification
-- using reified moduli and cyclotomic indices.
class (Tensor t, TElt t (Complex Double), TElt t Double, TElt t Int64)
  => EntailTensor t where
  entailTensor :: Tagged '(t,m,q) ((Reifies q Int64, Fact m) :-
    (ProtoType (t m (RRq q)) ~ KqProduct,
     ProtoType (t m (Zq q))  ~ RqProduct,
     Protoable (t m (Zq q)),
     Protoable (t m (RRq q)),
     TElt t (Zq q), TElt t (RRq q)))

-- | Yield a list of challenge names by getting all directory contents
-- and filtering on all directories whose names start with "chall".
challengeList :: (MonadIO m) => FilePath -> m [String]
challengeList path = liftIO $ do
  challDirExists <- doesDirectoryExist path
  unless challDirExists $ error $ "Could not find " ++ path
  -- putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (path </>)) =<<
    filter (("chall" ==) . take 5) <$> getDirectoryContents path
  when (null names) $ error "No challenges found."
  return names

-- | Do nothing if the file exists, otherwise throw an exception in the monad.
checkFileExists :: (MonadIO m, MonadError String m) => FilePath -> m ()
checkFileExists file = do
  fileExists <- liftIO $ doesFileExist file
  throwErrorUnless fileExists $
    "Error reading " ++ file ++ ": file does not exist."

-- | Parse the beacon time/offset used to reveal a challenge from a proto-buf stream.
parseBeaconAddr :: (MonadError String m) => Challenge -> m BeaconAddr
parseBeaconAddr Challenge{..} = do
  let ba = BA beaconEpoch beaconOffset
  -- validate the time and offset
  throwErrorUnless (validBeaconAddr ba) $ "Invalid beacon address: " ++ show ba
  return ba

-- | Yield the ID of the suppressed secret for a challenge, given a
-- beacon record and a byte offset.
suppressedSecretID :: InstanceID -> Record -> Int32 -> InstanceID
suppressedSecretID numInstances record byteOffset =
  let output = outputValue record
      byte = unpack output !! fromIntegral byteOffset
  in fromIntegral byte `mod` numInstances

-- * Directory Structure

-- | The root directory for challenges and their instances.
challengeFilesDir :: FilePath -> String -> FilePath
challengeFilesDir path name = path </> name

-- | The name for a challenge file is some string
-- with a .challenge extension.
challFilePath :: FilePath -> String -> FilePath
challFilePath path name = challengeFilesDir path name </> name ++ ".challenge"

-- | The name for an instance file is some string followed by a hex ID
-- with a .instance extension.
instFilePath :: FilePath -> String -> InstanceID -> FilePath
instFilePath path name instID = challengeFilesDir path name </> name ++ "-" ++
  instIDString instID ++ ".instance"

-- | The name for a secret file is some string followed by a hex ID
-- with the .secret extension.
secretFilePath :: FilePath -> String -> InstanceID -> FilePath
secretFilePath path name instID = challengeFilesDir path name </> name ++ "-" ++
  instIDString instID ++ ".secret"

-- | The name of a beacon XML file.
beaconFilePath :: FilePath -> BeaconEpoch -> FilePath
beaconFilePath path t = path </> "epoch-" ++ show t ++ ".xml"

-- | The filename for the NIST X509 certificate.
certFilePath :: FilePath -> FilePath
certFilePath path = path </> "beacon.cer"

-- | Hex representation of the instance ID.
instIDString :: InstanceID -> String
instIDString = printf "%02X"

-- * Functions for easy exceptions.

-- | Throw an error if the condition is 'True'.
throwErrorIf :: (MonadError String m) => Bool -> String -> m ()
throwErrorIf b = when b . throwError

-- | Throw an error if the condition is 'False'.
throwErrorUnless :: (MonadError String m) => Bool -> String -> m ()
throwErrorUnless b = unless b . throwError

-- | Throw an error if the input is 'Nothing'.
maybeThrowError :: (MonadError String m) => Maybe a -> String -> m a
maybeThrowError m str = do
  throwErrorIf (isNothing m) str
  return $ fromJust m

-- | Pretty printing of error messages.
printPassFailGeneric :: (MonadIO m)
                 => Color              -- ^ Color to print failure message.
                 -> String             -- ^ String to print if computation succeeds.
                 -> String             -- ^ String to print if computation fails.
                 -> ExceptT String m a -- ^ Computation to test.
                 -> m (Maybe a)
printPassFailGeneric failColor str pass e = do
  liftIO $ putStr str
  res <- runExceptT e
  case res of
    (Left st) -> do printANSI failColor st
                    return Nothing
    (Right a) -> do printANSI Green pass
                    return $ Just a

printPassFail, printPassWarn :: (MonadIO m)
  => String -> String -> ExceptT String m a -> m (Maybe a)
-- | Specialized version of 'printPassFailGeneric' that fails in red.
printPassFail = printPassFailGeneric Red
-- | Specialized version of 'printPassFailGeneric' that fails in yellow.
printPassWarn = printPassFailGeneric Yellow

-- | Print the input string in the specified color.
printANSI :: (MonadIO m) => Color -> String -> m ()
printANSI sgr str = liftIO $ do
  setSGR [SetColor Foreground Vivid sgr]
  putStrLn str
  setSGR [Reset]
