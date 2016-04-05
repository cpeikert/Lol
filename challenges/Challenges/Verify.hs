{-# LANGUAGE ConstraintKinds, FlexibleContexts, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

module Challenges.Verify where

import Challenges.Beacon
import Challenges.Common
import Challenges.ProtoReader

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)

import Crypto.Lol

import Data.ByteString.Lazy (toStrict,unpack)

import Net.Beacon

import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)

-- | Get a list of challenge names by getting all directory contents and filtering
-- on all directories whose first five characters are "chall".
getChallengeList :: FilePath -> IO [String]
getChallengeList challDir = do
  putStrLn $ "Reading challenges from \"" ++ challDir ++ "\""
  names <- filterM (doesDirectoryExist . (challDir </>)) =<< 
    filter (("chall" ==) . (take 5)) <$> getDirectoryContents challDir
  when (length names == 0) $ error "No challenges found."
  return names

-- | Parse the beacon time/offset used to reveal a challenge.
readRevealData :: (MonadIO m) => FilePath -> ExceptT String m BeaconPos
readRevealData path = do
  let revealPath = path </> revealFileName
  revealExists <- liftIO $ doesFileExist revealPath
  when (not revealExists) $ throwError $ revealPath ++ "does not exist."
  [timeStr, offsetStr] <- liftIO $ lines <$> readFile revealPath
  let time = read timeStr
      offset = read offsetStr
  -- validate the time and offset
  when ((time `mod` beaconInterval /= 0) || offset < 0 || offset >= bytesPerBeacon) $ 
    throwError "Invalid beacon position."
  return $ BP time offset

-- | Given a beacon record and a byte offset, return the secret index for this challenge.
getSecretIdx :: Record -> Int -> Int
getSecretIdx record byteOffset =
  let output = outputValue record
      byte = (unpack output) !! byteOffset
  in (fromIntegral byte) `mod` numInstances

-- | Verify that the (scaled, squared) norm of the noise for each sample in the instance is below @mhat*n*v@.
checkInstance :: forall v t m zp . (CheckSample v t m zp)
  => v -> Cyc t m (LiftOf zp) -> [LWESample t m zp] -> Bool
checkInstance v sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      eps = 1/(2^(40 :: Int))
      --d = (1/pi)*(1/2-(log eps)/(fromIntegral n))
      d = computeD n eps
      bound = (fromIntegral $ mhat*n)*v*d
  in all (checkSample bound sk) samples

type CheckSample v t m zp = 
  (CheckErr v t m zp, Ord v, Field v, Transcendental v, Show v)

-- | Verify that the (scaled, squared) norm of the noise for an LWE sample is below the provided bound.
checkSample :: forall v t m zp . (CheckSample v t m zp) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound
  
type CheckErr v t m zp = 
  (Fact m, Ring v, Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp))

-- | Given an instance and corresponding secret, outputs the (scaled, squared) norm of the error term.
sampleError :: forall v t m zp . (CheckErr v t m zp) 
  => Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm

computeD :: (Field v, Ord v, Transcendental v) => Int -> v -> v
computeD n eps = go (1 / (2*pi))
  where go d = 
          let d' = (1/2 + (log $ 2 * pi * d)/2 - (log eps)/(fromIntegral n))/pi
          in if ((d'-d) < 0.0001)
             then d'
             else go d'