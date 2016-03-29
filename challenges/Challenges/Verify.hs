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

import Data.BooleanList (byteStringToBooleanList)
import Data.ByteString.Lazy (toStrict)

import Net.Beacon

import System.Directory (doesFileExist)

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

checkInstance :: forall v t m zp . (CheckSample v t m zp)
  => v -> Cyc t m (LiftOf zp) -> [LWESample t m zp] -> Bool
checkInstance v sk samples = 
  let n = proxy totientFact (Proxy::Proxy m)
      mhat = proxy valueHatFact (Proxy::Proxy m)
      bound = (fromIntegral $ mhat*n)*v
  in all (checkSample bound sk) samples

type CheckSample v t m zp = 
  (CheckErr v t m zp, Ord v, Field v)

checkSample :: (CheckSample v t m zp) 
  => v -> Cyc t m (LiftOf zp) -> LWESample t m zp -> Bool
checkSample bound sk pair@(LWESample a b) = (sampleError sk pair) < bound
type CheckErr v t m zp = 
  (Fact m, Ring v, Lift' zp, CElt t zp, CElt t (LiftOf zp), ToInteger (LiftOf zp))

sampleError :: forall v t m zp . (CheckErr v t m zp) 
  => Cyc t m (LiftOf zp) -> LWESample t m zp -> v
sampleError sk (LWESample a b) = 
  let e' = b-a*reduce sk
      e = liftCyc Dec e' :: Cyc t m (LiftOf zp)
      norm = gSqNorm e
  in fromIntegral norm