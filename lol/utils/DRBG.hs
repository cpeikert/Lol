{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}

module DRBG (evalCryptoRandIO) where

import Crypto.Lol.LatticePrelude (Proxy)
import Crypto.Lol.Types.Random
import "crypto-api" Crypto.Random
import Control.Monad.Random (Rand, evalRand)
import Data.Binary.Get
import Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import System.Random

-- This function keeps all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function.
evalCryptoRandIO :: (CryptoRandomGen gen) => Proxy gen -> Rand (CryptoRand gen) a -> IO a
evalCryptoRandIO _ x = do
  gen <- newGenIO -- uses system entropy
  return $ evalRand x gen
