{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}

module DRBG (CryptoRand, evalCryptoRandIO) where

import Crypto.Lol.LatticePrelude (intLog, Proxy)
import "crypto-api" Crypto.Random
import Control.Monad.Random (Rand, evalRand)
import Data.Binary.Get
import Data.ByteString
import Data.ByteString.Lazy (fromStrict)
import System.Random

newtype CryptoRand g = CryptoRand g deriving (CryptoRandomGen)

-- This function keeps all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function.
evalCryptoRandIO :: (CryptoRandomGen gen) => Proxy gen -> Rand (CryptoRand gen) a -> IO a
evalCryptoRandIO _ x = do
  gen <- newGenIO -- uses system entropy
  return $ evalRand x gen

intBytes = 
  let bits = intLog 2 $ (1 + (fromIntegral (maxBound :: Int)) - (fromIntegral (minBound :: Int)) :: Integer)
  in if (bits `mod` 8) == 0 then bits `div` 8 else error "invalid Int bits in `intBytes`"

bytesToInt :: ByteString -> Int
bytesToInt bs = case intBytes of
  4 -> fromIntegral $ runGet getWord32host $ fromStrict bs
  8 -> fromIntegral $ runGet getWord64host $ fromStrict bs
  _ -> error "Unsupported Int size in `bytesToInt`"

instance (CryptoRandomGen g) => RandomGen (CryptoRand g) where
  next (CryptoRand g) = case genBytes intBytes g of
    Right (bs, g') -> (bytesToInt bs, CryptoRand g')
    Left err -> error $ "Next error: " ++ show err

  genRange _ = (minBound, maxBound)

  split (CryptoRand g) = case splitGen g of
    Right (g1, g2) -> (CryptoRand g1, CryptoRand g2)
    Left err -> error $ "Split error: " ++ show err