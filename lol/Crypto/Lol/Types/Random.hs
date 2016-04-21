{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}

-- | Defines a newtype wrapper for crypto-api's 'CryptoRandomGen', and a corresponding 'RandomGen' instance.
--   'CryptoRandomGen' generators can only be used to get 'ByteString's; the 'RandomGen' instance allows them
--   to be used to generate any 'Random' type. Typical usage is with Control.Monad.Random's 'RandT' monad.

module Crypto.Lol.Types.Random (CryptoRand, evalCryptoRandIO) where

import Control.Monad.Random      (Rand, evalRand)
import "crypto-api" Crypto.Random
import Data.Binary.Get
import Data.ByteString hiding (pack)
import Data.ByteString.Lazy (pack)
import Data.Proxy
import System.Random

-- This function keeps all of the code in the IO monad, which helps write clean code below
-- No sequencing occurs between separate calls to this function.
evalCryptoRandIO :: (CryptoRandomGen gen) => Proxy gen -> Rand (CryptoRand gen) a -> IO a
evalCryptoRandIO _ x = do
  gen <- newGenIO -- uses system entropy
  return $ evalRand x gen

newtype CryptoRand g = CryptoRand g deriving (CryptoRandomGen)

intBytes = 
  let bits = intLog 2 (1 + fromIntegral (maxBound :: Int) -
                             fromIntegral (minBound :: Int) :: Integer)
  in if (bits `mod` 8) == 0 then bits `div` 8 
     else error "invalid Int bits in `intBytes`"

bytesToInt :: ByteString -> Int
bytesToInt bs = case intBytes of
  4 -> fromIntegral $ runGet getWord32host $ pack $ unpack bs
  8 -> fromIntegral $ runGet getWord64host $ pack $ unpack bs
  _ -> error "Unsupported Int size in `bytesToInt`"

-- | Yield @log_b(n)@ when it is a non-negative integer (otherwise
-- error).
intLog :: (Integral i) => i -> i -> Int
intLog _ 1 = 0
-- correct because ceil (lg (x)) == ceil (log (ceil (x)))
intLog b n | (n `mod` b) == 0 = 1 + intLog b (n `div` b)
           | otherwise = error "invalid arguments to intLog"

instance (CryptoRandomGen g) => RandomGen (CryptoRand g) where
  next (CryptoRand g) = case genBytes intBytes g of
    Right (bs, g') -> (bytesToInt bs, CryptoRand g')
    Left err -> error $ "Next error: " ++ show err

  genRange _ = (minBound, maxBound)

  split (CryptoRand g) = case splitGen g of
    Right (g1, g2) -> (CryptoRand g1, CryptoRand g2)
    Left err -> error $ "Split error: " ++ show err
