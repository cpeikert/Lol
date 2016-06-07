{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Defines a newtype wrapper 'CryptoRand' for crypto-api's
-- 'CryptoRandomGen', and a corresponding 'RandomGen' wrapper
-- instance.  These are needed because 'CryptoRandomGen' generators
-- can only be used to get 'ByteString's; the 'RandomGen' wrapper
-- instance allows them to be used to generate any 'Random' type.

module Crypto.Lol.Types.Random (CryptoRand, evalCryptoRandIO) where

import Control.Arrow
import Control.Monad.CryptoRandom
import Control.Monad.IO.Class
import Control.Monad.Random       (RandT, evalRandT)
import Crypto.Random
import System.Random

-- | Turns a 'CryptoRandomGen' @g@ into a standard 'RandomGen'.
newtype CryptoRand g = CryptoRand g deriving (CryptoRandomGen)

-- | Evaluate a 'RandT' computation using a cryptographic generator
-- @g@, seeded by system entropy.  Note that the updated generator is
-- not returned.
evalCryptoRandIO :: (CryptoRandomGen g, MonadIO io) => RandT g io a -> io a
evalCryptoRandIO x = do
  gen <- liftIO newGenIO -- uses system entropy
  evalRandT x gen

-- "standard" RNG interface wrapper for a cryptographic RNG
instance (CryptoRandomGen g) => RandomGen (CryptoRand g) where
  -- use 'CRandom' instance for 'Int'
  next (CryptoRand g) = either (error . show) (second CryptoRand) $ crandom g

  split (CryptoRand g) =
    either (error . show) (CryptoRand *** CryptoRand) $ splitGen g
