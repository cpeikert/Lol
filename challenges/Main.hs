{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

import TGaussian
import LWE
import HPFloat

import Utils
import Data.Serialize
import Data.Either

import Control.Monad.Random

import Crypto.Lol hiding (encode)

main = do
  x :: [LWESample RT F32 (Zq 129)] <- 
    proxyT (lweSamples (1.0 :: Double) 1) (Proxy::Proxy (BigFloat (Prec 50)))
  --let x' = encode x
  print $ x == (read $ show x)
  print $ x == (head $ rights [decode $ encode x])
  print x