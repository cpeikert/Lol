{-# LANGUAGE BangPatterns, DataKinds, NoImplicitPrelude, PackageImports, RebindableSyntax, ScopedTypeVariables, TemplateHaskell #-}

import TGaussian
import LWE
import HPFloat
import Random
import Utils
--import FiatShamir

import Data.ByteString as BS hiding (init, tail, map)
import Data.Serialize
import Crypto.Lol hiding (encode, errorRounded)
import System.IO as IO
import Algebra.IntegralDomain (divUp)

import "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Control.Applicative
import Data.BooleanList (byteStringToBooleanList)
import Data.ByteString.Builder
import Numeric (readHex)
import Data.Char
import Net.Beacon
import Control.Monad.Random
import Crypto.Lol.GaussRandom

import qualified Data.Vector.Unboxed as V -- (toList)
import Control.Monad


type Dim = $(fType $ 128)
type FPrec = BigFloat (Prec 50)
type T = RT

fastSampleSum :: forall v m q . (ToRational v, OrdFloat q, Random q, MonadRandom m) => v -> Int -> m q
fastSampleSum v num = go num 0
  where go :: Int -> q -> m q
        go 0 !acc = return acc
        go x !acc = do
          (a,b) <- realGaussian v
          go (x-2) $ acc + a*a + b*b

main :: IO ()
main = do

  let num = 10000000
      v = 1 :: FPrec
  --samples <- V.map (^(2::Int)) <$> realGaussians v num
  --print $ "sample average: " ++ (show $ (fromIntegral num) / (V.sum samples :: FPrec))
  sum' :: FPrec <- fastSampleSum v num
  print $ (fromIntegral num) / sum'

  let n = proxy totientFact (Proxy::Proxy Dim)
      mhat = proxy valueHatFact (Proxy::Proxy Dim)
      bound = (fromIntegral $ mhat*n)*v
      numErrs = 10000

  realErrs :: [Cyc T Dim FPrec] <- replicateM numErrs $ tGaussian v
  rndErrs :: [Cyc T Dim Int64] <- replicateM numErrs $ proxyT (errorRounded v) (Proxy::Proxy FPrec)
  let realNorms = map gSqNorm realErrs
      rndNorms = map gSqNorm rndErrs

      realRatios = map ((bound) /) realNorms
      rndRatios = map (((bound) /) . fromIntegral) rndNorms

  print $ "Continuous noise to bound ratio: " ++ (show $ average realRatios)
  print $ "Rounded noise to bound ratio: " ++ (show $ average rndRatios)
