{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude, PackageImports, RebindableSyntax, ScopedTypeVariables #-}

import TGaussian
import LWE
import HPFloat
import Random
import Utils
--import FiatShamir

import Data.ByteString as BS hiding (init, tail, map, null)
import Data.ByteString.Lazy (toStrict, fromStrict, null)
import Data.Serialize
import Crypto.Lol hiding (encode, null)
import System.IO as IO
import Algebra.IntegralDomain (divUp)

import "crypto-api" Crypto.Random
import Crypto.Random.DRBG
import Control.Applicative
import Data.BooleanList (byteStringToBooleanList) -- boolean-list
import Data.ByteString.Builder
import Numeric (readHex)
import Data.Char
import Net.Beacon
import Control.Monad.Random

import Crypto.Lol.Types.Proto
import Text.ProtocolBuffers.Header (ReflectDescriptor, Wire)
import System.Directory (removeFile)

type F = Double
type T = RT
type M = F11

main :: IO ()
main = do
  -- Generate (numInstances * fsInstSize) LWE instances,
  -- corresponding to (numInstances * fsInstSize * numSamples) LWE samples.
  -- We reveal all but numInstances LWE keys.
  let --numInstances = 1   -- number of secret LWE instances to generate
      numSamples   = 1  -- number of LWE samples per instance
      --fsInstSize   = 2   -- number of LWE instances per FS instance (we reveal keys for all but one)
      v = 1 :: F

  -- check LWE instance
  (sk,inst) :: (Cyc T M Int64, LWEInstance F T M (Zq 23)) <- proxyT (lweInstance v numSamples) (Proxy::Proxy F)
  if checkInstance sk inst
  then print "Instance passed."
  else print "Instance failed."

  -- check instance serialization
  BS.writeFile "lwe.raw" (encode inst)
  bsInst <- decode' <$> BS.readFile "lwe.raw"
  if (bsInst == inst)
  then print "Serialization passed."
  else print "Serialization failed."
  removeFile "lwe.raw"

  -- check instance protocol buffer
  BS.writeFile "lwe.proto" $ toStrict $ msgPut inst
  prInst <- msgGet' <$> BS.readFile "lwe.proto"
  if (prInst == inst)
  then print "Protocol buffer passed."
  else print "Protocol buffer failed."
  removeFile "lwe.proto"

  -- check instance printing/parsing
  IO.writeFile "lwe.txt" (show inst)
  txInst <- read <$> IO.readFile "lwe.txt"
  if(txInst == inst)
  then print "Text parse passed."
  else print "Text parse failed."
  removeFile "lwe.txt"

  -- get Beacon
  time <- localDateToSeconds 2 24 2016 11 0
  seed <- getBeacon time
  print $ showHexBS seed

  -- get random bits using Beacon as seed to generator
  let (Right gen) = newGen seed
      bits = evalRand (genBits 8) (gen :: HashDRBG)
  print bits









msgGet' :: (ReflectDescriptor (ProtoType a), Wire (ProtoType a), Protoable a) => ByteString -> a
msgGet' bs = 
  case msgGet $ fromStrict bs of
    (Left str) -> error $ "when getting protocol buffer. Got string " ++ str
    (Right (a,bs')) -> 
      if null bs'
      then a
      else error $ "when getting protocol buffer. There were leftover bits!"


decode' :: (Serialize a) => ByteString -> a
decode' bs = 
  case decode bs of
    (Left str) -> error $ "when decoding bytestring. Got string " ++ str
    (Right x) -> x

showHexBS :: ByteString -> String
showHexBS = map toUpper . tail . init . show . toLazyByteString . byteStringHex

-- outputs the requested number of bits
-- wastes any unused bits in the last byte sampled
genBits :: (CryptoRandomGen gen) => Int -> Rand gen [Bool]
genBits n = liftRand $ \g ->
  let numBytes = n `divUp` 8
      mbytes = genBytes numBytes g
  in case mbytes of
      (Left err) -> error "An error occured when sampling bits."
      (Right (bs,g')) -> (byteStringToBooleanList bs, g')
