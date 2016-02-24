{-# LANGUAGE DataKinds, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

import TGaussian
import LWE
import HPFloat
import Random
import Utils
--import FiatShamir

import Data.ByteString as BS hiding (init)
import Data.Serialize
import Crypto.Lol hiding (encode)
import System.IO as IO


main :: IO ()
main = do
  -- Generate (numInstances * fsInstSize) LWE instances,
  -- corresponding to (numInstances * fsInstSize * numSamples) LWE samples.
  -- We reveal all but numInstances LWE keys.
  let --numInstances = 3   -- number of secret LWE instances to generate
      numSamples   = 10  -- number of LWE samples per instance
      --fsInstSize   = 2   -- number of LWE instances per FS instance (we reveal keys for all but one)
      v = 1
  inst :: LWEInstance Double RT F5 (Zq 101) <- proxyT (lweInstance v numSamples) (Proxy::Proxy (BigFloat (Prec 25)))
  if checkInstance inst
  then print "Instance passed!"
  else print "Instance FAILED."
  BS.writeFile "lwe.raw" (encode inst)
  IO.writeFile "lwe.txt" (show inst)


  let beaconTime = dateToSeconds 2 23 2016
      -- there has got to be a better way to get the seconds since the epoch as an Int...
      beaconTime' = read $ init $ show beaconTime :: Int
  val <- beaconValue beaconTime'
  print val
  print "done"
