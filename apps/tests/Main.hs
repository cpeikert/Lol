{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

import SHETests
import KHPRFTests

import Crypto.Lol
import Crypto.Lol.Types

import Tests
import Test.Framework

type Zq q = ZqBasic q Int64
type Gad = BaseBGad 2

main :: IO ()
main = do
  prftest <- hideArgs "key homomorphism" (prop_keyHomom 10)
              (Proxy::Proxy '(RT, F32, Zq 2, Zq 64, Gad))
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
   [prftest, testGroup "SHE Tests" sheTests]