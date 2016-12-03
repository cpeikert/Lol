
module TestCPPMain where

import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Tests.Standard
import Data.Proxy

main :: IO ()
main = defaultTestMain (Proxy::Proxy CT)