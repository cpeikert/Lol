
module TestRepaMain where

import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Tests.Standard
import Data.Proxy

main :: IO ()
main = defaultTestMain (Proxy::Proxy RT)