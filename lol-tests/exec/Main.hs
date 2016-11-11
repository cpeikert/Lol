
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Cyclotomic.Tensor.Repa
import Crypto.Lol.Tests.Standard
import Data.Proxy
import Test.Framework

main :: IO ()
main = flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
  $ zqTs : defaultTests (Proxy::Proxy CT) ++ defaultTests (Proxy::Proxy RT)
