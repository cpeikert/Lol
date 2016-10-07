
import Crypto.Lol.Tests.Standard
import Crypto.Lol.Types
import Data.Proxy

import Test.Framework

main :: IO ()
main = flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
  $ zqTs : mainT (Proxy::Proxy CT) ++ mainT (Proxy::Proxy RT)

