
import TensorTests
import CycTests
import ZqTests

import Test.Framework

main :: IO ()
main =
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
    [ testGroup "Tensor Tests" tensorTests
     ,testGroup "Cyc Tests" cycTests
     ,testGroup "Zq Tests" zqTests
    ]
