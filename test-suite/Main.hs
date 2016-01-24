
import SHETests
import TensorTests
import CycTests
import ZqTests

import Test.Framework

main :: IO ()
main = do
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
    [  testGroup "Tensor Tests" tensorTests
     , testGroup "Cyc Tests" cycTests
     , testGroup "SHE Tests" sheTests
     , testGroup "Zq Tests" zqTests
    ]
