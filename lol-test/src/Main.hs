
import TensorTests
import CycTests
import ZqTests

import Test.Framework
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  opts <- interpretArgsOrExit args
  --
  defaultMainWithOpts
    [ testGroup "Tensor Tests" tensorTests
    , testGroup "Cyc Tests" cycTests
    , testGroup "Zq Tests" zqTests
    ]
    opts { ropt_threads = Just 1 }

