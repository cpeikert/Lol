
import SHETests

import Test.Framework

main :: IO ()
main = do
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"]
    [ testGroup "SHE Tests" sheTests
    ]
