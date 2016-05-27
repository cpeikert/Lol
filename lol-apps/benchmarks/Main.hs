
import SHEBenches

import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain =<< (sequence [
  sheBenches
  ])
