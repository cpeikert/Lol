
import CycBenches
import ZqBenches

import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain =<< (sequence [
  zqBenches,
  cycBenches
  ])
