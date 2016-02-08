import CycBenches
import UCycBenches
import SHEBenches
import ZqBenches

import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain =<< (sequence [
  ucycBenches,
  cycBenches,
  sheBenches
  ])
