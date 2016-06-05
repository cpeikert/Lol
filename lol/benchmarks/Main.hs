
import CycBenches
import TensorBenches
import UCycBenches
import ZqBenches

import Criterion.Main

main :: IO ()
main = defaultMain =<< (sequence [
  zqBenches,
  tensorBenches,
  ucycBenches,
  cycBenches
  ])
