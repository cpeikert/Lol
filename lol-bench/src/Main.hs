
import CycBenches
import ZqBenches

import Criterion.Main

main :: IO ()
main = defaultMain =<< (sequence [
  zqBenches,
  cycBenches
  ])
