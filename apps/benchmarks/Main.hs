
import SHEBenches

import Criterion.Main

main :: IO ()
main = defaultMain =<< sequence [
  sheBenches
  ]
