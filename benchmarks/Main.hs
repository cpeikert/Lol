

import CycBenches
import SHEBenches

import SHETests
import CycTests

import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain =<< (sequence [
  cycBenches,
  sheBenches
  ])