

import CycBenches
import Criterion.Main
import Control.Monad

main :: IO ()
main = defaultMain =<< (sequence [cycBenches])