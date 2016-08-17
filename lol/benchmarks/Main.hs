
import Benchmarks hiding (benches)
import CycBenches
import TensorBenches
import UCycBenches
--import SimpleUCycBenches
--import SimpleTensorBenches
--import CTBenches

benches :: [String]
benches = [
  {-"unzipPow",
  "unzipDec",
  "unzipCRT",
  "zipWith (*)",
  "crt",
  "crtInv",
  "l",
  "lInv",
  "*g Pow",
  "*g Dec",
  "*g CRT",
  "divg Pow",
  "divg Dec",
  "divg CRT",
  "lift",
  "error",-}
  "twacePow",
  --"twaceDec",
  "twaceCRT",
  "embedPow",
  "embedDec",
  "embedCRT"

  ]

main :: IO ()
main = 
  let opts = defaultWidthOpts Progress benches
      reports = [tensorBenches,
                 ucycBenches,
                 cycBenches
                ]
  in prettyBenches opts reports

