
import Benchmarks hiding (benches)
import SHEBenches
import KHPRFBenches

benches :: [String]
benches = [
  "encrypt",
  "decrypt",
  "*",
  "addPublic",
  "mulPublic",
  "rescaleCT",
  "keySwitch",
  "tunnel",

  "ring-startup-left",
  "ring-startup-right",
  "ring-startup-balanced",
  "ring-amortized-left",
  "ring-amortized-right",
  "ring-amortized-balanced",
  "lwe-startup-left",
  "lwe-startup-right",
  "lwe-startup-balanced",
  "lwe-amortized-left",
  "lwe-amortized-right",
  "lwe-amortized-balanced"
    ]

main :: IO ()
main =
  let opts = defaultWidthOpts Progress ["SHE", "KHPRF"] benches
      reports = [sheBenches, khPRFBenches 5]
  in prettyBenches opts reports

