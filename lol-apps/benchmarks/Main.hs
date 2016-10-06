
import Benchmarks hiding (benches)
import SHEBenches

benches :: [String]
benches = [
  "encrypt",
  "decrypt",
  "*",
  "addPublic",
  "mulPublic",
  "rescaleCT",
  "keySwitch",
  "tunnel"
    ]

main :: IO ()
main = 
  let opts = defaultWidthOpts Progress benches
      reports = [sheBenches]
  in prettyBenches opts reports

