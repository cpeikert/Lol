
import Crypto.Lol.Benchmarks.ZqBenches

import Criterion.Main

main :: IO ()
main = defaultMain =<< sequence [zqBenches]