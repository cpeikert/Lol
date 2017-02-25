
module BenchAppsCPPMain where

main :: IO ()
main = do
  let o = (defaultOpts Nothing){benches=[]}
      pct = Proxy::Proxy CT
  bs <- sequence $
          sheBenches' pct (Proxy::Proxy TrivGad) (Proxy::Proxy HashDRBG) ++
          [khprfBenches pct (Proxy::Proxy (BaseBGad 2))]
  mapM_ (prettyBenches o) bs