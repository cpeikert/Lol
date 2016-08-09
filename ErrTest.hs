{-# LANGUAGE TupleSections, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Monad
import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Lol.Cyclotomic.UCyc hiding (errorRounded)

type Range = (Double, Double)

type Param = (Factored, Int64, Double)

type M = F243
type R = Int64

main :: IO ()
main = do
  res <- replicateM 16000 errTest
  print $ maximum res

errTest :: IO Int64
errTest = do
  let v = 1.0/162 :: Double
  e1 <- errorRounded v
  e1' <- errorRounded v
  e2 <- errorRounded v
  e2' <- errorRounded v
  e3 <- errorRounded v
  let e = uncycDec $ e1*e1' + e2*e2' + e3 :: UCyc CT M D R
  return $ maximum $ fmap abs e

-- max over 2^14 KEx, multiply by 4, then a fudge factor of