{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables #-}

import TGaussian
import LWE
import HPFloat

import Utils
import Data.Either

import Control.Applicative
import Data.Serialize
import Control.Monad.Random
import Data.Constraint

import Crypto.Lol hiding (encode)
import Crypto.Lol.Cyclotomic.UCyc

import Data.Vector.Unboxed as U (fromList)
import Data.Array.Repa as R (fromUnboxed, Array, DIM1, U)
import Data.Array.Repa.Index (Z(..),(:.)(..))

main = do
  x :: [LWESample RT F5 (Zq 11)] <- proxyT (lweSamples (1.0 :: Double) 1) (Proxy::Proxy (BigFloat (Prec 50)))
  let adviseLWEPow (x,y) = (advisePow x, advisePow y)
      z = map adviseLWEPow x




      cd = 0 :: Complex Double
      cds = [0,1,2,3] :: [Complex Double]
      cdv = U.fromList cds
      cdr = R.fromUnboxed (Z:.(length cds)) cdv :: Array U DIM1 (Complex Double)
      
  zqrt :: CT F5 (Zq 11) <- getRandom
  cdrt :: CT F5 (Complex Double) <- getRandom
  --let cdrt2 :: RT F5 (Complex Double) = read $ "Arr (AUnboxed (Z :. 4) [Complex (-12.94427190999916 +: -1.175570504584945),Complex (4.94427190999916 +: 1.9021130325903055),Complex (4.944271909999157 +: -1.90211303259031),Complex (-12.94427190999916 +: 1.175570504584952)])"

  (Left uczq) :: Either (UCyc CT F5 P (Zq 10)) (UCyc CT F5 C (Zq 10)) <- getRandom
  let uccd = toCRT uczq

  print $ "Complex Double: " ++ show (cd == (read $ show cd))
  print $ "[Complex Double]: " ++ show (cds == (read $ show cds))
  print $ "Vector (Complex Double): " ++ show (cdv == (read $ show cdv))
  print $ "Array DIM1 (Complex Double): " ++ show (cdr == (read $ show cdr))
  print $ "RT F5 (Zq 11): " ++ show (zqrt == (read $ show zqrt))
  print $ "RT F5 (Complex Double): " ++ show (cdrt == (read $ show cdrt))
  --print $ "RT F5 (Complex Double): " ++ show (cdrt2 == (read $ show cdrt2))
  print $ show uczq
  print $ "UCyc F5 (Zq 10): " ++ show (uczq == (read $ show uczq))
  --print $ show cdrt
  print $ show uccd
  print $ "UCyc F5 (Complex Double): " ++ show (uccd == (read $ show uccd))






  print ""
  print ""
  c' :: Either (UCyc RT F2 P (Zq 4)) (UCyc RT F2 C (Zq 4)) <- getRandom
  --let c = (\(Left x) -> x) c'
  let c = (\(Left x) -> toCRT x) c'
  --c :: RT F2 (Complex Double) <- getRandom
  print $ show c
  print $ c == (read $ show c)  
  print $ z == (read $ show z)
  print $ z == (head $ rights [decode $ encode z])
  print $ x == (head $ rights [decode $ encode x])
  print "X"
  print $ show x
  print "Show . Read. Show"
  print $ show $ ((read $ show x) `asTypeOf` x)
  print "(read.show == id)"
  print $ x == (read $ show x)

  
-- *** Exception: Arr (AUnboxed (Z :. 1) [Complex (2.0 +: 0.0)])
-- *** Exception: Arr (AUnboxed (Z :. 1) [ZqB 3])