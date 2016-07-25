{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module SimpleTensorBenches (simpleTensorBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Criterion
import BenchParams

simpleTensorBenches :: IO Benchmark
simpleTensorBenches = do
  x1 :: T M (R, R) <- getRandom
  x2 :: T M R <- getRandom
  x3 :: T M R <- getRandom
  x4 :: T M' R <- getRandom
  gen <- newGenIO
  return $ bgroup "STensor" [
    bench "unzipPow"    $ nf unzipT x1,
    bench "unzipDec"    $ nf unzipT x1,
    bench "unzipCRT"    $ nf unzipT x1,
    bench "zipWith (*)" $ nf (zipWithT (*) x2) x3,
    bench "crt"         $ nf (fromJust' "SimpleTensorBenches.crt" crt) x2,
    bench "crtInv"      $ nf (fromJust' "SimpleTensorBenches.crtInv" crtInv) x2,
    bench "l"           $ nf l x2,
    bench "lInv"        $ nf lInv x2,
    bench "*g Pow"      $ nf mulGPow x2,
    bench "*g CRT"      $ nf (fromJust' "SimpleTensorBenches.gcrt" mulGCRT) x2,
    bench "lift"        $ nf (fmapT lift) x2,
    bench "error"       $ nf (evalRand (fmapT (roundMult one) <$>
                           (tGaussianDec (0.1 :: Double) :: Rand (CryptoRand HashDRBG) (T M Double))) :: CryptoRand HashDRBG -> T M Int64) gen,
    bench "twacePow"    $ nf (twacePowDec :: T M R -> T M' R) x2,
    bench "twaceCRT"    $ nf (fromJust' "SimpleTensorBenches.twaceCRT" twaceCRT :: T M R -> T M' R) x2,
    bench "embedPow"    $ nf (embedPow :: T M' R -> T M R) x4,
    bench "embedDec"    $ nf (embedDec :: T M' R -> T M R) x4
    ]