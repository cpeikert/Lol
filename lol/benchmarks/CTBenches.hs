{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module CTBenches (ctBenches) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Prelude
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Criterion
import Params.LolParams

ctBenches :: IO Benchmark
ctBenches = do
  x1 :: T M (R, R) <- getRandom
  x2 :: T M R <- getRandom
  x3 :: T M R <- getRandom
  gen <- newGenIO
  return $ bgroup "CT" [
    bench "unzipPow"    $ nf unzipT' x1,
    bench "unzipDec"    $ nf unzipT' x1,
    bench "unzipCRT"    $ nf unzipT' x1,
    bench "zipWith (*)" $ nf (zipWithT' (*) x2) x3,
    bench "crt"         $ nf (wrap $ fromJust' "CTBenches.crt" crt') x2,
    bench "crtInv"      $ nf (wrap $ fromJust' "CTBenches.crtInv" crtinv') x2,
    bench "l"           $ nf (wrap l') x2,
    bench "lInv"        $ nf (wrap lInv') x2,
    bench "*g Pow"      $ nf (wrap mulGPow'') x2,
    bench "*g CRT"      $ nf (wrap $ fromJust' "CTBenches.gcrt" mulGCRT'') x2,
    bench "lift"        $ nf (fmapT lift) x2,
    bench "error"       $ nf (evalRand (fmapT (roundMult one) <$>
                           (CT <$> cDispatchGaussian
                             (0.1 :: Double) :: Rand (CryptoRand HashDRBG) (T M Double))) :: CryptoRand HashDRBG -> (T M Int64)) gen

    ]
