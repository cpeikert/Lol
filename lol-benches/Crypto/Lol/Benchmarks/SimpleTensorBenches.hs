{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.SimpleTensorBenches (simpleTensorBenches1, simpleTensorBenches2) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol.Prelude
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types
import Crypto.Random.DRBG

import Criterion

{-# INLINE simpleTensorBenches1 #-}
simpleTensorBenches1 :: _ => _ -> _ -> Benchmark
simpleTensorBenches1 (Proxy :: Proxy '(t,m,r)) (Proxy::Proxy (gen :: *)) = do
  x1 :: t m (r, r) <- getRandom
  x2 :: t m r <- getRandom
  x3 :: t m r <- getRandom
  let x2' = mulGPow x2
      x2'' = mulGDec x2
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
    bench "*g Dec"      $ nf mulGDec x2,
    bench "*g CRT"      $ nf (fromJust' "SimpleTensorBenches.*gcrt" mulGCRT) x2,
    bench "divg Pow"    $ nf divGPow x2',
    bench "divg Dec"    $ nf divGDec x2'',
    bench "divg CRT"    $ nf (fromJust' "SimpleTensorBenches./gcrt" divGCRT) x2,
    bench "lift"        $ nf (fmapT lift) x2,
    bench "error"       $ nf (evalRand (fmapT (roundMult one) <$>
                           (tGaussianDec
                             (0.1 :: Double) :: Rand (CryptoRand gen) (t m Double)))
                               :: CryptoRand gen -> t m Int64) gen
    ]
{-# INLINE simpleTensorBenches2 #-}
simpleTensorBenches2 :: _ => _ -> Benchmark
simpleTensorBenches2 (Proxy :: Proxy '(t,m',m,r)) = do
  x2 :: t m r <- getRandom
  x4 :: t m' r <- getRandom
  return $ bgroup "STensor" [
    bench "twacePow" $ nf (twacePowDec :: t m r -> t m' r) x2,
    bench "twaceDec" $ nf (twacePowDec :: t m r -> t m' r) x2,
    bench "twaceCRT" $ nf (fromJust' "SimpleTensorBenches.twaceCRT" twaceCRT :: t m r -> t m' r) x2,
    bench "embedPow" $ nf (embedPow :: t m' r -> t m r) x4,
    bench "embedDec" $ nf (embedDec :: t m' r -> t m r) x4,
    bench "embedCRT" $ nf (fromJust' "SimpleTensorBenches.embedCRT" embedCRT :: t m' r -> t m r) x4
    ]