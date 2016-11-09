{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module KHPRFBenches (khPRFBenches) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (fromList)
import Control.Monad.State hiding (state)

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Benchmarks
import Crypto.Lol.Types
import Crypto.Lol.Utils.GenArgs

import GHC.TypeLits

import MathObj.Matrix hiding (zipWith)

type Gad = BaseBGad 2
type Zq q = ZqBasic q Int64

ringParams :: Proxy '(CT, F128, Zq 8, Zq 2, Gad)
ringParams = Proxy

lweParams :: Proxy '(Zq 8, Zq 2, 10, Gad)
lweParams = Proxy

instance (NFData a) => NFData (T a) where
  rnf = rnf . rows

khPRFBenches :: (MonadRandom m) => Int -> m Benchmark
khPRFBenches n = benchGroup "KHPRF" [
  hideArgs "ring-startup-left" (benchRingPRF n leftSpineTree [0]) ringParams,
  hideArgs "ring-startup-right" (benchRingPRF n rightSpineTree [0]) ringParams,
  hideArgs "ring-startup-balanced" (benchRingPRF n balancedTree [0]) ringParams,
  hideArgs "ring-amortized-left" (benchRingPRF n leftSpineTree (grayCode n)) ringParams,
  hideArgs "ring-amortized-right" (benchRingPRF n rightSpineTree (grayCode n)) ringParams,
  hideArgs "ring-amortized-balanced" (benchRingPRF n balancedTree (grayCode n)) ringParams,
  hideArgs "lwe-startup-left" (benchLatticePRF n leftSpineTree [0]) lweParams,
  hideArgs "lwe-startup-right" (benchLatticePRF n rightSpineTree [0]) lweParams,
  hideArgs "lwe-startup-balanced" (benchLatticePRF n balancedTree [0]) lweParams,
  hideArgs "lwe-amortized-left" (benchLatticePRF n leftSpineTree (grayCode n)) lweParams,
  hideArgs "lwe-amortized-right" (benchLatticePRF n rightSpineTree (grayCode n)) lweParams,
  hideArgs "lwe-amortized-balanced" (benchLatticePRF n balancedTree (grayCode n)) lweParams
  ]

data As n gad rq = As (Matrix rq) (Matrix rq)

instance (Gadget gad rq, Random rq, MonadRandom rnd, KnownNat n)
  => Generatable rnd (As n gad rq) where
  genArg = do
    let gadLen = length $ untag (gadget :: Tagged gad [rq])
        n = fromInteger $ natVal (Proxy::Proxy n)
    a0 <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
    a1 <- fromList n (n*gadLen) <$> take (gadLen*n*n) <$> getRandoms
    return $ As a0 a1

benchRingPRF :: forall t m zq (zp :: *) (gad :: *) . (_)
  => Int -> (Int -> FullBinTree) -> [Int] -> As 1 gad (Cyc t m zq) -> Cyc t m zq -> Bench '(t,m,zq,zp,gad)
benchRingPRF size t xs (As a0 a1) s =
  let family = makeFamily a0 a1 (t size) :: PRFFamily gad (Cyc t m zq) (Cyc t m zp)
      st = prfState family Nothing -- initialize with input 0
  in bench (flip evalState st . mapM (ringPRFM s)) xs

data Mat m n zq = Mat (Matrix zq)

instance (Random zq, MonadRandom rnd, KnownNat m, KnownNat n) => Generatable rnd (Mat m n zq) where
  genArg = do
    let m = fromInteger $ natVal (Proxy::Proxy m)
        n = fromInteger $ natVal (Proxy::Proxy n)
    Mat <$> fromList m n <$> take (m*n) <$> getRandoms


benchLatticePRF :: forall (zp :: *) zq n (gad :: *) . (_)
  => Int -> (Int -> FullBinTree) -> [Int] -> As n gad zq -> Mat 1 n zq -> Bench '(zq,zp,n,gad)
benchLatticePRF size t xs (As a0 a1) (Mat s) =
  let family = makeFamily a0 a1 (t size) :: PRFFamily gad zq zp
      state = prfState family Nothing -- initialize with input 0
  in bench (flip evalState state . mapM (latticePRFM s)) xs
