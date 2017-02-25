{-|
Module      : Crypto.Lol.Applications.Benchmarks.Standard
Description : Standard benchmarks for lol-apps.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Mostly-monomorphized benchmarks for lol-apps.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Benchmarks.Standard (sheBenches, khprfBenches) where

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.Benchmarks.KHPRFBenches
import qualified Crypto.Lol.Applications.Benchmarks.SHEBenches as SHE
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Benchmarks
import Crypto.Lol.Types

infixr 9 **
data a ** b

type family Zq (a :: k) :: * where
  Zq (a ** b) = (Zq a, Zq b)
  Zq q = (ZqBasic q Int64)

sheBenches :: _ => Proxy t -> Proxy gad -> Proxy gen -> [rnd Benchmark]
sheBenches pt pgad pgen  = [
  benchGroup "SHE" $ ($ pt) <$>
    [SHE.sheBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)) pgen,
     SHE.sheBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857)) pgen],
  benchGroup "Dec" $ ($ pt) <$>
    [SHE.decBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)),
     SHE.decBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857))],
  benchGroup "Rescale" $ ($ pt) <$>
    [SHE.rescaleBenches (Proxy::Proxy '(F32, F2048,      Zq 16, Zq 1017857, Zq (1017857 ** 1032193))) pgad,
     SHE.rescaleBenches (Proxy::Proxy '(F32, F64*F9*F25, Zq 16, Zq 1008001, Zq (1008001 ** 1065601))) pgad],
  benchGroup "Tunnel" $ ($ pt) <$>
    [SHE.tunnelBenches {- H0 -> H1 -} (Proxy::Proxy '(F128,
                                                  F128 * F7 * F13,
                                                  F64 * F7, F64 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     SHE.tunnelBenches {- H1 -> H2 -} (Proxy::Proxy '(F64 * F7,
                                                  F64 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     SHE.tunnelBenches {- H2 -> H3 -} (Proxy::Proxy '(F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     SHE.tunnelBenches {- H3 -> H4 -} (Proxy::Proxy '(F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad,
     SHE.tunnelBenches {- H4 -> H5 -} (Proxy::Proxy '(F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 *F13,
                                                  F9 * F5 * F7 * F13,
                                                  F9 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)) pgad]]

khprfBenches :: forall t gad rnd . (_) => Proxy t -> Proxy gad -> rnd Benchmark
khprfBenches pt _ = benchGroup "KHPRF Table"
  [benchGroup "left/KHPRF"     $ benches' leftSpineTree,
   benchGroup "balanced/KHPRF" $ benches' balancedTree,
   benchGroup "right/KHPRF"    $ benches' rightSpineTree]
  where
    benches' = khPRFBenches 5 pt (Proxy::Proxy F128) (Proxy::Proxy '(Zq 8, Zq 2, gad))

-- EAC: is there a simple way to parameterize the variance?
-- generates a secret key with scaled variance 1.0
instance (GenSKCtx t m' z Double) => Random (SK (Cyc t m' z)) where
  random = runRand $ genSK (1 :: Double)
  randomR = error "randomR not defined for SK"
