{-|
Module      : Crypto.Lol.Applications.Benchmarks.Default
Description : Default benchmarks for lol-apps.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Mostly-monomorphized benchmarks for lol-apps.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Benchmarks.Default
 ( defaultSHEBenches
 , defaultKHPRFBenches
 ) where

import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.Benchmarks.SHEBenches
import Crypto.Lol.Applications.Benchmarks.KHPRFBenches
import Crypto.Lol.Benchmarks (bgroup, Benchmark, Zq, type (**))

defaultSHEBenches :: forall t gad gen rnd . (MonadRandom rnd, _)
                  => Proxy t -> Proxy gad -> Proxy gen -> rnd [Benchmark]
defaultSHEBenches pt pgad pgen  = sequence [
  fmap (bgroup "SHE") $ sequence $ (($ pt) . ($ pgen)) <$>
    [sheBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)),
     sheBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857))],
  fmap (bgroup "Dec") $ sequence $ ($ pt) <$>
    [decBenches (Proxy::Proxy '(F16, F1024, Zq 8,  Zq 1017857)),
     decBenches (Proxy::Proxy '(F16, F2048, Zq 16, Zq 1017857))],
  fmap (bgroup "Rescale") $ sequence $ ($ pt) <$>
    [rescaleBenches (Proxy::Proxy '(F32, F2048,      Zq 16, Zq 1017857, Zq (1032193 ** 1017857))),
     rescaleBenches (Proxy::Proxy '(F32, F64*F9*F25, Zq 16, Zq 1008001, Zq (1065601 ** 1008001)))],
  fmap (bgroup "KeySwitch") $ sequence $ (($ pt) . ($ pgad)) <$>
    [keySwitchBenches (Proxy::Proxy '(F32, F2048,      Zq 16, Zq (1017857 ** 1032193))),
     keySwitchBenches (Proxy::Proxy '(F32, F64*F9*F25, Zq 16, Zq (1008001 ** 1065601)))],
  fmap (bgroup "Tunnel") $ sequence $ (($ pt) . ($ pgad)) <$>
    [tunnelBenches {- H0 -> H1 -} (Proxy::Proxy '(F128,
                                                  F128 * F7 * F13,
                                                  F64 * F7, F64 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)),
     tunnelBenches {- H1 -> H2 -} (Proxy::Proxy '(F64 * F7,
                                                  F64 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)),
     tunnelBenches {- H2 -> H3 -} (Proxy::Proxy '(F32 * F7 * F13,
                                                  F32 * F7 * F13,
                                                  F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  Zq PP32,
                                                  Zq 3144961)),
     tunnelBenches {- H3 -> H4 -} (Proxy::Proxy '(F8 * F5 * F7 * F13,
                                                  F8 * F5 * F7 *F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961)),
     tunnelBenches {- H4 -> H5 -} (Proxy::Proxy '(F4 * F3 * F5 * F7 * F13,
                                                  F4 * F3 * F5 * F7 *F13,
                                                  F9 * F5 * F7 * F13,
                                                  F9 * F5 * F7 * F13,
                                                  Zq PP32,
                                                  Zq 3144961))]]


defaultKHPRFBenches :: _ => Proxy t -> Proxy gad -> rnd [Benchmark]
defaultKHPRFBenches = khprfBenches
