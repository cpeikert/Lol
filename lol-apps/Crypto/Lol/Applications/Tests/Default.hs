{-|
Module      : Crypto.Lol.Applications.Tests.Default
Description : Default tests for lol-apps.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Mostly-monomorphized tests for lol-apps.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Tests.Default (defaultAppsTests) where

import Control.Monad.Random

import Crypto.Lol (Cyc)
import Crypto.Lol.Applications.SymmSHE hiding (CT)
import Crypto.Lol.Applications.Tests.KHPRFTests
import Crypto.Lol.Applications.Tests.SHETests
import Crypto.Lol.Factored
import Crypto.Lol.Gadget
import Crypto.Lol.Tests hiding (Test)

import Data.Proxy

import Test.Framework

defaultAppsTests :: _ => Proxy t -> Proxy gad -> [Test]
defaultAppsTests pt pgad  =
  [testGroup "SHE" $ ($ pt) <$> [
    sheTests (Proxy::Proxy '(F7, F7, Zq 2,Zq (19393921 ** 18869761))),
    sheTests (Proxy::Proxy '(F7, F21,Zq 2,Zq (19393921 ** 18869761))),
    sheTests (Proxy::Proxy '(F2, F8, Zq 2,Zq 536871001)),
    sheTests (Proxy::Proxy '(F1, F8, Zq 2,Zq 536871001)),
    sheTests (Proxy::Proxy '(F4, F12,Zq 2,Zq 2148249601)),
    sheTests (Proxy::Proxy '(F4, F8, Zq 3,Zq 2148249601)),
    sheTests (Proxy::Proxy '(F7, F7, Zq 4,Zq (19393921 ** 18869761))),
    sheTests (Proxy::Proxy '(F7, F21,Zq 4,Zq (19393921 ** 18869761))),
    sheTests (Proxy::Proxy '(F1, F4, Zq 4,Zq 18869761)),
    sheTests (Proxy::Proxy '(F4, F4, Zq 4,Zq 18869761)),
    sheTests (Proxy::Proxy '(F14,F14,Zq 4,Zq 18869761)),
    sheTests (Proxy::Proxy '(F28,F28,Zq 4,Zq 18869761)),
    sheTests (Proxy::Proxy '(F28,F28,Zq 4,Zq 80221)),
    sheTests (Proxy::Proxy '(F1, F8, Zq 4,Zq 536871001)),
    sheTests (Proxy::Proxy '(F2, F8, Zq 4,Zq 536871001)),
    sheTests (Proxy::Proxy '(F4, F12,Zq 8,Zq 2148249601)),

    decTest (Proxy::Proxy '(F2, F8, Zq 2,Zq 536871001)),
    decTest (Proxy::Proxy '(F1, F8, Zq 2,Zq 536871001)),
    decTest (Proxy::Proxy '(F4, F12,Zq 2,Zq 2148249601)),
    decTest (Proxy::Proxy '(F4, F8, Zq 3,Zq 2148249601)),
    decTest (Proxy::Proxy '(F1, F4, Zq 4,Zq 18869761)),
    decTest (Proxy::Proxy '(F4, F4, Zq 4,Zq 18869761)),
    decTest (Proxy::Proxy '(F14,F14,Zq 4,Zq 18869761)),
    decTest (Proxy::Proxy '(F28,F28,Zq 4,Zq 18869761)),
    decTest (Proxy::Proxy '(F28,F28,Zq 4,Zq 80221)),
    decTest (Proxy::Proxy '(F1, F8, Zq 4,Zq 536871001)),
    decTest (Proxy::Proxy '(F2, F8, Zq 4,Zq 536871001)),
    decTest (Proxy::Proxy '(F4, F12,Zq 8,Zq 2148249601)),

    modSwPTTest (Proxy::Proxy '(F7,F21,Zq 4,Zq 8,Zq 18869761)),
    modSwPTTest (Proxy::Proxy '(F7,F42,Zq 2,Zq 4,Zq (18869761 ** 19393921))),

    ksTests (Proxy::Proxy '(F1, F7,  Zq 2, Zq 19393921,   Zq (19393921 ** 18869761))) pgad,
    ksTests (Proxy::Proxy '(F2, F4,  Zq 8, Zq 2148854401, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F4, F12, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F8, F64, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F3, F27, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F2, F4,  Zq 8, Zq 2148854401, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F4, F12, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F8, F64, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F3, F27, Zq 2, Zq 2148854401, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,

    twemTests (Proxy::Proxy '(F1, F7, F3, F21, Zq 2, Zq 18869761)),

    tunnelTests (Proxy::Proxy '(F8,F40,F20,F60,Zq 4,Zq (18869761 ** 19393921))) pgad],
  testGroup "KHPRF" $ ($ pt) <$> [
    khprfTests (Proxy::Proxy '(F32, Zq 2, Zq 8, BaseBGad 2)),
    khprfTests (Proxy::Proxy '(F32, Zq 2, Zq 8, TrivGad)),
    khprfTests (Proxy::Proxy '(F32, Zq 32, Zq 257, BaseBGad 2))]
  ]

-- EAC: is there a simple way to parameterize the variance?
-- generates a secret key with scaled variance 1.0
instance (GenSKCtx t m' z Double) => Random (SK (Cyc t m' z)) where
  random = runRand $ genSK (1 :: Double)
  randomR = error "randomR not defined for SK"