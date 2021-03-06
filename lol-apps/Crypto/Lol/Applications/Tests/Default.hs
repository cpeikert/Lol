{-|
Module      : Crypto.Lol.Applications.Tests.Default
Description : Default tests for lol-apps.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Mostly-monomorphized tests for lol-apps.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Tests.Default (defaultAppsTests) where
import Crypto.Lol.Applications.Tests.BGVTests
import Crypto.Lol.Factored
import Crypto.Lol.Tests

import Data.Proxy

defaultAppsTests ::
  ( forall m r. (Fact m, Show r, Eq r) => Show (t m r)
  , forall m r. (Fact m, Show r, Eq r) => Eq (t m r)
  , _
  ) => Proxy t -> Proxy gad -> [Test]
defaultAppsTests pt pgad  =
  [testGroup "BGV" $ ($ pt) <$> [
    bgvTests (Proxy::Proxy '(F7, F7, Zq 2,Zq (19393921 ** 18869761))),
    bgvTests (Proxy::Proxy '(F7, F21,Zq 2,Zq (19393921 ** 18869761))),
    bgvTests (Proxy::Proxy '(F2, F8, Zq 2,Zq 536871001)),
    bgvTests (Proxy::Proxy '(F1, F8, Zq 2,Zq 536871001)),
    bgvTests (Proxy::Proxy '(F4, F12,Zq 2,Zq 2148249601)),
    bgvTests (Proxy::Proxy '(F4, F8, Zq 3,Zq 2148249601)),
    bgvTests (Proxy::Proxy '(F7, F7, Zq 4,Zq (19393921 ** 18869761))),
    bgvTests (Proxy::Proxy '(F7, F21,Zq 4,Zq (19393921 ** 18869761))),
    bgvTests (Proxy::Proxy '(F1, F4, Zq 4,Zq 18869761)),
    bgvTests (Proxy::Proxy '(F4, F4, Zq 4,Zq 18869761)),
    bgvTests (Proxy::Proxy '(F14,F14,Zq 4,Zq 18869761)),
    bgvTests (Proxy::Proxy '(F28,F28,Zq 4,Zq 18869761)),
    bgvTests (Proxy::Proxy '(F28,F28,Zq 4,Zq 80221)),
    bgvTests (Proxy::Proxy '(F1, F8, Zq 4,Zq 536871001)),
    bgvTests (Proxy::Proxy '(F2, F8, Zq 4,Zq 536871001)),
    bgvTests (Proxy::Proxy '(F4, F12,Zq 8,Zq 2148249601)),

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

    ksTests (Proxy::Proxy '(F1, F7,  Zq 2, Zq (19393921 ** 18869761))) pgad,
    ksTests (Proxy::Proxy '(F2, F4,  Zq 8, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F4, F12, Zq 2, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F8, F64, Zq 2, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F3, F27, Zq 2, Zq (2148854401 ** 2148249601))) pgad,
    ksTests (Proxy::Proxy '(F2, F4,  Zq 8, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F4, F12, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F8, F64, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,
    ksTests (Proxy::Proxy '(F3, F27, Zq 2, Zq (2148854401 ** 2148249601 ** 2150668801))) pgad,

    twemTests (Proxy::Proxy '(F1, F7, F3, F21, Zq 2, Zq 18869761)),

    tunnelTests (Proxy::Proxy '(F8,F40,F20,F60,Zq 4,Zq (18869761 ** 19393921))) pgad]]
