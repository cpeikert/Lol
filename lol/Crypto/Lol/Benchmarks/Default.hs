{-|
Module      : Crypto.Lol.Benchmarks.Default
Description : Default high-level benchmarks for 'Crypto.Lol.Cyclotomic.Tensor' implementations.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Default high-level benchmarks for 'Crypto.Lol.Cyclotomic.Tensor' implementations.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Benchmarks.Default (defaultLolBenches, oneIdxBenches, twoIdxBenches) where

import Crypto.Lol
--import Crypto.Lol.Benchmarks.SimpleTensorBenches
import Crypto.Lol.Benchmarks.TensorBenches
--import Crypto.Lol.Benchmarks.SimpleUCycBenches
import Crypto.Lol.Benchmarks.UCycBenches
import Crypto.Lol.Benchmarks.CycBenches
import Crypto.Lol.Utils.Benchmarks
import Crypto.Lol.Utils.ShowType

-- | Benchmark parameters reported in the paper. We suggest running these benchmarks
-- to quickly compare performance on your system or with your
-- 'Crypto.Lol.Cyclotomic.Tensor' backend.
{-# INLINABLE defaultLolBenches #-}
defaultLolBenches :: _ => Proxy t -> Proxy h -> IO [Benchmark]
defaultLolBenches pt phash = sequence [
  benchGroup "Single Index" $ (($ phash) . ($ pt)) <$> [
    oneIdxBenches (Proxy::Proxy '(F1024,        Zq 12289)),
    oneIdxBenches (Proxy::Proxy '(F2048,        Zq 12289)),
    oneIdxBenches (Proxy::Proxy '(F64*F27,      Zq 3457)),
    oneIdxBenches (Proxy::Proxy '(F64*F81,      Zq 10369)),
    oneIdxBenches (Proxy::Proxy '(F64*F9*F25,   Zq 14401))],
  benchGroup "Twace-Embed" $ ($ pt) <$> [
    twoIdxBenches (Proxy::Proxy '(F8*F7*F13,  F32*F7*F13,   Zq 8737)),
    twoIdxBenches (Proxy::Proxy '(F8*F7*F13,  F8*F5*F7*F13, Zq 14561)),
    twoIdxBenches (Proxy::Proxy '(F128,       F128*F7*F13,  Zq 23297))]
    ]

-- | Collection of all single-index operations at all levels of the library.
{-# INLINABLE oneIdxBenches #-}
oneIdxBenches :: forall t m r gen . _ => Proxy '(m,r) -> Proxy t -> Proxy gen -> IO Benchmark
oneIdxBenches _ _ pgen =
  let ptmr = Proxy :: Proxy '(t,m,r)
  in benchGroup (showType ptmr) $ (($ pgen) . ($ ptmr)) <$> [
      --simpleTensorBenches1,
      tensorBenches1,
      --simpleUCycBenches1,
      ucycBenches1,
      cycBenches1
      ]

-- | Collection of all inter-ring operations at all levels of the library.
{-# INLINABLE twoIdxBenches #-}
twoIdxBenches :: forall t m m' r . _ => Proxy '(m,m',r) -> Proxy t -> IO Benchmark
twoIdxBenches _ _ =
  let ptmr = Proxy :: Proxy '(t,m,m',r)
  in benchGroup (showType ptmr) $ ($ ptmr) <$> [
      --simpleTensorBenches2,
      tensorBenches2,
      --simpleUCycBenches2,
      ucycBenches2,
      cycBenches2
      ]
