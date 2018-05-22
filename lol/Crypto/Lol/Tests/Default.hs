{-|
Module      : Crypto.Lol.Tests.Default
Description : High-level tensor tests.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

High-level test groups and parameters,
which can be used to verify a 'Crypto.Lol.Cyclotomic.Tensor' implementation.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.Default (defaultZqTests, int64TensorTests, zqTensorTests) where

import Crypto.Lol (Complex, Int64)
import Crypto.Lol.Factored
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.BuildGen
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Types.IrreducibleChar2 ()

import Control.Monad.Random
import Data.Proxy
import qualified Test.Framework  as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen (choose, chooseAny)

-- | Default parameters for 'Crypto.Lol.Types.Unsafe.ZqBasic' tests.
defaultZqTests :: TF.Test
defaultZqTests = TF.testGroup "Zq Tests" $ [
  zqTests (Proxy::Proxy (Zq 3)),
  zqTests (Proxy::Proxy (Zq 7)),
  zqTests (Proxy::Proxy (Zq (3 ** 5))),
  zqTests (Proxy::Proxy (Zq (3 ** 5 ** 7)))]

unifTensorTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorTests1 _ _ =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorTests1 genTensor

unifTensorTests2 :: forall t m m' r . (Random r, Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorTests2 _ _ =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorTests2 (Proxy::Proxy '(t,m,m',r)) genTensor

unifTensorCrtTests2 :: forall t m m' r . (Random r, Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorCrtTests2 _ _ =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests2 (Proxy::Proxy '(t,m,m',r)) genTensor

unifTensorCrtTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorCrtTests1 _ _ =
  let genRing   = chooseAny :: QC.Gen r
      genTensor = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests1 genRing genTensor

-- Calls tensorTests1 with an Int64 generator for values in [-100, 100]
boundedTensorTests1 :: forall t m . (Random (t m Int64), BuildGen Int64 (t m), _)
                    => Proxy m -> Proxy t -> TF.Test
boundedTensorTests1 _ _ =
  let genRange  = choose (-100, 100) :: QC.Gen Int64
      genTensor = buildGen genRange :: QC.Gen (t m Int64) in
  tensorTests1 genTensor

-- Calls tensorTests2 with an Int64 generator for values in [-100, 100]
boundedTensorTests2 :: forall t m m' . (Random (t m Int64), BuildGen Int64 (t m), _)
                    => Proxy '(m,m') -> Proxy t -> TF.Test
boundedTensorTests2 _ _ =
  let genRange  = choose (-100, 100) :: QC.Gen Int64
      genTensor = buildGen genRange :: QC.Gen (t m Int64) in
  tensorTests2 (Proxy::Proxy '(t,m,m',Int64)) genTensor

zqTensorTests :: _ => Proxy t -> TF.Test
zqTensorTests pt =
  let uniIndexWithoutCrt = TF.testGroup "Tensor Tests over ZqBasic" $ ($ pt) <$> [
        unifTensorTests1 (Proxy::Proxy '(F7,  Zq 29)),
        unifTensorTests1 (Proxy::Proxy '(F12, SmoothZQ1)),
        unifTensorTests1 (Proxy::Proxy '(F1,  Zq 17)),
        unifTensorTests1 (Proxy::Proxy '(F2,  Zq 17)),
        unifTensorTests1 (Proxy::Proxy '(F4,  Zq 17)),
        unifTensorTests1 (Proxy::Proxy '(F8,  Zq 17)),
        unifTensorTests1 (Proxy::Proxy '(F21, Zq 8191)),
        unifTensorTests1 (Proxy::Proxy '(F42, Zq 8191)),
        unifTensorTests1 (Proxy::Proxy '(F42, ZQ1)),
        unifTensorTests1 (Proxy::Proxy '(F89, Zq 179))]
      uniIndexWithCrt = TF.testGroup "TensorCRT Tests over ZqBasic" $ ($ pt) <$> [
        unifTensorCrtTests1 (Proxy::Proxy '(F7,  Zq 29)),
        unifTensorCrtTests1 (Proxy::Proxy '(F12, SmoothZQ1)),
        unifTensorCrtTests1 (Proxy::Proxy '(F1,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F2,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F4,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F8,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F21, Zq 8191)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, Zq 8191)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, ZQ1)),
        unifTensorCrtTests1 (Proxy::Proxy '(F89, Zq 179))]
      multiIndexWithoutCrt = TF.testGroup "Multi-Index Tensor Tests over ZqBasic" $ ($ pt) <$> [
        unifTensorTests2 (Proxy::Proxy '(F1, F7,  Zq 29)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, Zq 536871001)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, SmoothZQ1)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8,  Zq 17)),
        unifTensorTests2 (Proxy::Proxy '(F8, F8,  Zq 17)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8,  SmoothZQ1)),
        unifTensorTests2 (Proxy::Proxy '(F4, F8,  Zq 17)),
        unifTensorTests2 (Proxy::Proxy '(F3, F21, Zq 8191)),
        unifTensorTests2 (Proxy::Proxy '(F7, F21, Zq 8191)),
        unifTensorTests2 (Proxy::Proxy '(F3, F42, Zq 8191))]
      multiIndexWithCrt = TF.testGroup "Multi-Index TensorCRT Tests over ZqBasic" $ ($ pt) <$> [
        unifTensorCrtTests2 (Proxy::Proxy '(F1, F7,  Zq 29)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, Zq 536871001)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, SmoothZQ1)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8,  Zq 17)),
        unifTensorCrtTests2 (Proxy::Proxy '(F8, F8,  Zq 17)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8,  SmoothZQ1)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F8,  Zq 17)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F21, Zq 8191)),
        unifTensorCrtTests2 (Proxy::Proxy '(F7, F21, Zq 8191)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F42, Zq 8191))] in
  TF.testGroup "All Tensor-like Tests over ZqBasic" [
    uniIndexWithoutCrt, uniIndexWithCrt, multiIndexWithoutCrt, multiIndexWithCrt]

int64TensorTests :: _ => Proxy t -> TF.Test
int64TensorTests pt =
  let uniIndexWithoutCrt = TF.testGroup "Tensor Tests over Int64" $ ($ pt) <$> [
        boundedTensorTests1 (Proxy::Proxy F7),
        boundedTensorTests1 (Proxy::Proxy F12),
        boundedTensorTests1 (Proxy::Proxy F1),
        boundedTensorTests1 (Proxy::Proxy F2),
        boundedTensorTests1 (Proxy::Proxy F4),
        boundedTensorTests1 (Proxy::Proxy F8),
        boundedTensorTests1 (Proxy::Proxy F21),
        boundedTensorTests1 (Proxy::Proxy F42),
        boundedTensorTests1 (Proxy::Proxy F42),
        boundedTensorTests1 (Proxy::Proxy F89)]
      multiIndexWithoutCrt = TF.testGroup "Multi-Index Tensor Tests over Int64" $ ($ pt) <$> [
        boundedTensorTests2 (Proxy::Proxy '(F1, F7)),
        boundedTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedTensorTests2 (Proxy::Proxy '(F8, F8)),
        boundedTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedTensorTests2 (Proxy::Proxy '(F4, F8)),
        boundedTensorTests2 (Proxy::Proxy '(F3, F21)),
        boundedTensorTests2 (Proxy::Proxy '(F7, F21)),
        boundedTensorTests2 (Proxy::Proxy '(F3, F42))] in
  TF.testGroup "All Tensor-like Tests over Int64" [uniIndexWithoutCrt, multiIndexWithoutCrt]

{-
-- | Default @m@/@r@ test parameters, for an arbitrary 'Crypto.Lol.Cyclotomic.Tensor'.
zqTensorTests :: _ => Proxy t -> [Test]
zqTensorTests pt = [
  testGroup "Tensor Tests over ZqBasic" $ ($ pt) <$> [
    testSingleIndexWithCRT (Proxy::Proxy '(F7,  Zq 29)),
    testSingleIndexWithCRT (Proxy::Proxy '(F12, SmoothZQ1)),
    testSingleIndexWithCRT (Proxy::Proxy '(F1,  Zq 17)),
    testSingleIndexWithCRT (Proxy::Proxy '(F2,  Zq 17)),
    testSingleIndexWithCRT (Proxy::Proxy '(F4,  Zq 17)),
    testSingleIndexWithCRT (Proxy::Proxy '(F8,  Zq 17)),
    testSingleIndexWithCRT (Proxy::Proxy '(F21, Zq 8191)),
    testSingleIndexWithCRT (Proxy::Proxy '(F42, Zq 8191)),
    testSingleIndexWithCRT (Proxy::Proxy '(F42, ZQ1)),
    testSingleIndexWithCRT (Proxy::Proxy '(F89, Zq 179)),

    testMultiIndexWithCRT (Proxy::Proxy '(F1, F7,  Zq 29)),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F12, Zq 536871001)),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F12, SmoothZQ1)),
    testMultiIndexWithCRT (Proxy::Proxy '(F2, F8,  Zq 17)),
    testMultiIndexWithCRT (Proxy::Proxy '(F8, F8,  Zq 17)),
    testMultiIndexWithCRT (Proxy::Proxy '(F2, F8,  SmoothZQ1)),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F8,  Zq 17)),
    testMultiIndexWithCRT (Proxy::Proxy '(F3, F21, Zq 8191)),
    testMultiIndexWithCRT (Proxy::Proxy '(F7, F21, Zq 8191)),
    testMultiIndexWithCRT (Proxy::Proxy '(F3, F42, Zq 8191))]
  ]

int64Tests :: _ => Proxy t -> [Test]
int64Tests pt = [
  testGroup "Tensor Tests over Int64" $ ($ pt) <$> [
    testSingleIndexWithoutCRT (Proxy::Proxy '(F7,  Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F12, Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F1,  Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F2,  Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F4,  Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F8,  Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F21, Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F42, Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F42, Int64)),
    testSingleIndexWithoutCRT (Proxy::Proxy '(F89, Int64)),

    testMultiIndexWithoutCRT (Proxy::Proxy '(F1, F7,  Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F4, F12, Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F4, F12, Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F2, F8,  Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F8, F8,  Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F2, F8,  Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F4, F8,  Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F3, F21, Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F7, F21, Int64)),
    testMultiIndexWithoutCRT (Proxy::Proxy '(F3, F42, Int64))]
  ]

complexDoubleTests :: _ => Proxy t -> [Test]
complexDoubleTests pt = [
  testGroup "Tensor Tests over Complex Double" $ ($ pt) <$> [
    testSingleIndexWithCRT (Proxy::Proxy '(F7,  (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F12, (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F1,  (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F2,  (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F4,  (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F8,  (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F21, (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F42, (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F42, (Complex Double))),
    testSingleIndexWithCRT (Proxy::Proxy '(F89, (Complex Double))),

    testMultiIndexWithCRT (Proxy::Proxy '(F1, F7,  (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F12, (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F12, (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F2, F8,  (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F8, F8,  (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F2, F8,  (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F4, F8,  (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F3, F21, (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F7, F21, (Complex Double))),
    testMultiIndexWithCRT (Proxy::Proxy '(F3, F42, (Complex Double)))]
  ]
-}

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761

type SmoothZQ1 = Zq 2148249601
