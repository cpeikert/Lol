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

--module Crypto.Lol.Tests.Default (complexDoubleTests, zqTensorTests, defaultZqTests) where
module Crypto.Lol.Tests.Default (defaultZqTests, zqTensorTests) where

import Crypto.Lol (Complex, Int64)
import Crypto.Lol.Factored
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Types.IrreducibleChar2 ()

import Control.Monad.Random
import Data.Proxy
import qualified Test.Framework as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen (chooseAny)

-- | Default parameters for 'Crypto.Lol.Types.Unsafe.ZqBasic' tests.
defaultZqTests :: TF.Test
defaultZqTests = TF.testGroup "Zq Tests" $ [
  zqTests (Proxy::Proxy (Zq 3)),
  zqTests (Proxy::Proxy (Zq 7)),
  zqTests (Proxy::Proxy (Zq (3 ** 5))),
  zqTests (Proxy::Proxy (Zq (3 ** 5 ** 7)))]

-- TODO: Rename this to something more like testSingleIndexWithCRT. Continuing the convention,
--         tensorTests2 should be named testMultiIndexWithCRT. And the tests for Int64 should be the
--         same except "WithoutCRT"

unifTensorTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorTests1 pmr pt =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorTests1 genTensor

unifTensorCrtTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorCrtTests1 pmr pt =
  let genRing   = chooseAny :: QC.Gen r
      genTensor = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests1 genRing genTensor

zqTensorTests :: _ => Proxy t -> TF.Test
zqTensorTests pt =
  let uniIndexWithoutCRT = TF.testGroup "Tensor Tests over ZqBasic" $ ($ pt) <$> [
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
      uniIndexWithCRT = TF.testGroup "TensorCRT Tests over ZqBasic" $ ($ pt) <$> [
        unifTensorCrtTests1 (Proxy::Proxy '(F7,  Zq 29)),
        unifTensorCrtTests1 (Proxy::Proxy '(F12, SmoothZQ1)),
        unifTensorCrtTests1 (Proxy::Proxy '(F1,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F2,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F4,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F8,  Zq 17)),
        unifTensorCrtTests1 (Proxy::Proxy '(F21, Zq 8191)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, Zq 8191)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, ZQ1)),
        unifTensorCrtTests1 (Proxy::Proxy '(F89, Zq 179))] in
  TF.testGroup "All Tensor-like Tests over ZqBasic" [uniIndexWithoutCRT, uniIndexWithCRT]

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
