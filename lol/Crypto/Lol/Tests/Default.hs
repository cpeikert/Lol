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

module Crypto.Lol.Tests.Default (defaultZqTests, cpxTensorTests, int64TensorTests, zqTensorTests) where

import Crypto.Lol (Int64, Ring)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Types.IrreducibleChar2 ()
import Crypto.Lol.Types.Unsafe.Complex (Complex)

import qualified Algebra.Ring    as Ring (fromInteger)
import Control.Monad.Random
import Control.Applicative (liftA2)
import Data.Proxy
import qualified Test.Framework  as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen (choose, chooseAny)

type CpxDbl = Complex Double

-- Gens a complex number in [0,1) + [0,i)
cpxDblGen :: QC.Gen CpxDbl
cpxDblGen = chooseAny

-- Gens an Int64 in [-100,100]
boundedInt64Gen :: QC.Gen Int64
boundedInt64Gen = choose (-100, 100)

-- Gens a Tensor over Int64s in the range [-100,100]
int64TensorGen :: forall t m . _ => QC.Gen (t m Int64)
int64TensorGen = choose (Ring.fromInteger (-100), Ring.fromInteger 100)

unifTensorTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorTests1 _ _ =
  let tensorGen = chooseAny :: QC.Gen (t m r) in
  tensorTests1 tensorGen

unifTensorTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorTests2 _ _ =
  let tensorGen = chooseAny :: QC.Gen (t m r) in
  tensorTests2 (Proxy::Proxy '(t,m,m',r)) tensorGen

unifTensorCrtTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorCrtTests2 _ _ =
  let tensorGen = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests2 (Proxy::Proxy '(t,m,m',r)) tensorGen

unifTensorCrtTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorCrtTests1 _ _ =
  let ringGen   = chooseAny :: QC.Gen r
      tensorGen = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests1 ringGen tensorGen

boundedInt64TensorTests1 :: forall t m . (Tensor t Int64, Fact m, _) => Proxy m -> Proxy t -> TF.Test
boundedInt64TensorTests1 _ _ = tensorTests1 (int64TensorGen :: QC.Gen (t m Int64))

boundedInt64TensorTests2 :: forall t m m' . _ => Proxy '(m,m') -> Proxy t -> TF.Test
boundedInt64TensorTests2 _ _ = tensorTests2
                                 (Proxy::Proxy '(t,m,m',Int64))
                                 (int64TensorGen :: QC.Gen (t m Int64))

-- | Default parameters for 'Crypto.Lol.Types.Unsafe.ZqBasic' tests.
defaultZqTests :: TF.Test
defaultZqTests = TF.testGroup "Zq Tests" $ [
  zqTests (Proxy::Proxy (Zq 3)),
  zqTests (Proxy::Proxy (Zq 7)),
  zqTests (Proxy::Proxy (Zq (3 ** 5))),
  zqTests (Proxy::Proxy (Zq (3 ** 5 ** 7)))]

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
        boundedInt64TensorTests1 (Proxy::Proxy F7),
        boundedInt64TensorTests1 (Proxy::Proxy F12),
        boundedInt64TensorTests1 (Proxy::Proxy F1),
        boundedInt64TensorTests1 (Proxy::Proxy F2),
        boundedInt64TensorTests1 (Proxy::Proxy F4),
        boundedInt64TensorTests1 (Proxy::Proxy F8),
        boundedInt64TensorTests1 (Proxy::Proxy F21),
        boundedInt64TensorTests1 (Proxy::Proxy F42),
        boundedInt64TensorTests1 (Proxy::Proxy F42),
        boundedInt64TensorTests1 (Proxy::Proxy F89)]
      multiIndexWithoutCrt = TF.testGroup "Multi-Index Tensor Tests over Int64" $ ($ pt) <$> [
        boundedInt64TensorTests2 (Proxy::Proxy '(F1, F7)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F8, F8)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F4, F8)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F3, F21)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F7, F21)),
        boundedInt64TensorTests2 (Proxy::Proxy '(F3, F42))] in
  TF.testGroup "All Tensor-like Tests over Int64" [uniIndexWithoutCrt, multiIndexWithoutCrt]

cpxTensorTests :: _ => Proxy t -> TF.Test
cpxTensorTests pt =
  let uniIndexWithoutCrt = TF.testGroup "Tensor Tests over Complex Double" $ ($ pt) <$> [
        unifTensorTests1 (Proxy::Proxy '(F7, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F12, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F1, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F2, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F4, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F8, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F21, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F42, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F42, CpxDbl)),
        unifTensorTests1 (Proxy::Proxy '(F89, CpxDbl))]
      uniIndexWithCrt = TF.testGroup "TensorCRT Tests over Complex Double" $ ($ pt) <$> [
        unifTensorCrtTests1 (Proxy::Proxy '(F7, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F12, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F1, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F2, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F4, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F8, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F21, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, CpxDbl)),
        unifTensorCrtTests1 (Proxy::Proxy '(F89, CpxDbl))]
      multiIndexWithoutCrt = TF.testGroup "Multi-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
        unifTensorTests2 (Proxy::Proxy '(F1, F7, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F8, F8, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F4, F8, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F3, F21, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F7, F21, CpxDbl)),
        unifTensorTests2 (Proxy::Proxy '(F3, F42, CpxDbl))]
      multiIndexWithCrt = TF.testGroup "Multi-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
        unifTensorCrtTests2 (Proxy::Proxy '(F1, F7, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F8, F8, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F8, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F21, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F7, F21, CpxDbl)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F42, CpxDbl))] in
  TF.testGroup "All Tensor-like Tests over Complex Double" [
    uniIndexWithoutCrt, uniIndexWithCrt, multiIndexWithoutCrt, multiIndexWithCrt]

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761

type SmoothZQ1 = Zq 2148249601
