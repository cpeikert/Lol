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

module Crypto.Lol.Tests.Default (
  cycTests, cpxTensorTests, dblTensorTests, defaultZqTests, int64TensorTests, zqTensorTests
) where

import Crypto.Lol (Cyc, Int64)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Factored
import Crypto.Lol.Tests.CycTests
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.Tests (choose, chooseAny, testGroup, Test, Gen)
import Crypto.Lol.Types.IrreducibleChar2 ()
import Crypto.Lol.Types.Unsafe.Complex (Complex)

import qualified Algebra.Ring    as Ring (fromInteger)
import Control.Monad.Random
import Control.Applicative (liftA2)
import Data.Proxy

type CpxDbl = Complex Double

-- Gens a complex number in [0,1) + [0,i)
cpxDblGen :: Gen CpxDbl
cpxDblGen = chooseAny

-- Gens an Int64 in [-100,100]
boundedInt64Gen :: Gen Int64
boundedInt64Gen = choose (-100, 100)

-- Gens a Tensor over Int64s in the range [-100,100]
int64TensorGen :: forall t m . _ => Gen (t m Int64)
int64TensorGen = choose (Ring.fromInteger (-100), Ring.fromInteger 100)

unifTensorTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> Test
unifTensorTests1 _ _ =
  let tensorGen = chooseAny :: Gen (t m r) in
  tensorTests1 tensorGen

unifTensorTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> Test
unifTensorTests2 _ _ =
  let tensorGen = chooseAny :: Gen (t m r) in
  tensorTests2 (Proxy::Proxy '(t,m,m',r)) tensorGen

unifTensorCrtTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> Test
unifTensorCrtTests2 _ _ =
  let tensorGen = chooseAny :: Gen (t m r) in
  tensorCrtTests2 (Proxy::Proxy '(t,m,m',r)) tensorGen

unifTensorCrtTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> Test
unifTensorCrtTests1 _ _ =
  let ringGen   = chooseAny :: Gen r
      tensorGen = chooseAny :: Gen (t m r) in
  tensorCrtTests1 ringGen tensorGen

boundedInt64TensorTests1 :: forall t m . (TensorPowDec t Int64, Fact m, _)
  => Proxy m -> Proxy t -> Test
boundedInt64TensorTests1 _ _ = tensorTests1 (int64TensorGen :: Gen (t m Int64))

boundedInt64TensorTests2 :: forall t m m' . _ => Proxy '(m,m') -> Proxy t -> Test
boundedInt64TensorTests2 _ _ = tensorTests2
                                 (Proxy::Proxy '(t,m,m',Int64))
                                 (int64TensorGen :: Gen (t m Int64))

unifCycTests1 :: forall t m r . (Random (Cyc t m r), _) => Proxy '(m,r) -> Proxy t -> Test
unifCycTests1 _ _ =
  let cycGen = chooseAny :: Gen (Cyc t m r) in
  cycTests1 cycGen

unifCycTests2 :: forall t m m' r . (Random (Cyc t m' r), _) => Proxy '(m,m',r) -> Proxy t -> Test
unifCycTests2 _ _ =
  let cycGen = chooseAny :: Gen (Cyc t m' r) in
  cycTests2 (Proxy::Proxy '(t,m,m',r)) cycGen

-- | Default parameters for 'Crypto.Lol.Types.Unsafe.ZqBasic' tests.
defaultZqTests :: Test
defaultZqTests = testGroup "Zq Tests" $ [
  zqTests (Proxy::Proxy (Zq 3)),
  zqTests (Proxy::Proxy (Zq 7)),
  zqTests (Proxy::Proxy (Zq (3 ** 5))),
  zqTests (Proxy::Proxy (Zq (3 ** 5 ** 7)))]

zqTensorTests :: _ => Proxy t -> Test
zqTensorTests pt =
  let uniIndexWithoutCrt = testGroup "Single-Index Tensor Tests over ZqBasic" $ ($ pt) <$> [
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
      uniIndexWithCrt = testGroup "Single-Index TensorCRT Tests over ZqBasic" $ ($ pt) <$> [
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
      multiIndexWithoutCrt = testGroup "Multi-Index Tensor Tests over ZqBasic" $ ($ pt) <$> [
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
      multiIndexWithCrt = testGroup "Multi-Index TensorCRT Tests over ZqBasic" $ ($ pt) <$> [
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
  testGroup "All Tensor-like Tests over ZqBasic" [
    uniIndexWithoutCrt, uniIndexWithCrt, multiIndexWithoutCrt, multiIndexWithCrt]

int64TensorTests :: _ => Proxy t -> Test
int64TensorTests pt =
  let uniIndexWithoutCrt = testGroup "Single-Index Tensor Tests over Int64" $ ($ pt) <$> [
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
      multiIndexWithoutCrt = testGroup "Multi-Index Tensor Tests over Int64" $ ($ pt) <$> [
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
  testGroup "All Tensor-like Tests over Int64" [uniIndexWithoutCrt, multiIndexWithoutCrt]

dblTensorTests :: _ => Proxy t -> Test
dblTensorTests pt =
  let uniIndexWithoutCrt = testGroup "Single-Index Tensor Tests over Double" $ ($ pt) <$> [
        unifTensorTests1 (Proxy::Proxy '(F7, Double)),
        unifTensorTests1 (Proxy::Proxy '(F12, Double)),
        unifTensorTests1 (Proxy::Proxy '(F1, Double)),
        unifTensorTests1 (Proxy::Proxy '(F2, Double)),
        unifTensorTests1 (Proxy::Proxy '(F4, Double)),
        unifTensorTests1 (Proxy::Proxy '(F8, Double)),
        unifTensorTests1 (Proxy::Proxy '(F21, Double)),
        unifTensorTests1 (Proxy::Proxy '(F42, Double)),
        unifTensorTests1 (Proxy::Proxy '(F42, Double)),
        unifTensorTests1 (Proxy::Proxy '(F89, Double))]
      uniIndexWithCrt = testGroup "Single-Index TensorCRT Tests over Double" $ ($ pt) <$> [
        unifTensorCrtTests1 (Proxy::Proxy '(F7, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F12, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F1, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F2, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F4, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F8, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F21, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F42, Double)),
        unifTensorCrtTests1 (Proxy::Proxy '(F89, Double))]
      multiIndexWithoutCrt = testGroup "Multi-Index Tensor Tests over Double" $ ($ pt) <$> [
        unifTensorTests2 (Proxy::Proxy '(F1, F7, Double)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, Double)),
        unifTensorTests2 (Proxy::Proxy '(F4, F12, Double)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8, Double)),
        unifTensorTests2 (Proxy::Proxy '(F8, F8, Double)),
        unifTensorTests2 (Proxy::Proxy '(F2, F8, Double)),
        unifTensorTests2 (Proxy::Proxy '(F4, F8, Double)),
        unifTensorTests2 (Proxy::Proxy '(F3, F21, Double)),
        unifTensorTests2 (Proxy::Proxy '(F7, F21, Double)),
        unifTensorTests2 (Proxy::Proxy '(F3, F42, Double))]
      multiIndexWithCrt = testGroup "Multi-Index TensorCRT Tests over Double" $ ($ pt) <$> [
        unifTensorCrtTests2 (Proxy::Proxy '(F1, F7, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F12, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F8, F8, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F2, F8, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F4, F8, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F21, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F7, F21, Double)),
        unifTensorCrtTests2 (Proxy::Proxy '(F3, F42, Double))] in
  testGroup "All Tensor-like Tests over Double" [uniIndexWithoutCrt, multiIndexWithoutCrt]

cpxTensorTests :: _ => Proxy t -> Test
cpxTensorTests pt =
  let uniIndexWithoutCrt = testGroup "Single-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
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
      uniIndexWithCrt = testGroup "Single-Index TensorCRT Tests over Complex Double" $ ($ pt) <$> [
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
      multiIndexWithoutCrt = testGroup "Multi-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
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
      multiIndexWithCrt = testGroup "Multi-Index TensorCRT Tests over Complex Double" $ ($ pt) <$> [
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
  testGroup "All Tensor-like Tests over Complex Double" [
    uniIndexWithoutCrt, uniIndexWithCrt, multiIndexWithoutCrt, multiIndexWithCrt]

cycTests :: _ => Proxy t -> Test
cycTests pt =
  let uniIndex = testGroup "Single-Index Cyc Tests over ZqBasic" $ ($ pt) <$> [
        unifCycTests1 (Proxy::Proxy '(F7,  Zq 29)),
        unifCycTests1 (Proxy::Proxy '(F7,  Zq 32)),
        unifCycTests1 (Proxy::Proxy '(F12, SmoothZQ1)),
        unifCycTests1 (Proxy::Proxy '(F1,  Zq 17)),
        unifCycTests1 (Proxy::Proxy '(F2,  Zq 17)),
        unifCycTests1 (Proxy::Proxy '(F4,  Zq 17)),
        unifCycTests1 (Proxy::Proxy '(F8,  Zq 17)),
        unifCycTests1 (Proxy::Proxy '(F21, Zq 8191)),
        unifCycTests1 (Proxy::Proxy '(F42, Zq 8191)),
        unifCycTests1 (Proxy::Proxy '(F42, ZQ1)),
        unifCycTests1 (Proxy::Proxy '(F42, Zq 1024)),
        unifCycTests1 (Proxy::Proxy '(F42, ZQ2)),
        unifCycTests1 (Proxy::Proxy '(F89, Zq 179))]
      multiIndex = testGroup "Multi-Index Cyc Tests over ZqBasic" $ ($ pt) <$> [
        unifCycTests2 (Proxy::Proxy '(H01, H1, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(H01, H1, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(H01, H1, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(H01, H1, Zq PP16)),
        unifCycTests2 (Proxy::Proxy '(H12, H2, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(H12, H2, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(H12, H2, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(H12, H2, Zq PP16)),
        unifCycTests2 (Proxy::Proxy '(H23, H3, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(H23, H3, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(H23, H3, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(H23, H3, Zq PP16)),
        unifCycTests2 (Proxy::Proxy '(H34, H4, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(H34, H4, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(H34, H4, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(H34, H4, Zq PP16)),
        unifCycTests2 (Proxy::Proxy '(H45, H5, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(H45, H5, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(H45, H5, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(H45, H5, Zq PP16)),
        unifCycTests2 (Proxy::Proxy '(F4, F28, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(F4, F28, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(F4, F28, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(F7, F7*F13, Zq PP2)),
        unifCycTests2 (Proxy::Proxy '(F7, F7*F13, Zq PP4)),
        unifCycTests2 (Proxy::Proxy '(F1, F7, Zq PP8)),
        unifCycTests2 (Proxy::Proxy '(F1, F7, Zq PP2))] in
      testGroup "All Cyc Tests over ZqBasic" [uniIndex, multiIndex]

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = Zq (19393921 ** 18869761)

type SmoothZQ1 = Zq 2148249601

type H01 = F64
type H1 = F64 * F7
type H12 = F32 * F7
type H2 = F32 * F7 * F13
type H23 = F8 * F7 * F13
type H3 = F8 * F5 * F7 * F13
type H34 = F4 * F5 * F7 * F13
type H4 = F4 * F3 * F5 * F7 * F13
type H45 = F3 * F5 * F7 * F13
type H5 = F9 * F5 * F7 * F13
