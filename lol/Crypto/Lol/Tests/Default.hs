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

import Crypto.Lol (Int64)
import Crypto.Lol.Factored
import Crypto.Lol.Tests.TensorTests
import Crypto.Lol.Tests.ZqTests
import Crypto.Lol.Utils.BuildGen
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Types.IrreducibleChar2 ()
import Crypto.Lol.Types.Unsafe.Complex (Complex, cis)

import Control.Monad.Random
import Control.Applicative (liftA2)
import Data.Proxy
import qualified Test.Framework  as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen (choose, chooseAny)

genUnitDouble :: QC.Gen Double
genUnitDouble = choose (-1, 1)

genUnitCpx :: QC.Gen (Complex Double)
genUnitCpx = cis <$> (choose (0, 2*pi) :: QC.Gen Double)

genBoundedInt64 :: QC.Gen Int64
genBoundedInt64 = choose (-100, 100)

genCpxTensor :: forall t m . (Fact m, BuildGen (Complex Double) (t m), _) => QC.Gen (t m (Complex Double))
genCpxTensor = buildGen genUnitCpx

genInt64Tensor :: forall t m .(Fact m, _) => QC.Gen (t m Int64)
genInt64Tensor = buildGen genBoundedInt64

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

unifTensorTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorTests2 _ _ =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorTests2 (Proxy::Proxy '(t,m,m',r)) genTensor

unifTensorCrtTests2 :: forall t m m' r . (Random (t m r), _) => Proxy '(m,m',r) -> Proxy t -> TF.Test
unifTensorCrtTests2 _ _ =
  let genTensor = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests2 (Proxy::Proxy '(t,m,m',r)) genTensor

unifTensorCrtTests1 :: forall t m r . (Random r, Random (t m r), _) => Proxy '(m,r) -> Proxy t -> TF.Test
unifTensorCrtTests1 _ _ =
  let genRing   = chooseAny :: QC.Gen r
      genTensor = chooseAny :: QC.Gen (t m r) in
  tensorCrtTests1 genRing genTensor

-- Calls tensorTests1 with an Int64 generator for values in [-100, 100]
boundedInt64TensorTests1 :: forall t m . (Random (t m Int64), BuildGen Int64 (t m), _)
                    => Proxy m -> Proxy t -> TF.Test
boundedInt64TensorTests1 _ _ = tensorTests1 (genInt64Tensor :: QC.Gen (t m Int64))

-- Calls tensorTests2 with an Int64 generator for values in [-100, 100]
boundedInt64TensorTests2 :: forall t m m' . (Random (t m Int64), BuildGen Int64 (t m), _)
                    => Proxy '(m,m') -> Proxy t -> TF.Test
boundedInt64TensorTests2 _ _ = tensorTests2
                                 (Proxy::Proxy '(t,m,m',Int64))
                                 (genInt64Tensor :: QC.Gen (t m Int64))

-- Calls tensorTests1 with a Complex Double generator for values in the unit square [-1,1]×[-i,i]
boundedCpxTensorTests1 :: forall t m . (BuildGen (Complex Double) (t m), _)
                       => Proxy m -> Proxy t -> TF.Test
boundedCpxTensorTests1 _ _ = tensorTests1 (genCpxTensor :: QC.Gen (t m (Complex Double)))

-- Calls tensorCrtTests1 with a Complex Double generator for values in the unit square [-1,1]×[-i,i]
boundedCpxCrtTensorTests1 :: forall t m . (BuildGen (Complex Double) (t m), _)
                          => Proxy m -> Proxy t -> TF.Test
boundedCpxCrtTensorTests1 _ _ = tensorCrtTests1
                                  genUnitCpx
                                  (genCpxTensor :: QC.Gen (t m (Complex Double)))

-- Calls tensorTests2 with a Complex Double generator for values in the unit square [-1,1]×[-i,i]
boundedCpxTensorTests2 :: forall t m m' . (BuildGen (Complex Double) (t m), _)
                       => Proxy '(m,m') -> Proxy t -> TF.Test
boundedCpxTensorTests2 _ _ = tensorTests2
                               (Proxy::Proxy '(t,m,m',(Complex Double)))
                               (genCpxTensor :: QC.Gen (t m (Complex Double)))

-- Calls tensorCrtTests2 with a Complex Double generator for values in the unit square [-1,1]×[-i,i]
boundedCpxCrtTensorTests2 :: forall t m m' . (BuildGen (Complex Double) (t m), _)
                       => Proxy '(m,m') -> Proxy t -> TF.Test
boundedCpxCrtTensorTests2 _ _ = tensorCrtTests2
                                  (Proxy::Proxy '(t,m,m',(Complex Double)))
                                  (genCpxTensor :: QC.Gen (t m (Complex Double)))

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
        boundedCpxTensorTests1 (Proxy::Proxy F7),
        boundedCpxTensorTests1 (Proxy::Proxy F12),
        boundedCpxTensorTests1 (Proxy::Proxy F1),
        boundedCpxTensorTests1 (Proxy::Proxy F2),
        boundedCpxTensorTests1 (Proxy::Proxy F4),
        boundedCpxTensorTests1 (Proxy::Proxy F8),
        boundedCpxTensorTests1 (Proxy::Proxy F21),
        boundedCpxTensorTests1 (Proxy::Proxy F42),
        boundedCpxTensorTests1 (Proxy::Proxy F42),
        boundedCpxTensorTests1 (Proxy::Proxy F89)]
      uniIndexWithCrt = TF.testGroup "TensorCRT Tests over Complex Double" $ ($ pt) <$> [
        boundedCpxCrtTensorTests1 (Proxy::Proxy F7),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F12),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F1),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F2),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F4),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F8),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F21),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F42),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F42),
        boundedCpxCrtTensorTests1 (Proxy::Proxy F89)]
      multiIndexWithoutCrt = TF.testGroup "Multi-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
        boundedCpxTensorTests2 (Proxy::Proxy '(F1, F7)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F8, F8)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F4, F8)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F3, F21)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F7, F21)),
        boundedCpxTensorTests2 (Proxy::Proxy '(F3, F42))]
      multiIndexWithCrt = TF.testGroup "Multi-Index Tensor Tests over Complex Double" $ ($ pt) <$> [
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F1, F7)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F4, F12)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F8, F8)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F2, F8)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F4, F8)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F3, F21)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F7, F21)),
        boundedCpxCrtTensorTests2 (Proxy::Proxy '(F3, F42))] in
  TF.testGroup "All Tensor-like Tests over Complex Double" [
    uniIndexWithoutCrt, uniIndexWithCrt, multiIndexWithoutCrt, multiIndexWithCrt]

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761

type SmoothZQ1 = Zq 2148249601
