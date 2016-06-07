{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude,
             ScopedTypeVariables, TypeOperators #-}

module ZqTests (zqTests) where

import Harness.Zq
import Tests
import Utils

import Crypto.Lol
import Crypto.Lol.CRTrans

zqTests = [
  testGroupM "(+)" $ applyBasic zqTypes $ hideArgs prop_add,
  testGroupM "(*)" $ applyBasic zqTypes $ hideArgs prop_mul,
  testGroupM "^-1" $ applyBasic zqTypes $ hideArgs prop_recip,
  testGroupM "extension ring (*)" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_mul_ext
  ]

prop_add :: forall r . (Ring r, Eq r) => LiftedMod r -> LiftedMod r -> Test r
prop_add (LMod x) (LMod y) = test $ (fromIntegral $ x + y) == ((fromIntegral x) + (fromIntegral y :: r))

prop_mul :: forall r . (Ring r, Eq r) => LiftedInvertible r -> LiftedInvertible r -> Test r
prop_mul (LInv x) (LInv y) = test $ (fromIntegral $ x * y) == ((fromIntegral x) * (fromIntegral y :: r))

prop_recip :: forall r . (Field r, Eq r) => Invertible r -> Test r
prop_recip (Invertible x) = test $ one == (x * recip x)

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: forall r . (CRTEmbed r, Ring r, Eq r)
  => Invertible r -> Invertible r -> Test r
prop_mul_ext (Invertible x) (Invertible y) = test $
  let z = x * y
      z' = fromExt $ toExt x * (toExt y)
  in z == z'

type ZqTypes = [
  Zq 3,
  Zq 7,
  Zq (3 ** 5),
  Zq (3 ** 5 ** 7)]

zqTypes :: Proxy ZqTypes
zqTypes = Proxy
