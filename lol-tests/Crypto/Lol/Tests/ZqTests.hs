{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.ZqTests (zqTests) where

import Crypto.Lol.Utils.GenArgs.Zq
import Crypto.Lol.Tests
import Crypto.Lol.Utils.ShowType

import Control.Applicative

import Crypto.Lol
import Crypto.Lol.CRTrans

import qualified Test.Framework as TF

zqTests :: _ => Proxy r -> TF.Test
zqTests p = testGroupM (showType p) $ ($ p) <$> [
  hideArgs "(+)" prop_add,
  hideArgs "(*)" prop_mul,
  hideArgs "^-1" prop_recip,
  hideArgs "extension ring (*)" prop_mul_ext
  ]

prop_add :: forall r . (Ring r, Eq r) => LiftedMod r -> LiftedMod r -> Test r
prop_add (LMod x) (LMod y) = test $ fromIntegral (x + y) == (fromIntegral x + fromIntegral y :: r)

prop_mul :: forall r . (Ring r, Eq r) => LiftedInvertible r -> LiftedInvertible r -> Test r
prop_mul (LInv x) (LInv y) = test $ fromIntegral (x * y) == (fromIntegral x * fromIntegral y :: r)

prop_recip :: (Field r, Eq r) => Invertible r -> Test r
prop_recip (Invertible x) = test $ one == (x * recip x)

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: (CRTEmbed r, Eq r) => Invertible r -> Invertible r -> Test r
prop_mul_ext (Invertible x) (Invertible y) = test $
  let z = x * y
      z' = fromExt $ toExt x * toExt y
  in z == z'
