{-|
Module      : Crypto.Lol.Tests.ZqTests
Description : Tests for modular arithmetic.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for modular arithmetic.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- TODO: Update tests in this module to use QuickCheck

module Crypto.Lol.Tests.ZqTests (zqTests) where

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.Tests

import Control.Applicative
import Control.Monad.Random

import qualified Test.Framework  as TF
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Gen (chooseAny)

-- | Tests for modular arithmetic implementations.
zqTests :: forall r . (Mod r, _) => Proxy r -> TF.Test
zqTests p =
  let gen1 = chooseAny :: QC.Gen (LiftedMod r, LiftedMod r)
      gen2 = chooseAny :: QC.Gen (LiftedInvertible r, LiftedInvertible r)
      gen3 = chooseAny :: QC.Gen (Invertible r)
      gen4 = chooseAny :: QC.Gen (Invertible r, Invertible r) in
  TF.testGroup (showType p) [
  testWithGen "(+)" prop_add gen1,
  testWithGen "(*)" prop_mul gen2,
  testWithGen "^-1" prop_recip gen3,
  testWithGen "extension ring (*)" prop_mul_ext gen4
  ]

prop_add :: forall r . (Ring r, Eq r) => (LiftedMod r, LiftedMod r) -> Bool
prop_add (LMod x, LMod y) = fromIntegral (x + y) == (fromIntegral x + fromIntegral y :: r)

prop_mul :: forall r . (Ring r, Eq r) => (LiftedInvertible r, LiftedInvertible r) -> Bool
prop_mul (LInv x, LInv y) = fromIntegral (x * y) == (fromIntegral x * fromIntegral y :: r)

prop_recip :: (Field r, Eq r) => Invertible r -> Bool
prop_recip (Invertible x) = one == (x * recip x)

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: (CRTEmbed r, Eq r) => (Invertible r, Invertible r) -> Bool
prop_mul_ext (Invertible x, Invertible y) =
  let z = x * y
      z' = fromExt $ toExt x * toExt y
  in z == z'

data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

deriving instance Show (ModRep r) => Show (LiftedMod r)

instance (Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Random (LiftedMod r) where
  random =
    let q = proxy modulus (Proxy::Proxy r)
    in \g -> let (x,g') = randomR (0,q-1) g in (LMod x, g')
  randomR = error "randomR not defined for `LiftedMod`"


data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

deriving instance Show (ModRep r) => Show (LiftedInvertible r)

instance (Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Random (LiftedInvertible r) where
  random =
    let q = proxy modulus (Proxy::Proxy r)
    in \g -> let (x,g') = randomR (1,q-1) g
             in if gcd x q == 1 then (LInv x, g') else random g'
  randomR = error "randomR not defined for `LiftedInvertible`"

newtype Invertible r = Invertible r

deriving instance Show r => Show (Invertible r)

instance (Random (LiftedInvertible r), Ring r, ToInteger (ModRep r))
  => Random (Invertible r) where
  random g =
    let (LInv x :: LiftedInvertible r, g') = random g
    in (Invertible $ fromIntegral x, g')
  randomR = error "randomR not defined for `Invertible`"
