{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.ZqTests where

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Tests
import Crypto.Lol.Utils.ShowType

import Control.Applicative
import Control.Monad.Random

import qualified Test.Framework as TF

-- | Tests for 'ZqBasic'
zqTests :: _ => Proxy r -> TF.Test
zqTests p = testGroup (showType p) $ ($ p) <$> [
  genTestArgs "(+)" prop_add,
  genTestArgs "(*)" prop_mul,
  genTestArgs "^-1" prop_recip,
  genTestArgs "extension ring (*)" prop_mul_ext
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

data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

instance (Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Random (LiftedMod r) where
  random =
    let q = proxy modulus (Proxy::Proxy r)
    in \g -> let (x,g') = randomR (0,q-1) g in (LMod x, g')
  randomR = error "randomR not defined for `LiftedMod`"


data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

instance (Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Random (LiftedInvertible r) where
  random =
    let q = proxy modulus (Proxy::Proxy r)
    in \g -> let (x,g') = randomR (1,q-1) g
             in if gcd x q == 1 then (LInv x, g') else random g'
  randomR = error "randomR not defined for `LiftedInvertible`"

newtype Invertible r = Invertible r

instance (Random (LiftedInvertible r), Ring r, ToInteger (ModRep r))
  => Random (Invertible r) where
  random g =
    let (LInv x :: LiftedInvertible r, g') = random g
    in (Invertible $ fromIntegral x, g')
  randomR = error "randomR not defined for `Invertible`"
