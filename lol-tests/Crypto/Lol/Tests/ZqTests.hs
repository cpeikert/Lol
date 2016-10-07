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

module Crypto.Lol.Tests.ZqTests (zqTests) where

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Tests
import Crypto.Lol.Utils.GenArgs
import Crypto.Lol.Utils.ShowType

import Control.Applicative
import Control.Monad.Random

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

data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

newtype Invertible r = Invertible r

instance (MonadRandom rnd, Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedMod r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
    in LMod <$> getRandomR (0,q-1)

instance (MonadRandom rnd, Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedInvertible r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
        go = do
          x <- getRandomR (1,q-1)
          if gcd x q == 1
          then return $ LInv x
          else go
    in go

instance (MonadRandom rnd, Generatable rnd (LiftedInvertible r), Ring r)
  => Generatable rnd (Invertible r) where
  genArg = do
    (LInv x) :: LiftedInvertible r <- genArg
    return $ Invertible $ fromIntegral x