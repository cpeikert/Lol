{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module Crypto.Alchemy.Interpreter.Eval
( E, eval
)
where

import Data.Tuple

import Algebra.Additive as Additive
import Algebra.Ring     as Ring
import NumericPrelude

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

-- | Metacircular evaluator.
newtype E e a = E { appE :: e -> a }

-- | Evaluate a closed expression (i.e., one not having any unbound
-- variables)
eval :: E () a -> a
eval = flip appE ()

instance Lambda E where
  lam f  = E $ curry $ appE f
  f $: a = E $ \e -> appE f e $ appE a e

instance DB E a where
  v0  = E snd
  s a = E $ appE a . fst

instance (Additive.C a) => Add E a where
  x +: y = E $ \e -> appE x e + appE y e

instance (Ring.C a) => Mul E a a where
  x *: y = E $ \e -> appE x e * appE y e
