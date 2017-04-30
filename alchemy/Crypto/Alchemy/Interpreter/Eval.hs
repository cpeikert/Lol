{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

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
import Crypto.Alchemy.Language.Lit

import Crypto.Lol (Cyc)

-- | Metacircular evaluator.
newtype E e a = E { unE :: e -> a }

-- | Evaluate a closed expression (i.e., one not having any unbound
-- variables)
eval :: E () a -> a
eval = flip unE ()

instance Lambda E where
  lam f  = E $ curry $ unE f
  f $: a = E $ \e -> unE f e $ unE a e

instance DB E a where
  v0  = E snd
  s a = E $ unE a . fst

instance (Additive.C a) => Add E a where
  x +: y = E $ \e -> unE x e + unE y e

instance (Ring.C a) => Mul E a where
  type PreMul E a = a
  x *: y = E $ \e -> unE x e * unE y e

instance Lit E (Cyc t m zp) where
  lit = E . const
