{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Interpreter.Dup
( Dup, dup
) where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

data Dup ex1 ex2 e a = Dup (ex1 e a) (ex2 e a)

dup :: Dup ex1 ex2 e a -> (ex1 e a, ex2 e a)
dup (Dup a1 a2) = (a1,a2)

instance (Lambda ex1, Lambda ex2) => Lambda (Dup ex1 ex2) where
  lam (Dup f1 f2) = Dup (lam f1) (lam f2)
  (Dup f1 f2) $: (Dup a1 a2) = Dup (f1 $: a1) (f2 $: a2)

instance (DB ex1 a, DB ex2 a) => DB (Dup ex1 ex2) a where
  v0 = Dup v0 v0
  s (Dup a1 a2) = Dup (s a1) (s a2)

instance (Add ex1 a, Add ex2 a) => Add (Dup ex1 ex2) a where
  (Dup a1 a2) +: (Dup b1 b2) = Dup (a1 +: b1) (a2 +: b2)

instance (Mul ex1 a b, Mul ex2 a b) => Mul (Dup ex1 ex2) a b where
  (Dup a1 a2) *: (Dup b1 b2) = Dup (a1 *: b1) (a2 *: b2)
