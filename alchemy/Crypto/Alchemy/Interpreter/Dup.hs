{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Interpreter.Dup
( Dup, dup
) where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

data Dup ex e a where
  Dup :: ex1 (Unzip1 e) (Fst a)
      -> ex2 (Unzip2 e) (Snd a)
      -> Dup '(ex1,ex2) e a

type family Fst a where
  Fst (a1,a2)  = a1
  Fst (a -> b) = Fst a -> Fst b

type family Snd a where
  Snd (a1,a2)  = a2
  Snd (a -> b) = Snd a -> Snd b

type family Unzip1 e where  -- use on, e.g., (((),(b1,b2)),(a1,a2))
  Unzip1 ()    = ()         -- weird-ish base case, but ((),()) doesn't work
  Unzip1 (e,a) = (Unzip1 e, Fst a)

type family Unzip2 e where
  Unzip2 ()    = ()
  Unzip2 (e,a) = (Unzip2 e, Snd a)

dup :: Dup '(ex1,ex2) e a
    -> (ex1 (Unzip1 e) (Fst a), ex2 (Unzip2 e) (Snd a))
dup (Dup a1 a2) = (a1,a2)

instance (Lambda ex1, Lambda ex2) => Lambda (Dup '(ex1,ex2)) where
  lam (Dup f1 f2) = Dup (lam f1) (lam f2)
  (Dup f1 f2) $: (Dup a1 a2) = Dup (f1 $: a1) (f2 $: a2)

instance (DB ex1 (Fst a), DB ex2 (Snd a)) => DB (Dup '(ex1,ex2)) a where
  v0 = Dup v0 v0
  s (Dup a1 a2) = Dup (s a1) (s a2)

instance (Add ex1 a1, Add ex2 a2) => Add (Dup '(ex1,ex2)) (a1,a2) where
  (Dup a1 a2) +: (Dup b1 b2) = Dup (a1 +: b1) (a2 +: b2)

instance (Mul ex1 a1, Mul ex2 a2) => Mul (Dup '(ex1,ex2)) (a1,a2) where
  type PreMul (Dup '(ex1,ex2)) (a1,a2) = (PreMul ex1 a1, PreMul ex2 a2)
  (Dup a1 a2) *: (Dup b1 b2) = Dup (a1 *: b1) (a2 *: b2)
