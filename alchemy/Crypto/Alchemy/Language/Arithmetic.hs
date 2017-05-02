{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Language.Arithmetic where

import Crypto.Alchemy.Language.Lambda

-- | Addition.

class Add expr a where
  -- | Addition.
  add :: expr e (a -> a -> a)
  -- | Negation.
  neg :: expr e (a -> a)

-- | Convenient equivalent of 'add'.
infixl 6 +:                     -- match Haskell's precedence
(+:), (-:) :: (Add expr a, Lambda expr) => expr e a -> expr e a -> expr e a
a +: b = add $: a $: b

infixl 6 -:
a -: b = a +: neg $: b

-- | Addition of (metalanguage) literals to (object language)
-- expressions.

class AddLit expr a where
  infixl 6 `addLit`

  addLit :: a -> expr e a -> expr e a

-- | Multiplication. (Note that the input type @b@ may differ from the
-- output type @a@.)

class Mul expr a where
  type PreMul expr a

  -- | Multiplication.
  mul :: expr e (PreMul expr a -> PreMul expr a -> a)

-- | Convenient equivalent to 'mul'.
infixl 7 *:                     -- match Haskell's precedence
(*:) :: (Mul expr a, Lambda expr) =>
        expr e (PreMul expr a) -> expr e (PreMul expr a) -> expr e a
a *: b = mul $: a $: b

-- | Multiplication of (metalanguage) literals to (object language)
-- expressions.

class MulLit expr a where
  infixl 7 `mulLit`

  mulLit :: a -> expr e a -> expr e a
