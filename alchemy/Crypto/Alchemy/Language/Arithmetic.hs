{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Language.Arithmetic where

import Crypto.Alchemy.Language.Lambda

-- | Addition.

class Add expr a where
  -- | Addition.
  add_ :: expr e (a -> a -> a)
  -- | Negation.
  neg_ :: expr e (a -> a)

infixl 6 +:, -:
(+:), (-:) :: (Add expr a, Lambda expr) => expr e a -> expr e a -> expr e a

-- | Convenient metalanguage version of 'add_'.
a +: b = add_ $: a $: b

-- | Convenient metalanguage version of subtraction.
a -: b = a +: (neg_ $: b)

-- | Addition of (metalanguage) literals to (object language)
-- expressions.

class AddLit expr a where
  infixl 6 >+:

  (>+:) :: a -> expr e a -> expr e a

-- | Multiplication. (Note that the input type @b@ may differ from the
-- output type @a@.)

class Mul expr a where
  type PreMul expr a

  -- | Multiplication.
  mul_ :: expr e (PreMul expr a -> PreMul expr a -> a)

-- | Convenient metalanguage version of 'mul'.
infixl 7 *:                     -- match Haskell's precedence
(*:) :: (Mul expr a, Lambda expr) =>
        expr e (PreMul expr a) -> expr e (PreMul expr a) -> expr e a
a *: b = mul_ $: a $: b

-- | Multiplication of (metalanguage) literals to (object language)
-- expressions.

class MulLit expr a where
  infixl 7 >*:

  (>*:) :: a -> expr e a -> expr e a

-- | Symantics for division-by-2 of both a value and its integer
-- modulus.

class Div2 expr a where
  type PreDiv2 expr a

  div2_ :: expr e (PreDiv2 expr a -> a)
