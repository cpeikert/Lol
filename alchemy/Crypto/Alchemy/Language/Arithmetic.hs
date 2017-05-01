{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Language.Arithmetic where

-- | Addition.

class Add expr a where
  infixl 6 +:                   -- match Haskell's precedence

  (+:) :: expr e a -> expr e a -> expr e a
  neg  :: expr e a -> expr e a

-- | Addition of (metalanguage) literals to (object language)
-- expressions.

class AddLit expr a where
  infixl 6 `addLit`

  addLit :: a -> expr e a -> expr e a

-- | Multiplication. (Note that the input type @b@ may differ from the
-- output type @a@.)

class Mul expr a where
  type PreMul expr a

  infixl 7 *:                   -- match Haskell's precedence
  (*:) :: expr e (PreMul expr a) -> expr e (PreMul expr a) -> expr e a

-- | Multiplication of (metalanguage) literals to (object language)
-- expressions.

class MulLit expr a where
  infixl 7 `mulLit`

  mulLit :: a -> expr e a -> expr e a

infixl 6  -:
(-:) :: (Add expr a) => expr e a -> expr e a -> expr e a
a -: b = a +: neg b
