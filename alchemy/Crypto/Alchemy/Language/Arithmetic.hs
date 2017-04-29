{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Language.Arithmetic where

-- | Addition.

class Add expr a where
  infixl 6 +:                   -- match Haskell's precedence

  (+:) :: expr e a -> expr e a -> expr e a

-- | Multiplication. (Note that the input type @b@ may differ from the
-- output type @a@.)

class Mul expr a where
  infixl 7 *:                   -- match Haskell's precedence
  type PreMul expr a

  (*:) :: expr e (PreMul expr a) -> expr e (PreMul expr a) -> expr e a
