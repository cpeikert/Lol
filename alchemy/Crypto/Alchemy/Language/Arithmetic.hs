{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Crypto.Alchemy.Language.Arithmetic where

-- | Addition.

class Add expr a where
  infixl 6 +:                   -- match Haskell's precedence

  (+:) :: expr e a -> expr e a -> expr e a

-- | Multiplication. (Note that the input type @b@ may differ from the
-- output type @a@.)

class Mul expr b a | a -> b where
  infixl 7 *:                   -- match Haskell's precedence

  (*:) :: expr e b -> expr e b -> expr e a
