{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Alchemy.Language.Lit where

class Lit expr a where
  -- | Embed a literal into an expression.
  lit :: a -> expr e a