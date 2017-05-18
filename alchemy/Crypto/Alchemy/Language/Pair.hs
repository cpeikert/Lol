
module Crypto.Alchemy.Language.Pair where

class Pair expr where
  pair_ :: expr e (a -> b -> (a,b))