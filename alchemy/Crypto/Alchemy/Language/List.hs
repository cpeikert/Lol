
module Crypto.Alchemy.Language.List where

-- | Symantics for list operations.

class List expr where
  -- | Empty list.
  nil_  :: expr e [a]

  -- | List constructor (analogue of ':').
  cons_ :: expr e (a -> [a] -> [a])

{- CJP: Keeping these out for now.  They aren't useful until we have a
way to compare against nil, and have conditionals in the language.

  -- | Head of list (input must be nonempty).
  head_ :: expr e ([a] -> a)

  -- | Tail of list (input must be nonempty).
  tail_ :: expr e ([a] -> [a])
-}
