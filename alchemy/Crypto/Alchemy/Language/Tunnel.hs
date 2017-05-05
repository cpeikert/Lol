{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Crypto.Alchemy.Language.Tunnel where

-- | Symantics for (plaintext) ring-tunneling.

class Tunnel expr e r s where

  -- | Type representing @E@-linear functions from @R@ to @S@.
  -- (It is injective in e,r,s.)
  type LinearOf expr e r s = lin | lin -> e r s

  -- | An object-language expression representing the given linear
  -- function.
  tunnel :: LinearOf expr e r s -> expr env (r -> s)
