{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Crypto.Alchemy.Language.Tunnel where

import Crypto.Lol
import GHC.Exts (Constraint)

-- | Symantics for (plaintext) ring-tunneling.

class Tunnel expr m where

  -- | Constraints needed to tunnel
  type TunnelCtx
         expr
         (m :: * -> *)
         (t :: Factored -> * -> *)
         (e :: Factored)
         (r :: Factored)
         (s :: Factored)
         zp :: Constraint

  -- | 'Cyc' wrapper for the input to tunneling
  type PreTunnel expr m :: * -> *

  -- | An object-language expression representing the given linear function.
  tunnel :: (TunnelCtx expr m t e r s zp) =>
    Linear t zp e r s -> expr env ((PreTunnel expr m) (Cyc t r zp) -> m (Cyc t s zp))
