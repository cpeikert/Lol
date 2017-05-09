{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Crypto.Alchemy.Language.TunnelCyc where

import Crypto.Lol
import GHC.Exts   (Constraint)

-- | Symantics for ring-tunneling on cyclotomics.

class TunnelCyc expr m where

  -- | Constraints needed to tunnel
  type TunnelCycCtx
         expr
         (m :: * -> *)
         (t :: Factored -> * -> *)
         (e :: Factored)
         (r :: Factored)
         (s :: Factored)
         zp :: Constraint

  -- | 'Cyc' wrapper for the input to tunneling
  type PreTunnelCyc expr m :: * -> *

  -- | An object-language expression representing the given linear function.
  tunnelCyc :: (TunnelCycCtx expr m t e r s zp)
    => Linear t zp e r s
    -> expr env ((PreTunnelCyc expr m) (Cyc t r zp) -> m (Cyc t s zp))
