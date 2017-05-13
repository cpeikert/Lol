{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Crypto.Alchemy.Language.TunnelCyc where

import Crypto.Alchemy.Language.Lambda
import Crypto.Lol
import GHC.Exts                       (Constraint)

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

    -- CJP: TRY this: a class arg 'h' (for hook), and associated type
    -- TunnelCycRep h t s zp (and PreTunnelCycRep h t r zp) for the
    -- object-lang types in tunnelCyc_.  These should be injective in
    -- at least t,s,zp and maybe even h.

  -- | An object-language expression representing the given linear function.
  tunnelCyc_ :: (TunnelCycCtx expr m t e r s zp)
    => Linear t zp e r s
    -> expr env ((PreTunnelCyc expr m) (Cyc t r zp) -> m (Cyc t s zp))

tunnelCyc :: (TunnelCyc expr m, TunnelCycCtx expr m t e r s zp, Lambda expr)
  => Linear t zp e r s
  -> expr env ((PreTunnelCyc expr m) (Cyc t r zp))
  -> expr env (m (Cyc t s zp))
tunnelCyc f a = tunnelCyc_ f $: a
