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

class TunnelCyc expr rep where

  -- | Constraints needed to tunnel
  type TunnelCycCtx
         expr
         (rep :: * -> *)
         (t :: Factored -> * -> *)
         (e :: Factored)
         (r :: Factored)
         (s :: Factored)
         zp :: Constraint

  -- | 'Cyc' wrapper for the input to tunneling
  type PreTunnelCyc expr rep :: * -> *

  -- | An object-language expression representing the given linear function.
  tunnelCyc_ :: (TunnelCycCtx expr rep t e r s zp)
    => Linear t zp e r s
    -> expr env ((PreTunnelCyc expr rep) (Cyc t r zp) -> rep (Cyc t s zp))

tunnelCyc :: (TunnelCyc expr rep, TunnelCycCtx expr rep t e r s zp, Lambda expr)
  => Linear t zp e r s
  -> expr env ((PreTunnelCyc expr rep) (Cyc t r zp))
  -> expr env (rep (Cyc t s zp))
tunnelCyc f a = tunnelCyc_ f $: a
