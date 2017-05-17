{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module LinearDec2CRT where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor (TElt) -- EAC: I shouldn't need to explicitly import this
import Crypto.Lol.Types.ZPP                -- EAC: I shouldn't need to explicitly import this...

import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.LinearCyc

import Data.Singletons.Prelude.List (Reverse)

-- EAC: We can get rid of signatures once #13524 is fixed (should be in 8.2)

linear2 :: forall t r u s zp ms env expr mr mu .
  (LinearCyc expr mu, LinearCyc expr ms,
   LinearCycCtx expr mu t (FGCD r u) r u zp,
   LinearCycCtx expr ms t (FGCD u s) u s zp,
   mu ~ PreLinearCyc expr ms, mr ~ PreLinearCyc expr mu,
   Lambda expr, FunCtx t r u zp, FunCtx t u s zp)
  => Proxy u -> expr env (mr (Cyc t r zp) -> ms (Cyc t s zp))
linear2 _ = linearDecToCRT_ .: linearDecToCRT_ @u

linear5 :: forall t rngs a postTunnelPNoise env expr h0 h1 h2 h3 h4 h5 preTunnelPNoise  .
  (-- tunnel
   rngs ~ '[h0,h1,h2,h3,h4,h5],
   LinearChainCtx expr t postTunnelPNoise a rngs,
   PreLinearChainM expr postTunnelPNoise rngs ~ preTunnelPNoise)
  => Proxy rngs -> expr env (preTunnelPNoise (Cyc t h0 a) -> postTunnelPNoise (Cyc t h5 a))
linear5 _ = linearDecToCRT_ .: linearDecToCRT_ @h4 .:
    linearDecToCRT_ @h3 .: linearDecToCRT_ @h2 .: linearDecToCRT_ @h1

-- given the output 'm' (Cyc wrapper) of a chain of tunnels, returns the input Cyc wrapper.
type family PreLinearChainM expr m (rngs :: [Factored]) where
  PreLinearChainM expr m '[x] = m
  PreLinearChainM expr m (r ': rngs) = PreLinearChainM expr (PreLinearCyc expr m) rngs

-- | Context for a chaini of tunnels using the decToCRT linear function.
type LinearChainCtx expr m t z2k (rngs :: [Factored]) = LinearChainCtx' expr m t z2k (Reverse rngs)

-- | Helper family for LinearChainCtx. Takes rings in *reverse* order so that
-- the Cyc wrapper `m` is applied appropriately.
type family LinearChainCtx' expr m t z2k (rngs :: [Factored]) where
  LinearChainCtx' expr t m z2k '[x] = (Lambda expr)
  -- EAC: Reverse r and s here because they are applied in reverse
  LinearChainCtx' expr t m z2k (r ': s ': rngs) =
    (LinearDecToCRTCtx expr m t s r z2k, LinearChainCtx' expr t (PreLinearCyc expr m) z2k (s ': rngs))

-- | Constraint synonym for tunnelCyc'
type LinearDecToCRTCtx expr m t r s zp =
  (LinearCyc expr m, LinearCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)

linearDecToCRT_ :: forall s expr env m t r zp .
  (LinearCyc expr m, LinearCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)
  => expr env ((PreLinearCyc expr m) (Cyc t r zp) -> m (Cyc t s zp))
linearDecToCRT_ = linearCyc_ decToCRT

-- | Tunnel with the decToCRT linear function.
linearDecToCRT :: forall s expr env m t r zp .
  (LinearCyc expr m, LinearCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)
  => expr env ((PreLinearCyc expr m) (Cyc t r zp)) -> expr env (m (Cyc t s zp))
linearDecToCRT a = linearCyc_ decToCRT $: a

-- | Constraint synonym for decToCRT
type FunCtx t r s zp = FunCtx' t (FGCD r s) r s zp

type FunCtx' t e r s zp =
  (e `Divides` r, e `Divides` s, CElt t zp,  -- linearDec
   ZPP zp, TElt t (ZpOf zp))

-- EAC: needs a home; currently replicated in several places
-- | Linear function mapping decoding basis coefficients to CRT slots
decToCRT :: forall s t r zp e . (FunCtx t r s zp, e ~ FGCD r s) => Linear t zp e r s
decToCRT =
  let crts = proxy crtSet (Proxy::Proxy e)
      r = proxy totientFact (Proxy::Proxy r)
      e = proxy totientFact (Proxy::Proxy e)
      dim = r `div` e
      -- only take as many crts as we need
      -- otherwise linearDec fails
  in linearDec $ take dim crts
