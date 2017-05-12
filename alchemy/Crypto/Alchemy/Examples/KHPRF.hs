{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Alchemy.Examples.KHPRF where

import Crypto.Lol
import Crypto.Lol.Types
import Crypto.Lol.Cyclotomic.Tensor (TElt) -- EAC: I shouldn't need to explicitly import this
import Crypto.Lol.Types.ZPP                -- EAC: I shouldn't need to explicitly import this...

import Crypto.Alchemy.Interpreter.PT2CT.Noise hiding (take)
import Crypto.Alchemy.Interpreter.RescaleToTree
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.TunnelCyc

-- a concrete Z_2^e data type
type Z2E e i = ZqBasic ('PP '(Prime2, e)) i

easyPRF :: forall t r s z2k k i expr env h h' h'' zqenv e .
  (z2k ~ Z2E k i, Lambda expr,
   -- tunnel
   TunnelCyc expr (PNoise h''), TunnelCycCtx expr (PNoise h'') t e r s z2k,
   PreTunnelCyc expr (PNoise h'') ~ PNoise h', FunCtx t r s z2k, e ~ FGCD r s,
   -- rescaleCycCRT
   RescaleCycCRTCtx zqenv env t s expr k (PNoise h (ZqBasic PP2 i)) (PNoise h'' (Cyc t s z2k)))
  => expr env (PNoise h' (Cyc t r z2k)) -> expr env (PNoise h (Cyc t s (Z2E 'O i)))
easyPRF x =
  let roundPT = proxy rescaleCycCRT (Proxy::Proxy k)
  in roundPT $ tunnelCyc decToCRT x

type FunCtx t r s zp = FunCtx' t (FGCD r s) r s zp

type FunCtx' t e r s zp =
  (e `Divides` r, e `Divides` s, CElt t zp,  -- linearDec
   ZPP zp, TElt t (ZpOf zp))

-- EAC: needs a home; currently replicated in several places
decToCRT :: forall s t r zp e . (FunCtx t r s zp, e ~ FGCD r s) => Linear t zp e r s
decToCRT =
  let crts = proxy crtSet (Proxy::Proxy e)
      r = proxy totientFact (Proxy::Proxy r)
      e = proxy totientFact (Proxy::Proxy e)
      dim = r `div` e
      -- only take as many crts as we need
      -- otherwise linearDec fails
  in linearDec $ take dim crts
