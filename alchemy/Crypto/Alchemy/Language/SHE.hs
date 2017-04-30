{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Language.SHE where

import Crypto.Lol                      (Cyc, Factored)
import Crypto.Lol.Applications.SymmSHE (CT, KSQuadCircHint, TunnelInfo)
import GHC.Exts

-- | Symantics for somewhat-homomorphic encryption operations (not
-- including those defined in 'Crypto.Alchemy.Language.Arithmetic').

class SHE expr where

  type ModSwitchCtx expr ct zp' :: Constraint
  type RescaleCtx   expr ct zq' :: Constraint
  type AddPubCtx    expr ct     :: Constraint
  type MulPubCtx    expr ct     :: Constraint
  type KeySwitchCtx expr ct zq' gad :: Constraint
  type TunnelCtx    expr (t :: Factored -> * -> *) (e :: Factored) (r :: Factored) (s :: Factored) (e' :: Factored) (r' :: Factored) (s' :: Factored) zp zq gad :: Constraint

  modSwitchPT :: (ModSwitchCtx expr ct zp', ct ~ CT m zp (Cyc t m' zq))
    => expr env ct -> expr env (CT m zp' (Cyc t m' zq))

  rescale :: (RescaleCtx expr ct zq', ct ~ CT m zp (Cyc t m' zq))
    => expr env (CT m zp (Cyc t m' zq')) -> expr env ct

  -- CJP: doesn't quite fall into addLit, though we could possibly
  -- generalize addLit to cover this (not clear if a good idea; this
  -- signature is pretty special)
  addPublic :: (AddPubCtx expr ct, ct ~ CT m zp (Cyc t m' zq))
    => Cyc t m zp -> expr env ct -> expr env ct

  -- CJP: ditto here
  mulPublic :: (MulPubCtx expr ct, ct ~ CT m zp (Cyc t m' zq))
    => Cyc t m zp -> expr env ct -> expr env ct

  keySwitchQuad :: (KeySwitchCtx expr ct zq' gad, ct ~ CT m zp (Cyc t m' zq))
    => KSQuadCircHint gad (Cyc t m' zq') -> expr env ct -> expr env ct

  tunnel :: (TunnelCtx expr t e r s e' r' s' zp zq gad)
    => TunnelInfo gad t e r s e' r' s' zp zq
    -> expr env (CT r zp (Cyc t r' zq))
    -> expr env (CT s zp (Cyc t s' zq))
