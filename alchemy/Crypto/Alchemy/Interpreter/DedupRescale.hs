{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Alchemy.Interpreter.DedupRescale where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE

import Crypto.Lol (Cyc)
import Crypto.Lol.Applications.SymmSHE (CT)
import Data.Typeable

-- unlike section 2.4 of the lecture notes, we don't use a function from the
-- context to the value
-- this is because *every* op would then have to call rescaleCT (if there's a context)
-- with the possible exception of rescaleCT itself, if the context is right.
-- Thus it seems cleaner to work "bottom-up" rather than "top-down".

data DupRescale expr e a where
  -- indicates that `expr a` is the result of a rescale from b to a
  -- holds the value both pre- and post-rescaling
  Ctx :: (Typeable b) => {ctx :: expr e b, dedupRescale :: expr e a} -> DupRescale expr e a
  -- indicates that `expr a` is *not* the output of rescaleCT.
  NoCtx :: {dedupRescale :: expr e a} -> DupRescale expr e a

-- map, ignoring context
dupMap :: (expr e a -> expr e b) -> DupRescale expr e a -> DupRescale expr e b
dupMap f = NoCtx . f . dedupRescale


-- EAC: sharing implications?
-- consider: (\x -> (rescaleDown x) + (x *x)) (rescaleUp y)
-- if we simplify this to \y -> y + ((rescaleUp y)*(rescaleUp y)),
-- we end up doing the rescale twice (but remove the duplicate)
instance (Lambda expr) => Lambda (DupRescale expr) where
  lam f = NoCtx $ lam $ dedupRescale f
  f $: a = NoCtx $ (dedupRescale f) $: (dedupRescale a)

instance (DB expr a) => DB (DupRescale expr) a where
  v0  = NoCtx v0
  s a = NoCtx $ s $ dedupRescale a

instance (Add expr a) => Add (DupRescale expr) a where
  a +: b = NoCtx $ (dedupRescale a) +: (dedupRescale b)
  negate' = dupMap negate'

instance (Mul expr a) => Mul (DupRescale expr) a where
  type PreMul (DupRescale expr) a = PreMul expr a
  a *: b = NoCtx $ (dedupRescale a) *: (dedupRescale b)

instance (SHE expr) => SHE (DupRescale expr) where

  type ModSwitchPTCtx (DupRescale expr) ct zp' =
    (ModSwitchPTCtx expr ct zp')
  type RescaleLinearCtx (DupRescale expr) (CT m zp (Cyc t m' zq)) zq' =
    (Typeable (CT m zp (Cyc t m' zq)),
     Typeable (CT m zp (Cyc t m' zq')),
     RescaleLinearCtx expr (CT m zp (Cyc t m' zq)) zq')
  type AddPublicCtx (DupRescale expr) ct = (AddPublicCtx expr ct)
  type MulPublicCtx (DupRescale expr) ct = (MulPublicCtx expr ct)
  type KeySwitchQuadCtx (DupRescale expr) ct zq' gad = (KeySwitchQuadCtx expr ct zq' gad)
  type TunnelCtx    (DupRescale expr) t e r s e' r' s' zp zq gad = (TunnelCtx expr t e r s e' r' s' zp zq gad)

  modSwitchPT = dupMap modSwitchPT

  rescaleLinear :: forall ct zq' m zp t m' zq e .
    (RescaleLinearCtx (DupRescale expr) (CT m zp (Cyc t m' zq)) zq', ct ~ CT m zp (Cyc t m' zq))
            => (DupRescale expr) e (CT m zp (Cyc t m' zq')) -> (DupRescale expr) e ct
  rescaleLinear (Ctx (prev :: expr e ct') x) =
    case (eqT :: Maybe (ct' :~: ct)) of
      -- previous op scaled from zq -> zq', so rather than rescaling back down, we just use the un-rescaled value
      (Just Refl) -> NoCtx prev
      -- previous op was a rescale, but from a different modulus
      Nothing -> Ctx x $ rescaleLinear x
  rescaleLinear (NoCtx x) =
    -- even if there's no context, we might be able to remove this rescale if
    -- the input modulus is the same as the output modulus
    case (eqT :: Maybe ((CT m zp (Cyc t m' zq')) :~: ct)) of
      (Just Refl) -> NoCtx x
      Nothing -> Ctx x $ rescaleLinear x

  addPublic = dupMap . addPublic

  mulPublic = dupMap . mulPublic

  keySwitchQuad = dupMap . keySwitchQuad

  tunnel = dupMap . tunnel

