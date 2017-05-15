{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Alchemy.Interpreter.DedupRescale
( DedupRescale, dedupRescale)
where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE

import Crypto.Lol                      (Cyc)
import Crypto.Lol.Applications.SymmSHE (CT)
import Data.Typeable

data DedupRescale expr e a where
  -- rescaleLinear_ itself
  --Rescale  :: expr e a -> DedupRescale expr e a
  -- a result of rescaleLinear_
  Rescaled :: (Typeable b) => expr e b -> expr e a -> DedupRescale expr e a
  -- something else
  Unscaled :: expr e a -> DedupRescale expr e a

-- | De-duplicate rescaling operations in an expression.
dedupRescale :: DedupRescale expr e a -> expr e a
dedupRescale (Rescaled _ a) = a
dedupRescale (Unscaled a) = a

-- shorter
dr = dedupRescale

-- EAC: sharing implications?
-- consider: (\x -> rescaleDown x + (rescaleDown x*x)) (rescaleUp y)
-- if we simplify this to \y -> y + ((rescaleUp y)*(rescaleUp y)),
-- we rescaleUp twice (but remove the duplicate)

instance (Lambda expr) => Lambda (DedupRescale expr) where
  lam (Rescaled b a) = Rescaled (lam b) (lam a)
  lam (Unscaled   a) = Unscaled         (lam a)

  v0 = Unscaled v0

  s   (Rescaled b a) = Rescaled (s b) (s a)
  s   (Unscaled   a) = Unscaled       (s a)

  ($:) :: DedupRescale expr e (a -> b)
       -> DedupRescale expr e a
       -> DedupRescale expr e b

  (Unscaled   f) $: a = Unscaled $ f $: dr a
  f              $: (Unscaled a) = Unscaled $ dr f $: a

  -- the interesting case: double-rescaling
  (Rescaled _ f) $: (Rescaled (prev :: expr e b') a) =
    case (eqT :: Maybe (b :~: b')) of
      Just Refl -> Unscaled prev

instance (List expr) => List (DedupRescale expr) where
  nil_  = Unscaled nil_
  cons_ = Unscaled cons_

instance (Add expr a) => Add (DedupRescale expr) a where
  add_ = Unscaled add_
  neg_ = Unscaled neg_

instance (AddLit expr a) => AddLit (DedupRescale expr) a where
  addLit_ x = Unscaled $ addLit_ x

instance (MulLit expr a) => MulLit (DedupRescale expr) a where
  mulLit_ x = Unscaled $ mulLit_ x

instance (Mul expr a) => Mul (DedupRescale expr) a where
  type PreMul (DedupRescale expr) a = PreMul expr a
  mul_ = Unscaled mul_

instance (SHE expr, Lambda expr) => SHE (DedupRescale expr) where

  type ModSwitchPTCtx (DedupRescale expr) ct zp' =
    (ModSwitchPTCtx expr ct zp')
  type ModSwitchCtx (DedupRescale expr) (CT m zp (Cyc t m' zq)) zq' =
    (Typeable (CT m zp (Cyc t m' zq)),
     Typeable (CT m zp (Cyc t m' zq')),
     ModSwitchCtx expr (CT m zp (Cyc t m' zq)) zq')
  type AddPublicCtx (DedupRescale expr) ct = (AddPublicCtx expr ct)
  type MulPublicCtx (DedupRescale expr) ct = (MulPublicCtx expr ct)
  type KeySwitchQuadCtx (DedupRescale expr) ct gad =
    (KeySwitchQuadCtx expr ct gad)
  type TunnelCtx    (DedupRescale expr) t e r s e' r' s' zp zq gad =
    (TunnelCtx expr t e r s e' r' s' zp zq gad)

  modSwitchPT_ = Unscaled modSwitchPT_

  modSwitch_ :: forall ct zq' m zp t m' zq e .
    (ModSwitchCtx (DedupRescale expr) ct zq', ct ~ CT m zp (Cyc t m' zq))
    => (DedupRescale expr) e (ct -> CT m zp (Cyc t m' zq'))

  modSwitch_ =
    -- check if this rescale is a no-op
    case (eqT :: Maybe (ct :~: CT m zp (Cyc t m' zq'))) of
      Just Refl -> Unscaled $ lam v0 -- skip it, w/identity function
      Nothing   -> Rescaled undefined modSwitch_

  addPublic_     p = Unscaled $ addPublic_ p
  mulPublic_     p = Unscaled $ mulPublic_ p
  keySwitchQuad_ h = Unscaled $ keySwitchQuad_ h
  tunnel_        h = Unscaled $ tunnel_ h

instance (Functor_ expr) => Functor_ (DedupRescale expr) where
  fmap_ = Unscaled fmap_

instance (Applicative_ expr) => Applicative_ (DedupRescale expr) where
  pure_ = Unscaled pure_
  ap_   = Unscaled ap_

instance (Monad_ expr) => Monad_ (DedupRescale expr) where
  bind_ = Unscaled bind_

instance (MonadReader_ expr) => MonadReader_ (DedupRescale expr) where
  ask_   = Unscaled ask_
  local_ = Unscaled local_

instance (MonadWriter_ expr) => MonadWriter_ (DedupRescale expr) where
  tell_   = Unscaled tell_
  listen_ = Unscaled listen_
