{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Computes the multiplicative depth of an expression.

module Crypto.Alchemy.Interpreter.Depth
( D, depth )
where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.LinearCyc

newtype D expr a = D { depth :: Int }

instance Add D a where
  add_ = D 0
  neg_ = D 0

instance AddLit D a where
  addLit_ _ = D 0

instance Mul D a where
  type PreMul D a = a
  mul_ = D 1

instance MulLit D a where
  mulLit_ _ = D 0

instance Div2 D a where
  type PreDiv2 D a = a
  div2_ = D 0

instance Lambda D where
  lam (D i) = D i
  (D f) $: (D a) = D $ f + a
  v0 = D 0
  s (D i) = D i

instance List D where
  nil_ = D 0
  cons_ = D 0

instance Functor_ D where
  fmap_ = D 0

instance Applicative_ D where
  pure_ = D 0
  ap_ = D 0

instance Monad_ D where
  bind_ = D 0

instance MonadReader_ D where
  ask_ = D 0
  local_ = D 0

instance MonadWriter_ D where
  tell_ = D 0
  listen_ = D 0

instance SHE D where
  type ModSwitchPTCtx   D ct zp' = ()
  type ModSwitchCtx     D ct zq' = ()
  type AddPublicCtx     D ct     = ()
  type MulPublicCtx     D ct     = ()
  type KeySwitchQuadCtx D ct gad = ()
  type TunnelCtx D t e r s e' r' s' zp zq gad = ()

  modSwitchPT_ = D 0
  modSwitch_ = D 0
  addPublic_ _ = D 0
  mulPublic_ _ = D 0
  keySwitchQuad_ _ = D 0
  tunnel_ _ = D 0

instance LinearCyc D rep where
  type PreLinearCyc D rep = rep
  type LinearCycCtx D rep t e r s zp = ()

  linearCyc_ _ = D 0
