{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Crypto.Alchemy.Interpreter.Size where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.TunnelCyc

newtype S expr a = S Int

instance Add S a where
  add_ = S 1
  neg_ = S 1

instance AddLit S a where
  addLit_ _ = S 1

instance Mul S a where
  type PreMul S a = a
  mul_ = S 1

instance MulLit S a where
  mulLit_ _ = S 1

instance Lambda S where
  lam (S i) = S $ i+1
  (S f) $: (S a) = S $ f + a + 1
  v0 = S 1
  s (S i) = S i

instance List S where
  nil_ = S 1
  cons_ = S 1

instance Functor_ S where
  fmap_ = S 1

instance Applicative_ S where
  pure_ = S 1
  ap_ = S 1

instance Monad_ S where
  bind_ = S 1

instance MonadReader_ S where
  ask_ = S 1
  local_ = S 1

instance MonadWriter_ S where
  tell_ = S 1
  listen_ = S 1

instance SHE S where
  type ModSwitchPTCtx   S ct zp' = ()
  type ModSwitchCtx     S ct zq' = ()
  type AddPublicCtx     S ct     = ()
  type MulPublicCtx     S ct     = ()
  type KeySwitchQuadCtx S ct gad = ()
  type TunnelCtx S t e r s e' r' s' zp zq gad = ()

  modSwitchPT_ = S 1

  modSwitch_ = S 1

  addPublic_ _ = S 1

  mulPublic_ _ = S 1

  keySwitchQuad_ _ = S 1

  tunnel_ _ = S 1

instance TunnelCyc S rep where
  type PreTunnelCyc S rep = rep
  type TunnelCycCtx S rep t e r s zp = ()

  tunnelCyc_ _ = S 1