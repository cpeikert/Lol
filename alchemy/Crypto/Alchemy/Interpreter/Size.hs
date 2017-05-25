{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Computes the size of an AST for the expression.

module Crypto.Alchemy.Interpreter.Size
( S, size )
where

import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.LinearCyc

import Crypto.Lol                      (Cyc,PrimePower(..), Prime2)
import qualified Crypto.Lol as L
import Crypto.Lol.Applications.SymmSHE (CT)
import Crypto.Lol.Types

import Control.Monad.Identity

newtype S e a = S { size :: Int }

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

-- EAC: ideas
-- 1. Dis-associate PreDiv2. It shouldn't depend on expr, so make it an
--    unassociated, open type family
-- 2. Make this interpreter recursive. Could make sense (maybe I want to check
--    the size of a computation, then do an optimization pass, then check the size again.)
--    Of course this can be done already using dup. And, if we made (all) of the
--    interpreters recursive, then we'd need a dummy interpreter for the bottom of the stack.
instance Div2 S (Cyc t m (ZqBasic ('PP '(Prime2, k)) i)) where
  type PreDiv2 S (Cyc t m (ZqBasic ('PP '(Prime2, k)) i)) =
    Cyc t m (ZqBasic ('PP '(Prime2, 'L.S k)) i)
  div2_ = S 1

instance Div2 S (PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) where
  type PreDiv2 S (PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) =
    PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, 'L.S k)) i))
  div2_ = S 1

instance Div2 S (Identity (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) where
  type PreDiv2 S (Identity (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) =
    Identity (Cyc t m (ZqBasic ('PP '(Prime2, 'L.S k)) i))
  div2_ = S 1

instance Div2 S (CT m (ZqBasic ('PP '(Prime2, k)) i) (Cyc t m' zq)) where
  type PreDiv2 S (CT m (ZqBasic ('PP '(Prime2, k)) i) (Cyc t m' zq)) =
    CT m (ZqBasic ('PP '(Prime2, 'L.S k)) i) (Cyc t m' zq)
  div2_ = S 1

instance Lambda S where
  lam (S i) = S $ i+1
  (S f) $: (S a) = S $ f + a
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

instance LinearCyc S rep where
  type PreLinearCyc S rep = rep
  type LinearCycCtx S rep t e r s zp = ()

  linearCyc_ _ = S 1
