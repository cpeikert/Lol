{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Interpreter.Print
( P, pprint
)
where

import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.LinearCyc
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE

import Crypto.Lol                      (Cyc, Pos (..), Prime2,
                                        PrimePower (..))
import Crypto.Lol.Applications.SymmSHE (CT)
import Crypto.Lol.Types

import Control.Monad.Identity

-- the Int is the nesting depth of lambdas outside the expression
newtype P e a = P { unP :: Int -> String }

-- | Pretty-print a closed expression.
pprint :: P () a -> String
pprint = flip unP 0

pureP :: String -> P e a
pureP = P . const

-- | In the printout, variable indices grow "outside in," rather than
-- "inside out" (as in object-language code) because the
-- implementation is simpler that way.

instance Lambda P where
  lam f  = P $ \i -> "(\\v" ++ show  i ++ " -> " ++ unP f (i+1) ++ ")"
  f $: a = P $ \i -> "("    ++ unP f i ++ " "    ++ unP a i     ++ ")"
  v0     = P $ \i -> "v" ++ show (i-1)
  s  v   = P $ \i -> unP v (i-1)

instance List P where
  nil_  = pureP "nil"
  cons_ = pureP "cons"

instance Add P a where
  add_ = pureP "add"
  neg_ = pureP "neg"

instance Show a => AddLit P a where
  addLit_ a = pureP $ "addLit (" ++ show a ++ ")"

instance Mul P a where
  type PreMul P a = a
  mul_ = pureP "mul"

instance Show a => MulLit P a where
  mulLit_ a = pureP $ "mulLit (" ++ show a ++ ")"

instance Div2 P (Cyc t m (ZqBasic ('PP '(Prime2, k)) i)) where
  type PreDiv2 P (Cyc t m (ZqBasic ('PP '(Prime2, k)) i)) =
    Cyc t m (ZqBasic ('PP '(Prime2, 'S k)) i)
  div2_ = pureP "div2"

instance Div2 P (PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) where
  type PreDiv2 P (PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) =
    PNoiseTag h (Cyc t m (ZqBasic ('PP '(Prime2, 'S k)) i))
  div2_ = pureP "div2"

instance Div2 P (Identity (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) where
  type PreDiv2 P (Identity (Cyc t m (ZqBasic ('PP '(Prime2, k)) i))) =
    Identity (Cyc t m (ZqBasic ('PP '(Prime2, 'S k)) i))
  div2_ = pureP "div2"

instance Div2 P (CT m (ZqBasic ('PP '(Prime2, k)) i) (Cyc t m' zq)) where
  type PreDiv2 P (CT m (ZqBasic ('PP '(Prime2, k)) i) (Cyc t m' zq)) =
    CT m (ZqBasic ('PP '(Prime2, 'S k)) i) (Cyc t m' zq)

  div2_ = pureP "div2"

instance Functor_ P where
  fmap_ = pureP "fmap"

instance Applicative_ P where
  pure_ = pureP "pure"
  ap_   = pureP "ap"

instance Monad_ P where
  bind_ = pureP "bind"

instance MonadReader_ P where
  ask_   = pureP "ask"
  local_ = pureP "local"

instance MonadWriter_ P where
  tell_   = pureP "tell"
  listen_ = pureP "listen"

instance SHE P where

  type ModSwitchPTCtx   P a zp' = ()
  type ModSwitchCtx     P a zq' = ()
  type AddPublicCtx     P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type MulPublicCtx     P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type KeySwitchQuadCtx P a gad = ()
  type TunnelCtx        P t e r s e' r' s' zp zq gad = ()

  modSwitchPT_     = pureP   "modSwitchPT"
  modSwitch_       = pureP   "modSwitch"
  addPublic_     p = pureP $ "addPublic (" ++ show p ++ ")"
  mulPublic_     p = pureP $ "mulPublic (" ++ show p ++ ")"
  keySwitchQuad_ _ = pureP   "keySwitchQuad <HINT>"
  tunnel_        _ = pureP   "tunnel <HINT>"

instance LinearCyc P rep where
  type PreLinearCyc P rep = rep
  type LinearCycCtx P rep t e r s zp = ()

  linearCyc_ _ = pureP "linearCyc <FUNC>"

instance ErrorRate P where
  type ErrorRateCtx P ct z = ()
  errorRate_ _ = pureP "errorRate"
