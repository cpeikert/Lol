{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Interpreter.Params
( Params, params ) where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
--import Crypto.Alchemy.Language.List
--import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.LinearCyc

import Crypto.Alchemy.Interpreter.PT2CT.Noise

import Crypto.Lol                      (Cyc, Linear)
import Crypto.Lol.Applications.SymmSHE (CT, KSQuadCircHint, TunnelHint)
import Crypto.Lol.Utils.ShowType

import Data.Type.Natural

-- the Int is the nesting depth of lambdas outside the expression
newtype Params (expr :: * -> * -> *) e a = P String

params :: expr () a -> Params expr () a -> String
params _ (P str) = str

-- | In the printout, variable indices grow "outside in," rather than
-- "inside out" (as in object-language code) because the
-- implementation is simpler that way.

instance Lambda (Params expr) where
  lam (P f) = P f
  (P f) $: (P a) = P $ f ++ "\n" ++ a
  v0 = P ""
  s (P v) = P v
{-
instance List P where
  nil_  = pureP "nil"
  cons_ = pureP "cons"
-}
showCT :: forall zq e a expr . (Show (ArgType zq)) => String -> Params expr e a
showCT str = P $ str ++ " " ++ showType (Proxy::Proxy zq)

showPNoise :: forall h e a expr . (SingI (h :: Nat)) => String -> Params expr e a
showPNoise str = P $ str ++ " " ++ show (sNatToInt $ (sing :: SNat h) :: Int)

instance (Show (ArgType zq)) => Add (Params expr) (CT m zp (Cyc t m' zq)) where
  add_ = showCT @zq "add"
  neg_ = showCT @zq "neg"

instance (SingI (h :: Nat)) => Add (Params expr) (PNoise h a) where
  add_ = showPNoise @h "add"
  neg_ = showPNoise @h "neg"
{-
instance Show a => AddLit P a where
  addLit_ a = pureP $ "addLit (" ++ show a ++ ")"
-}
instance (Show (ArgType zq)) => Mul (Params expr) (CT m zp (Cyc t m' zq)) where
  type PreMul (Params expr) (CT m zp (Cyc t m' zq)) = (CT m zp (Cyc t m' zq))
  mul_ = showCT @zq "mul"

instance (SingI (h :: Nat)) => Mul (Params expr) (PNoise h a) where
  type PreMul (Params expr) (PNoise h a) = PreMul expr (PNoise h a)
  mul_ = showPNoise @h "mul"
{-
instance Show a => MulLit P a where
  mulLit_ a = pureP $ "mulLit (" ++ show a ++ ")"

instance Div2 P a where
  type PreDiv2 P a = a

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
-}
instance SHE (Params expr) where

  type ModSwitchPTCtx   (Params expr) (CT m zp (Cyc t m' zq)) zp' = ()
  type ModSwitchCtx     (Params expr) (CT m zp (Cyc t m' zq)) zq' = (Show (ArgType zq), Show (ArgType zq'))
  type AddPublicCtx     (Params expr) (CT m zp (Cyc t m' zq)) = ()
  type MulPublicCtx     (Params expr) (CT m zp (Cyc t m' zq)) = ()
  type KeySwitchQuadCtx (Params expr) (CT m zp (Cyc t m' zq)) gad = (Show (ArgType zq))
  type TunnelCtx        (Params expr) t e r s e' r' s' zp zq gad = (Show (ArgType zq))

  --modSwitchPT_     = pureP   "modSwitchPT"
  modSwitch_ :: forall ct zq' m zp t m' zq env .
    (ModSwitchCtx (Params expr) ct zq', ct ~ CT m zp (Cyc t m' zq))
    => Params expr env (ct -> CT m zp (Cyc t m' zq'))
  modSwitch_       = showCT @zq' $ "modSwitch " ++ showType (Proxy::Proxy zq) ++ " ->"

  --addPublic_     p = pureP $ "addPublic (" ++ show p ++ ")"
  --mulPublic_     p = pureP $ "mulPublic (" ++ show p ++ ")"

  keySwitchQuad_ :: forall ct gad m zp t m' zq env .
    (KeySwitchQuadCtx (Params expr) ct gad, ct ~ CT m zp (Cyc t m' zq))
    => KSQuadCircHint gad (Cyc t m' zq) -> Params expr env (ct -> ct)
  keySwitchQuad_ _ = showCT @zq "keySwitchQuad"

  tunnel_ :: forall t e r s e' r' s' zp zq gad env .
    (TunnelCtx (Params expr) t e r s e' r' s' zp zq gad)
    => TunnelHint gad t e r s e' r' s' zp zq
       -> Params expr env (CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq))
  tunnel_ _ = showCT @zq "tunnel"

instance (SingI (h :: Nat)) => LinearCyc (Params expr) (PNoise h) where
  type PreLinearCyc (Params expr) (PNoise h) = PreLinearCyc expr (PNoise h)
  type LinearCycCtx (Params expr) (PNoise h) t e r s zp = ()

  linearCyc_ :: forall t e r s zp env . (LinearCycCtx (Params expr) (PNoise h) t e r s zp)
    => Linear t zp e r s
    -> Params expr env ((PreLinearCyc (Params expr) (PNoise h)) (Cyc t r zp) -> PNoise h (Cyc t s zp))
  linearCyc_ _ = showPNoise @h "linear"
{-
instance ErrorRate P where
  type ErrorRateCtx P ct z = ()
  errorRate_ _ = pureP "errorRate"
-}