{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.Eval ( E, eval ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Tuple

import Algebra.Additive as Additive
import Algebra.Ring     as Ring

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE as SHE

-- | Metacircular evaluator.
newtype E e a = E { unE :: e -> a }
  deriving (Functor)            -- not Applicative; don't want 'pure'!

-- | Evaluate a closed expression (i.e., one not having any unbound
-- variables)
eval :: E () a -> a
eval = flip unE ()

instance Lambda E where
  lam f  = E $ curry $ unE f
  f $: a = E $ unE f <*> unE a
  v0     = E snd
  s a    = E $ unE a . fst

instance Additive.C a => Add E a where
  add_ = E $ pure (+)
  neg_ = E $ pure negate

instance Additive.C a => AddLit E a where
  x >+: y = (x +) <$> y

instance Ring.C a => Mul E a where
  type PreMul E a = a
  mul_ = E $ pure (*)

instance Ring.C a => MulLit E a where
  x >*: y = (x *) <$> y

instance List E where
  nil_  = E $ pure []
  cons_ = E $ pure (:)

instance Functor_ E where
  fmap_ = E $ pure fmap

instance Applicative_ E where
  pure_ = E $ pure pure
  ap_   = E $ pure (<*>)

instance Monad_ E where
  bind_ = E $ pure (>>=)

instance MonadReader_ E where
  ask_   = E $ pure ask
  local_ = E $ pure local

instance MonadWriter_ E where
  tell_   = E $ pure tell
  listen_ = E $ pure listen

instance SHE E where

  type ModSwitchPTCtx   E (CT m zp (Cyc t m' zq)) zp' = (SHE.ModSwitchPTCtx t m' zp zp' zq)
  type RescaleLinearCtx E (CT m zp (Cyc t m' zq)) zq' = (RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq)
  type AddPublicCtx     E (CT m zp (Cyc t m' zq))     = (SHE.AddPublicCtx t m m' zp zq)
  type MulPublicCtx     E (CT m zp (Cyc t m' zq))     = (SHE.MulPublicCtx t m m' zp zq)
  type KeySwitchQuadCtx E (CT m zp (Cyc t m' zq)) gad = (SHE.KeySwitchCtx gad t m' zp zq)
  type TunnelCtx        E t e r s e' r' s' zp zq gad  = (SHE.TunnelCtx t r s e' r' s' zp zq gad)

  modSwitchPT_     = E $ pure   modSwitchPT
  rescaleLinear_   = E $ pure   rescaleLinear
  addPublic_       = E . pure . addPublic
  mulPublic_       = E . pure . mulPublic
  keySwitchQuad_   = E . pure . keySwitchQuadCirc
  tunnel_          = E . pure . tunnel

-- | Uses 'SHE.errorTermUnrestricted' to compute 'errorRate'.
instance ErrorRate E where
  type ErrorRateCtx E (CT m zp (Cyc t m' zq)) z =
    (SHE.ErrorTermUCtx t m' z zp zq, Mod zq, ToInteger (LiftOf zq))

  errorRate_ :: forall t m' m z zp zq ct e .
                (ErrorRateCtx E ct z, ct ~ CT m zp (Cyc t m' zq)) =>
                SHE.SK (Cyc t m' z) -> E e (ct -> Double)
  errorRate_ sk = E $ pure $
    (/ (fromIntegral $ proxy modulus (Proxy::Proxy zq))) .
    fromIntegral . maximum . fmap abs . SHE.errorTermUnrestricted sk
