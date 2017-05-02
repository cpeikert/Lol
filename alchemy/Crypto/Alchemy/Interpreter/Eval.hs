{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.Eval ( E, eval ) where

import Control.Applicative
import Control.Monad.State
import Data.Tuple

import Algebra.Additive as Additive
import Algebra.Ring     as Ring

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE

import           Crypto.Lol
import           Crypto.Lol.Applications.SymmSHE (CT, ToSDCtx)
import qualified Crypto.Lol.Applications.SymmSHE as SHE

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

instance (Additive.C a) => Add E a where
  add_ = E $ pure (+)
  neg_ = E $ pure negate

instance (Additive.C a) => AddLit E a where
  x >+: y = (x +) <$> y

instance (Ring.C a) => Mul E a where
  type PreMul E a = a
  mul_ = E $ pure (*)

instance (Ring.C a) => MulLit E a where
  x >*: y = (x *) <$> y

instance Functor_ E where
  fmap_ = E $ pure fmap

instance Applicative_ E where
  pure_ = E $ pure pure
  ap_   = E $ pure (<*>)

instance Monad_ E where
  bind_ = E $ pure (>>=)

instance SHE E where

  type ModSwitchPTCtx   E (CT m zp (Cyc t m' zq)) zp'     = (SHE.ModSwitchPTCtx t m' zp zp' zq)
  type RescaleLinearCtx E (CT m zp (Cyc t m' zq)) zq'     = (RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq')
  type AddPublicCtx     E (CT m zp (Cyc t m' zq))         = (SHE.AddPublicCtx t m m' zp zq)
  type MulPublicCtx     E (CT m zp (Cyc t m' zq))         = (SHE.MulPublicCtx t m m' zp zq)
  type KeySwitchQuadCtx E (CT m zp (Cyc t m' zq)) zq' gad = (SHE.KeySwitchCtx gad t m' zp zq zq')
  type TunnelCtx        E t e r s e' r' s' zp zq gad      = (SHE.TunnelCtx t r s e' r' s' zp zq gad)

  modSwitchPT     = fmap   SHE.modSwitchPT
  rescaleLinear   = fmap   SHE.rescaleLinearCT
  addPublic       = fmap . SHE.addPublic
  mulPublic       = fmap . SHE.mulPublic
  keySwitchQuad   = fmap . SHE.keySwitchQuadCirc
  tunnel          = fmap . SHE.tunnelCT
