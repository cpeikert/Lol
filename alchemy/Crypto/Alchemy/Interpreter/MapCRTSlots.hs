{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Language.MapCRTSlots where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.ZqPow2

import Crypto.Lol
import Crypto.Lol.Types (ZqBasic)

type family Zq2Cyc t m a where
  Zq2Cyc t m (ZqBasic q i) = Cyc t m (ZqBasic q i)
  Zq2Cyc t m (a -> b) = Zq2Cyc t m a -> Zq2Cyc t m b
  Zq2Cyc t m (a, b) = (Zq2Cyc t m a, Zq2Cyc t m b)
  Zq2Cyc t m (f a) = f (Zq2Cyc t m a) -- covers PNoise h, Maybe, ...
  -- CJP: TODO include an error case to make things break when they should

-- inverse of the above
type family Cyc2Zq cyc where
  Cyc2Zq (Cyc t m zq) = zq
  Cyc2Zq (a -> b) = Cyc2Zq a -> Cyc2Zq b
  Cyc2Zq (a, b) = (Cyc2Zq a, Cyc2Zq b)
  Cyc2Zq (f a) = f (Cyc2Zq a)   -- covers PNoise h, Maybe, ...
  -- CJP: TODO error here

newtype MapCRTSlots expr t m e a =
  M { unM :: expr (Zq2Cyc t m e) (Zq2Cyc t m a) }

mapCRTSlots :: MapCRTSlots expr t m e a -> expr (Zq2Cyc t m e) (Zq2Cyc t m a)
mapCRTSlots = unM

-- instances: DivZq, ... but NOT RescaleZqPow2!

instance Lambda expr => Lambda (MapCRTSlots expr t m) where
  lam    = M . lam . unM
  f $: a = M $ unM f $: unM a
  v0     = M v0
  s      = M . s . unM

instance (Add expr (Zq2Cyc t m a)) => Add (MapCRTSlots expr t m) a where
  add_ = M add_
  neg_ = M neg_

instance (Mul expr (Zq2Cyc t m a),
          -- need Zq2Cyc, Cyc2Zq to be inverses in this scenario
          Zq2Cyc t m (Cyc2Zq (PreMul expr (Zq2Cyc t m a))) ~
          PreMul expr (Zq2Cyc t m a)) =>
  Mul (MapCRTSlots expr t m) a where

  type PreMul (MapCRTSlots expr t m) a = Cyc2Zq (PreMul expr (Zq2Cyc t m a))

  mul_ = M mul_

instance (zq ~ ZqBasic q i, AddLit expr (Cyc t m zq)) =>
  AddLit (MapCRTSlots expr t m) zq where
  a >+: (M b) = M $ scalarCyc a >+: b

instance (zq ~ ZqBasic q i, MulLit expr (Cyc t m zq)) =>
  MulLit (MapCRTSlots expr t m) zq where
  a >*: (M b) = M $ scalarCyc a >*: b

instance (Functor f, Zq2Cyc t m (f zq) ~ f (Cyc t m zq),
          AddLit expr (f (Cyc t m zq))) =>
  AddLit (MapCRTSlots expr t m) (f zq) where
  a >+: (M b) = M $ (scalarCyc <$> a) >+: b

instance (Functor f, Zq2Cyc t m (f zq) ~ f (Cyc t m zq),
          MulLit expr (f (Cyc t m zq))) =>
  MulLit (MapCRTSlots expr t m) (f zq) where
  a >*: (M b) = M $ (scalarCyc <$> a) >*: b

instance Functor_ expr => Functor_ (MapCRTSlots expr t m) where
  fmap_ :: MapCRTSlots expr t m e ((a -> b) -> f a -> f b)
  fmap_ = M $ (fmap_ :: expr _ ((_ -> _) -> f _ -> f _))
