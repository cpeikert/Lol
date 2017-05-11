{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Language.MapCRTSlots where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.ZqPow2

import Crypto.Lol
import Crypto.Lol.Types (ZqBasic)

type family Zq2Cyc t m a where
  Zq2Cyc t m (ZqBasic q i) = Cyc t m (ZqBasic q i)
  Zq2Cyc t m (a -> b) = Zq2Cyc t m a -> Zq2Cyc t m b
  Zq2Cyc t m (f a) = f (Zq2Cyc t m a) -- covers PNoise h, Maybe, ...

  -- CJP: TODO include an error case to make things break when they should

-- inverse of the above
type family Cyc2Zq cyc where
  Cyc2Zq (Cyc t m zq) = zq
  Cyc2Zq (a -> b) = Cyc2Zq a -> Cyc2Zq b
  Cyc2Zq (f a) = f (Cyc2Zq a)
  -- CJP: TODO error here

newtype MapCRTSlots expr t m e a = M { unM :: expr e (Zq2Cyc t m a) }

mapCRTSlots :: MapCRTSlots expr t m e a -> expr e (Zq2Cyc t m a)
mapCRTSlots = unM

-- instances: DivZq, Add, Mul, AddLit, MulLit, Lambda, ... but NOT
-- RescaleZqPow2!

instance (Add expr (Zq2Cyc t m a)) => Add (MapCRTSlots expr t m) a where
  add_ = M add_
  neg_ = M neg_

instance (Mul expr (Zq2Cyc t m a),
          -- Zq2Cyc, Cyc2Zq are inverses in this scenario
          Zq2Cyc t m (Cyc2Zq (PreMul expr (Zq2Cyc t m a))) ~
          PreMul expr (Zq2Cyc t m a)) =>
  Mul (MapCRTSlots expr t m) a where

  type PreMul (MapCRTSlots expr t m) a = Cyc2Zq (PreMul expr (Zq2Cyc t m a))

  mul_ = M mul_
