{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

-- | A transform that maps any /arithmetic/ function on \( \Z_q \)
-- values over the mod-\( q \) SIMD (or CRT) "slots" of a cyclotomic
-- ring \( R_q \).  If the contents of the slots are not limited to \(
-- \Z_q \) values, the behavior of the transform is undefined.

module Crypto.Alchemy.Interpreter.MapCRTSlots where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Interpreter.PT2CT.Noise

import Crypto.Lol
import Crypto.Lol.Types (ZqBasic)

import Control.Monad.Identity

type family Zq2Cyc t m a = cyca | cyca -> a where
  Zq2Cyc t m (ZqBasic (q :: k) i) = Cyc t m (ZqBasic q i)
  Zq2Cyc t m (a -> b) = Zq2Cyc t m a -> Zq2Cyc t m b
  Zq2Cyc t m (a, b) = (Zq2Cyc t m a, Zq2Cyc t m b)
  Zq2Cyc t m (PNoise h a) = PNoise h (Zq2Cyc t m a)
  Zq2Cyc t m (Identity a) = Identity (Zq2Cyc t m a)
  Zq2Cyc t m () = ()
  -- CJP: TODO include an error case to make things break when they should

-- inverse of the above
type family Cyc2Zq cyc where
  Cyc2Zq (Cyc t m zq) = zq
  Cyc2Zq (a -> b) = Cyc2Zq a -> Cyc2Zq b
  Cyc2Zq (a, b) = (Cyc2Zq a, Cyc2Zq b)
  Cyc2Zq (PNoise h a) = PNoise h (Cyc2Zq a)
  Cyc2Zq (Identity a) = Identity (Cyc2Zq a)
  -- CJP: TODO error here

newtype MapCRTSlots expr t m e a =
  M { unM :: expr (Zq2Cyc t m e) (Zq2Cyc t m a) }

mapCRTSlots :: forall t m expr e a . -- for Type Applications
  MapCRTSlots expr t m e a -> expr (Zq2Cyc t m e) (Zq2Cyc t m a)
mapCRTSlots = unM

instance Lambda expr => Lambda (MapCRTSlots expr t m) where
  lam    = M . lam . unM
  f $: a = M $ unM f $: unM a
  v0     = M v0
  s      = M . s . unM

instance (Add expr (Zq2Cyc t m a)) => Add (MapCRTSlots expr t m) a where
  add_ = M add_
  neg_ = M neg_

instance (Mul expr (Zq2Cyc t m a),
          -- Zq2Cyc, Cyc2Zq must be inverses in this scenario
          Zq2Cyc t m (Cyc2Zq (PreMul expr (Zq2Cyc t m a))) ~
          PreMul expr (Zq2Cyc t m a)) =>
  Mul (MapCRTSlots expr t m) a where

  type PreMul (MapCRTSlots expr t m) a = Cyc2Zq (PreMul expr (Zq2Cyc t m a))

  mul_ = M mul_

instance (Div2 expr (Zq2Cyc t m a), rq' ~ (PreDiv2 expr (Zq2Cyc t m a)),
          -- Zq2Cyc, Cyc2Zq must be inverses in this scenario
          Zq2Cyc t m (Cyc2Zq rq') ~ rq') => Div2 (MapCRTSlots expr t m) a where

  type PreDiv2 (MapCRTSlots expr t m) a = Cyc2Zq (PreDiv2 expr (Zq2Cyc t m a))

  div2_ = M div2_

instance (AddLit expr (Cyc t m (ZqBasic q i))) =>
  AddLit (MapCRTSlots expr t m) (ZqBasic q i) where
  addLit_ a = M $ addLit_ (scalarCyc a)

instance (MulLit expr (Cyc t m (ZqBasic q i))) =>
  MulLit (MapCRTSlots expr t m) (ZqBasic q i) where
  mulLit_ a = M $ mulLit_ (scalarCyc a)

instance (Functor f, Zq2Cyc t m (f zq) ~ f (Cyc t m zq),
          AddLit expr (f (Cyc t m zq))) =>
  AddLit (MapCRTSlots expr t m) (f zq) where
  addLit_ a = M $ addLit_ (scalarCyc <$> a)

instance (Functor f, Zq2Cyc t m (f zq) ~ f (Cyc t m zq),
          MulLit expr (f (Cyc t m zq))) =>
  MulLit (MapCRTSlots expr t m) (f zq) where
  mulLit_ a = M $ mulLit_ (scalarCyc <$> a)


-- CJP: unclear whether we can instantiate Functor_; should Zq2Cyc be
-- applied to the functor itself?  For functor (ZqB ->) it would seem
-- yes; for functor [] clearly no.
