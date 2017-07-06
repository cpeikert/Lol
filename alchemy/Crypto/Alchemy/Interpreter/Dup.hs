{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Interpreter.Dup (Dup, dup) where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.LinearCyc
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.Pair
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.String

import Prelude hiding (String)

dup :: Dup expr1 expr2 e a -> (expr1 e a, expr2 e a)
dup (Dup a b) = (a,b)

data Dup expr1 expr2 e a = Dup (expr1 e a) (expr2 e a)

instance (Lambda ex1, Lambda ex2) => Lambda (Dup ex1 ex2) where
  lam (Dup f1 f2) = Dup (lam f1) (lam f2)
  (Dup f1 f2) $: (Dup a1 a2) = Dup (f1 $: a1) (f2 $: a2)
  v0 = Dup v0 v0
  s (Dup a1 a2) = Dup (s a1) (s a2)

instance (Add ex1 a, Add ex2 a) => Add (Dup ex1 ex2) a where
  add_ = Dup add_ add_
  neg_ = Dup neg_ neg_

instance (Mul ex1 a, Mul ex2 a, PreMul ex1 a ~ PreMul ex2 a) =>
  Mul (Dup ex1 ex2) a where

  type PreMul (Dup ex1 ex2) a = PreMul ex1 a
  mul_ = Dup mul_ mul_

instance (AddLit ex1 a, AddLit ex2 a) => AddLit (Dup ex1 ex2) a where
  addLit_ a = Dup (addLit_ a) (addLit_ a)

instance (MulLit ex1 a, MulLit ex2 a) => MulLit (Dup ex1 ex2) a where
  mulLit_ a = Dup (mulLit_ a) (mulLit_ a)

instance (Div2 ex1 a, Div2 ex2 a, PreDiv2 ex1 a ~ PreDiv2 ex2 a)
  => Div2 (Dup ex1 ex2) a where
  type PreDiv2 (Dup ex1 ex2) a = PreDiv2 ex1 a
  div2_ = Dup div2_ div2_

instance (SHE ex1, SHE ex2) => SHE (Dup ex1 ex2) where
  type ModSwitchPTCtx (Dup ex1 ex2) ct zp' = (ModSwitchPTCtx ex1 ct zp',
                                              ModSwitchPTCtx ex2 ct zp')
  type ModSwitchCtx (Dup ex1 ex2) ct zq' = (ModSwitchCtx ex1 ct zq',
                                            ModSwitchCtx ex2 ct zq')
  type AddPublicCtx (Dup ex1 ex2) ct = (AddPublicCtx ex1 ct,
                                        AddPublicCtx ex2 ct)
  type MulPublicCtx (Dup ex1 ex2) ct = (MulPublicCtx ex1 ct,
                                        MulPublicCtx ex2 ct)
  type KeySwitchQuadCtx (Dup ex1 ex2) ct gad = (KeySwitchQuadCtx ex1 ct gad,
                                                KeySwitchQuadCtx ex2 ct gad)
  type TunnelCtx    (Dup ex1 ex2) t e r s e' r' s' zp zq gad =
    (TunnelCtx ex1 t e r s e' r' s' zp zq gad,
     TunnelCtx ex2 t e r s e' r' s' zp zq gad)

  modSwitchPT_     = Dup  modSwitchPT_       modSwitchPT_
  modSwitch_       = Dup  modSwitch_         modSwitch_
  addPublic_     p = Dup (addPublic_ p)     (addPublic_ p)
  mulPublic_     p = Dup (mulPublic_ p)     (mulPublic_ p)
  keySwitchQuad_ h = Dup (keySwitchQuad_ h) (keySwitchQuad_ h)
  tunnel_        h = Dup (tunnel_ h)        (tunnel_ h)

instance (LinearCyc ex1 rep, LinearCyc ex2 rep,
          PreLinearCyc ex1 rep ~ PreLinearCyc ex2 rep)
  => LinearCyc (Dup ex1 ex2) rep where
  type PreLinearCyc (Dup ex1 ex2) rep = PreLinearCyc ex1 rep

  type LinearCycCtx (Dup ex1 ex2) rep t e r s zp =
    (LinearCycCtx ex1 rep t e r s zp, LinearCycCtx ex2 rep t e r s zp)

  linearCyc_ f = Dup (linearCyc_ f) (linearCyc_ f)

instance (List ex1, List ex2) => List (Dup ex1 ex2) where
  nil_  = Dup nil_ nil_
  cons_ = Dup cons_ cons_

instance (Functor_ ex1, Functor_ ex2) => Functor_ (Dup ex1 ex2) where
  fmap_ = Dup fmap_ fmap_

instance (Applicative_ ex1, Applicative_ ex2) => Applicative_ (Dup ex1 ex2) where
  pure_ = Dup pure_ pure_
  ap_   = Dup ap_ ap_

instance (Monad_ ex1, Monad_ ex2) => Monad_ (Dup ex1 ex2) where
  bind_ = Dup bind_ bind_

instance (MonadReader_ ex1, MonadReader_ ex2) => MonadReader_ (Dup ex1 ex2) where
  ask_   = Dup ask_ ask_
  local_ = Dup local_ local_

instance (MonadWriter_ ex1, MonadWriter_ ex2) => MonadWriter_ (Dup ex1 ex2) where
  tell_   = Dup tell_ tell_
  listen_ = Dup listen_ listen_

instance (String ex1, String ex2) => String (Dup ex1 ex2) where
  string_ str = Dup (string_ str) (string_ str)

instance (Pair ex1, Pair ex2) => Pair (Dup ex1 ex2) where
  pair_ = Dup pair_ pair_

instance (ErrorRate ex1, ErrorRate ex2) => ErrorRate (Dup ex1 ex2) where

  type ErrorRateCtx (Dup ex1 ex2) ct z = (ErrorRateCtx ex1 ct z,
                                          ErrorRateCtx ex2 ct z)

  errorRate_ sk = Dup (errorRate_ sk) (errorRate_ sk)

{-

-- OLD AND PROBABLY BUSTED ATTEMPT AT ALLOWING DIFFERENT REP TYPES

data Dup ex e a where
  Dup :: ex1 (Unzip1 e) (Fst a)
      -> ex2 (Unzip2 e) (Snd a)
      -> Dup '(ex1,ex2) e a

type family Fst a where
  Fst (a1,a2)  = a1
  Fst (a -> b) = Fst a -> Fst b

type family Snd a where
  Snd (a1,a2)  = a2
  Snd (a -> b) = Snd a -> Snd b

type family Unzip1 e where  -- use on, e.g., (((),(b1,b2)),(a1,a2))
  Unzip1 ()    = ()         -- weird-ish base case, but ((),()) doesn't work
  Unzip1 (e,a) = (Unzip1 e, Fst a)

type family Unzip2 e where
  Unzip2 ()    = ()
  Unzip2 (e,a) = (Unzip2 e, Snd a)

dup :: Dup '(ex1,ex2) e a
    -> (ex1 (Unzip1 e) (Fst a), ex2 (Unzip2 e) (Snd a))
dup (Dup a1 a2) = (a1,a2)

instance (Lambda ex1, Lambda ex2) => Lambda (Dup '(ex1,ex2)) where
  lam (Dup f1 f2) = Dup (lam f1) (lam f2)
  (Dup f1 f2) $: (Dup a1 a2) = Dup (f1 $: a1) (f2 $: a2)

instance (DB ex1 (Fst a), DB ex2 (Snd a)) => DB (Dup '(ex1,ex2)) a where
  v0 = Dup v0 v0
  s (Dup a1 a2) = Dup (s a1) (s a2)

instance (Add ex1 a1, Add ex2 a2) => Add (Dup '(ex1,ex2)) (a1,a2) where
  (Dup a1 a2) +: (Dup b1 b2) = Dup (a1 +: b1) (a2 +: b2)

instance (Mul ex1 a1, Mul ex2 a2) => Mul (Dup '(ex1,ex2)) (a1,a2) where
  type PreMul (Dup '(ex1,ex2)) (a1,a2) = (PreMul ex1 a1, PreMul ex2 a2)
  (Dup a1 a2) *: (Dup b1 b2) = Dup (a1 *: b1) (a2 *: b2)

instance (Lit ex1 a1, Lit ex2 a2) => Lit (Dup '(ex1, ex2)) (a1, a2) where
  lit (a1,a2) = Dup (lit a1) (lit a2)
-}
