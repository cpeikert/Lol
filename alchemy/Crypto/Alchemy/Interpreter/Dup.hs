{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Interpreter.Dup (Dup, dup) where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE

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

instance (SHE ex1, SHE ex2) => SHE (Dup ex1 ex2) where
  type ModSwitchPTCtx (Dup ex1 ex2) ct zp' = (ModSwitchPTCtx ex1 ct zp',
                                              ModSwitchPTCtx ex2 ct zp')
  type RescaleLinearCtx (Dup ex1 ex2) ct zq' = (RescaleLinearCtx ex1 ct zq',
                                                RescaleLinearCtx ex2 ct zq')
  type AddPublicCtx (Dup ex1 ex2) ct = (AddPublicCtx ex1 ct, AddPublicCtx ex2 ct)
  type MulPublicCtx (Dup ex1 ex2) ct = (MulPublicCtx ex1 ct, MulPublicCtx ex2 ct)
  type KeySwitchQuadCtx (Dup ex1 ex2) ct zq' gad = (KeySwitchQuadCtx ex1 ct zq' gad,
                                                    KeySwitchQuadCtx ex2 ct zq' gad)
  type TunnelCtx    (Dup ex1 ex2) t e r s e' r' s' zp zq gad =
    (TunnelCtx ex1 t e r s e' r' s' zp zq gad,
     TunnelCtx ex2 t e r s e' r' s' zp zq gad)

  modSwitchPT (Dup a b) = Dup (modSwitchPT a) (modSwitchPT b)

  rescaleLinear (Dup a b) = Dup (rescaleLinear a) (rescaleLinear b)

  addPublic p (Dup a b) = Dup (addPublic p a) (addPublic p b)

  mulPublic p (Dup a b) = Dup (mulPublic p a) (mulPublic p b)

  keySwitchQuad h (Dup a b) = Dup (keySwitchQuad h a) (keySwitchQuad h b)

  tunnel f (Dup a b) = Dup (tunnel f a) (tunnel f b)



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
