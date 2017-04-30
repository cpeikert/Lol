{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Interpreter.Print
( P, pprint
)
where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE

import Crypto.Lol (Cyc)
import Crypto.Lol.Applications.SymmSHE (CT)

-- the Int is the nesting depth of lambdas outside the expression
newtype P e a = P { unP :: Int -> String }

-- | Pretty-print a closed expression.
pprint :: P () a -> String
pprint = flip unP 0

-- CJP: variable indices increase ("outside in"), rather than "inside
-- out" (the object-language form) because it is a simpler
-- implementation.

instance DB P a where
  v0   = P $ \i -> "v" ++ show (i-1)
  s  v = P $ \i -> unP v (i-1)

instance Lambda P where
  lam f  = P $ \i -> "(\\v" ++ show  i ++ " -> " ++ unP f (i+1) ++ ")"
  f $: a = P $ \i -> "("   ++ unP f i ++ " "    ++ unP a i     ++ ")"

instance Add P a where
  a +: b = P $ \i -> "( " ++ unP a i ++ " )" ++ " + " ++ "( " ++ unP b i ++ " )"
  negate' a = P $ \i -> "negate ( " ++ unP a i ++ " )"

instance Show a => AddLit P a where
  addLit a b = P $ \i -> "addLit ( " ++ show a ++ " ) ( " ++ unP b i ++ " )"

instance Mul P a where
  type PreMul P a = a
  a *: b = P $ \i -> "( " ++ unP a i ++ " )" ++ " * " ++ "( " ++ unP b i ++ " )"

instance Show a => MulLit P a where
  mulLit a b = P $ \i -> "mulLit ( " ++ show a ++ " ) ( " ++ unP b i ++ " )"

instance SHE P where

  type ModSwitchCtx P a zp' = ()
  type RescaleCtx   P a zq' = ()
  type AddPubCtx    P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type MulPubCtx    P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type KeySwitchCtx P a zq' gad = ()
  type TunnelCtx    P t e r s e' r' s' zp zq gad = ()

  modSwitchPT (P a) = P $ \i -> "modSwitch $ " ++ a i
  rescaleLinearCT (P a) = P $ \i -> "rescale $ " ++ a i
  addPublic a (P b) = P $ \i -> "( " ++ show a ++ " )" ++ " + " ++ "( " ++ b i ++ " )"
  mulPublic a (P b) = P $ \i -> "( " ++ show a ++ " )" ++ " * " ++ "( " ++ b i ++ " )"
  keySwitchQuad _ (P a) = P $ \i -> "keySwitch <HINT> $ " ++ a i
  tunnel _ (P a) = P $ \i -> "tunnel <FUNC> $ " ++ a i