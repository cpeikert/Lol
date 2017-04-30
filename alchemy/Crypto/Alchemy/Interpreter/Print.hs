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

import Crypto.Lol                      (Cyc)
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
  f $: a = P $ \i -> "("    ++ unP f i ++ " "    ++ unP a i     ++ ")"

instance Add P a where
  a +: b = P $ \i -> "(" ++ unP a i ++ " + " ++ unP b i ++ ")"
  negate' a = P $ \i -> "(negate " ++ unP a i ++ " )"

instance Show a => AddLit P a where
  addLit p a = P $ \i -> "(addLit (" ++ show p ++ ") " ++ unP a i ++ ")"

instance Mul P a where
  type PreMul P a = a
  a *: b = P $ \i -> "(" ++ unP a i ++ " * " ++ unP b i ++ ")"

instance Show a => MulLit P a where
  mulLit p a = P $ \i -> "(mulLit (" ++ show p ++ ") " ++ unP a i ++ ")"

instance SHE P where

  type ModSwitchPTCtx   P a zp' = ()
  type RescaleLinearCtx P a zq' = ()
  type AddPublicCtx     P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type MulPublicCtx     P (CT m zp (Cyc t m' zq)) = (Show (Cyc t m zp))
  type KeySwitchQuadCtx P a zq' gad = ()
  type TunnelCtx        P t e r s e' r' s' zp zq gad = ()

  modSwitchPT     a = P $ \i -> "(modSwitchPT "   ++ unP a i ++ ")"
  rescaleLinear   a = P $ \i -> "(rescaleLinear " ++ unP a i ++ ")"
  addPublic     p a = P $ \i -> "(addPublic (" ++ show p ++ ") " ++ unP a i ++ ")"
  mulPublic     p a = P $ \i -> "(mulPublic (" ++ show p ++ ") " ++ unP a i ++ ")"
  keySwitchQuad _ a = P $ \i -> "(keySwitchQuad <HINT> " ++ unP a i ++ ")"
  tunnel        _ a = P $ \i -> "(tunnel <FUNC> " ++ unP a i ++ ")"
