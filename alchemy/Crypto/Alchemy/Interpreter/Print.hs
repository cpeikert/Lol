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

-- | In the printout, variable indices grow "outside in," rather than
-- "inside out" (as in object-language code) because the
-- implementation is simpler that way.

instance Lambda P where
  lam f  = P $ \i -> "(\\v" ++ show  i ++ " -> " ++ unP f (i+1) ++ ")"
  f $: a = P $ \i -> "("    ++ unP f i ++ " "    ++ unP a i     ++ ")"
  v0     = P $ \i -> "v" ++ show (i-1)
  s  v   = P $ \i -> unP v (i-1)

instance Add P a where
  add = P $ const "add"
  neg = P $ const "neg"

instance Show a => AddLit P a where
  addLit p a = P $ \i -> "(addLit (" ++ show p ++ ") " ++ unP a i ++ ")"

instance Mul P a where
  type PreMul P a = a
  mul = P $ const "mul"

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
