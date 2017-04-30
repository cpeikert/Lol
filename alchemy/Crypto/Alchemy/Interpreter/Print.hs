{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Alchemy.Interpreter.Print
( P, pprint
)
where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

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
  lam f  = P $ \i -> "(\v" ++ show  i ++ " -> " ++ unP f (i+1) ++ ")"
  f $: a = P $ \i -> "("   ++ unP f i ++ " "    ++ unP a i     ++ ")"

instance Add P a where
  a +: b = P $ \i -> "(" ++ unP a i ++ " + " ++ unP b i ++ ")"

instance Show a => AddLit P a where
  addLit a b = P $ \i -> "(addLit " ++ show a ++ " " ++ unP b i ++ ")"

instance Mul P a where
  type PreMul P a = a
  a *: b = P $ \i -> "(" ++ unP a i ++ " * " ++ unP b i ++ ")"

instance Show a => MulLit P a where
  mulLit a b = P $ \i -> "(mulLit " ++ show a ++ " " ++ unP b i ++ ")"
