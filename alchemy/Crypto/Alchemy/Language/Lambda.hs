{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Alchemy.Language.Lambda where

-- | Symantics for functions and application.

class Lambda expr where
  -- | Lambda abstraction.
  lam :: expr (e,a) b -> expr e (a -> b)

  -- | Application.
  infixl 1 $:             -- ($) is infixr, but l is nicer for obj lang
  ($:) :: expr e (a -> b) -> expr e a -> expr e b

  -- | The zero'th (most-recently bound) variable.
  v0 :: expr (b,a) a

  -- | Extend environment.
  s  :: expr e a -> expr (e,x) a

-- | Let-sharing.
let_ :: Lambda expr => expr e a -> expr (e,a) b -> expr e b
let_ a f = lam f $: a

-- | Composition.
infixr 9 .:
(.:) :: (Lambda expr) => expr e (b -> c) -> expr e (a -> b) -> expr e (a -> c)
f .: g = lam (s f $: (s g $: v0))


-- CJP: for some reason have to give signature here, even though ghci
-- infers them correctly

-- | The one'th (second-most-recently bound) variable.
v1 :: Lambda expr => expr ((c,b),a) b
v1 = s v0

v2 :: Lambda expr => expr (((d,c),b),a) c
v2 = s v1

v3 :: Lambda expr => expr ((((e,d),c),b),a) d
v3 = s v2

v4 :: Lambda expr => expr (((((f,e),d),c),b),a) e
v4 = s v3

class Embed env' env where
  embedExpr :: (Lambda expr) => expr env' a -> expr env a

instance Embed e e where
  embedExpr = id

instance (Embed e1 e2) => Embed e1 (e2,x) where
  embedExpr = s . embedExpr