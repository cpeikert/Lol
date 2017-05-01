{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Alchemy.Language.Lambda where

-- | Symantics for functions and application.

class Lambda expr where
  -- | Abstraction.
  lam  :: expr (e,a) b -> expr e (a -> b)

  -- | Application.
  ($:) :: expr e (a -> b) -> expr e a -> expr e b

-- | Let-sharing.
let_ :: Lambda expr => expr e a -> expr (e,a) b -> expr e b
let_ a f = lam f $: a

-- | Composition.
infixr 9 .:
-- could also drop the DB (a->b), (b->c) and set f,g envs to (e,a)
(.:) :: (Lambda expr, DB expr a, DB expr (a -> b), DB expr (b -> c)) =>
        expr e (b -> c) -> expr e (a -> b) -> expr e (a -> c)
f .: g = lam (s f $: (s g $: v0))

-- | Symantics for de Bruijn variables.

class DB expr a where
  -- | The zero'th (most recently bound) variable.
  v0 :: expr (b,a) a

  -- | The next-most-recently-bound variable from the given one.
  s  :: expr e a -> expr (e,x) a

-- CJP: for some reason have to give signature here, even though ghci
-- infers them correctly

v1 :: DB expr b => expr ((c,b),a) b
v1 = s v0

v2 :: DB expr c => expr (((d,c),b),a) c
v2 = s v1

v3 :: DB expr d => expr ((((e,d),c),b),a) d
v3 = s v2

v4 :: DB expr e => expr (((((f,e),d),c),b),a) e
v4 = s v3
