{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Crypto.Alchemy.Language.LinearCyc where

import Crypto.Alchemy.Language.Lambda
import Crypto.Lol
import GHC.Exts                       (Constraint)

-- | Symantics for evaluating a linear function on cyclotomics.

class LinearCyc expr rep where

  -- | Constraints needed to linear
  type LinearCycCtx
         expr
         (rep :: * -> *)
         (t :: Factored -> * -> *)
         (e :: Factored)
         (r :: Factored)
         (s :: Factored)
         zp :: Constraint

  -- | 'Cyc' wrapper for the input to linearing
  type PreLinearCyc expr rep :: * -> *

  -- | An object-language expression representing the given linear function.
  linearCyc_ :: (LinearCycCtx expr rep t e r s zp)
    => Linear t zp e r s
    -> expr env ((PreLinearCyc expr rep) (Cyc t r zp) -> rep (Cyc t s zp))

linearCyc :: (LinearCyc expr rep, LinearCycCtx expr rep t e r s zp, Lambda expr)
  => Linear t zp e r s
  -> expr env ((PreLinearCyc expr rep) (Cyc t r zp))
  -> expr env (rep (Cyc t s zp))
linearCyc f a = linearCyc_ f $: a
