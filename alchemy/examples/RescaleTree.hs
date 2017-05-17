{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module RescaleTree where

import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Lol
import Crypto.Lol.Types

type Zq' (q :: k) = ZqBasic q Int64

rescale4to2 :: forall t m m2 ct2 ct4 m4 expr env .
  (ct2 ~ m2 (Cyc t m (Zq' PP2)), ct4 ~ m4 (Cyc t m (Zq' PP4)),
   PreMul expr (PreDiv2 expr ct2) ~ ct4,
   Ring ct4, Div2 expr ct2, Lambda expr,
   AddLit expr ct4, Mul expr (PreDiv2 expr ct2))
  => expr env (ct4 -> ct2)
rescale4to2 = lam $ div2_ $: v0 *: (one >+: v0)