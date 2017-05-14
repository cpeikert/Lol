{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

module Crypto.Alchemy.Interpreter.RescaleTree
( rescaleTreeZqPow2_, rescaleTreeCRT_, rescaleTreeCRT, RescaleCycCRTCtx)
where

import Crypto.Alchemy.Interpreter.MapCRTSlots

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

import Crypto.Lol
-- EAC: shouldn't have to import this
-- CJP: needed for the Reflects instance for Pos and use of 'value', right?
import Crypto.Lol.Reflects
import Crypto.Lol.Types

import Control.Applicative

-- | Constraint synonym for rescaling the CRT slots of a 'Cyc'.
type RescaleTreeCRTCtx env t m expr k z2 cyc2 cyc2k =
  (RescaleTreeCRTCtx' expr t m k z2 cyc2 cyc2k,
   Lambda expr)
-- CJP: left out Weaken until we can resolve other type errors


type RescaleTreeCRTCtx' expr t m k z2 cyc2 cyc2k =
  (cyc2k ~ Zq2Cyc t m (PreRescaleTreeZqPow2 expr k z2), cyc2 ~ Zq2Cyc t m z2)

-- | Convenience function: using the (arithmetic) rescaling tree,
-- rescale each \( \Z_{2^k} \) value in the mod-\( 2^k \) CRT slots of
-- \( R_{2^k} \) down to \( \Z_2 \), with the result in \( R_2 \).
-- (If the values in the slots are not restricted to \( \Z_{2^k} \),
-- the behavior is undefined.)
rescaleTreeCRT_ :: forall e t m k z2 cyc2 cyc2k expr .
  (RescaleTreeCRTCtx e t m expr k z2 cyc2 cyc2k) =>
  -- CJP: specialized to empty env until we can resolve other type errors
  Tagged '(t,m,k) (expr () (_ -> _)) 
  -- Zq2Cyc t m (PreRescaleTreeZqPow2 expr k z2) -> Zq2Cyc t m z2))
rescaleTreeCRT_ =  pure $
  mapCRTSlots @t @m $
  proxy (rescaleTreeZqPow2_ :: Tagged k ((MapCRTSlots expr t m) () (PreRescaleTreeZqPow2 expr k z2 -> z2)))
  (Proxy::Proxy k)

rescaleTreeCRT :: (RescaleTreeCRTCtx e t m expr k z2 cyc2 cyc2k)
  => Tagged '(t,m,k) (expr () cyc2k -> expr () (Zq2Cyc t m z2))
rescaleTreeCRT = ($:) <$> rescaleTreeCRT_


type RescaleTreeCtx expr k z2 =
  RescaleTreeCtx' expr k z2 (PreRescaleTreeZqPow2 expr k z2)

type RescaleTreeCtx' expr k z2 z2k =
  (Div2 expr z2k, RescaleTreeCtx'' expr k z2 (PreDiv2 expr z2k))

type RescaleTreeCtx'' expr k z2 prediv =
  (Lambda expr, AddLit expr prediv,
   Internal expr k z2, Reflects k Int, Mul expr prediv,
   Ring (PreMul expr prediv), Ring prediv, AddLit expr (PreMul expr prediv))

type family PreRescaleTreeZqPow2 expr k z2 where
  PreRescaleTreeZqPow2 expr 'O     z2 = z2
  PreRescaleTreeZqPow2 expr ('S k) z2 =
    PreMul expr (PreDiv2 expr (PreRescaleTreeZqPow2 expr k z2))

rescaleTreeZqPow2_ :: forall k z2 expr e .
  -- CJP: check for fencepost errors on k in the constraints
  (RescaleTreeCtx expr k z2)
  => Tagged ('S k) (expr e (PreRescaleTreeZqPow2 expr ('S k) z2 -> z2))
rescaleTreeZqPow2_ = pure $ lam $
    let v'    = v0 *: (one >+: v0)
        kval  = proxy value (Proxy::Proxy k) :: Int
        pDiv4 = 2^(kval-1)
    in internal (Proxy::Proxy k) $ take pDiv4 $
         map ((div2_ $:) . (>+: v')) [fromInteger $ y * (-y+1) | y <- [1..]]

class Internal expr (k :: Pos) z2 where
  internal :: Proxy k
           -> [expr env (PreRescaleTreeZqPow2 expr k z2)]
           -> expr env z2

instance Internal expr 'O z2 where
  internal _ [x] = x
  internal _ _   = error "Internal error in RescaleTreeZqPow2 (internal) base case."

instance (Internal expr k z2, Lambda expr,
          Div2 expr (PreRescaleTreeZqPow2 expr k z2),
          Mul expr (PreDiv2 expr (PreRescaleTreeZqPow2 expr k z2)))
  => Internal expr ('S k) z2 where
  internal _ = internal (Proxy::Proxy k) . map ((div2_ $:) . uncurry (*:)) . listToPairs

listToPairs :: [a] -> [(a,a)]
listToPairs []       = []
listToPairs (a:b:xs) = (a,b) : listToPairs xs
listToPairs _        = error "listToPairs internal error: odd number of elements"
