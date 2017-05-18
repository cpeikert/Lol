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
( rescaleTreePow2_, RescaleTreePow2Ctx, PreRescaleTreePow2 )
where

import Data.Constraint

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

import Crypto.Lol
-- EAC: shouldn't have to import this
-- CJP: needed for the Reflects instance for Pos and use of 'value', right?
-- EAC: Yes, but it should be exported by Lol.
import Crypto.Lol.Reflects
import Data.Singletons

type RescaleTreePow2Ctx expr k r2 =
  (Lambda expr, PosC k, RescaleTreePow2Ctx' expr k r2)

type family RescaleTreePow2Ctx' expr k r2 :: Constraint where
  RescaleTreePow2Ctx' expr 'O      r2 = ()
  RescaleTreePow2Ctx' expr ('S k') r2 =
    (PosC k', TreeMul expr k' r2, Div2 expr (PreRescaleTreePow2 expr k' r2),
     RescaleTreePow2Ctx'' expr (PreDiv2 expr (PreRescaleTreePow2 expr k' r2)))

type RescaleTreePow2Ctx'' expr r2k1 =
  (AddLit expr r2k1, AddLit expr (PreMul expr r2k1), Mul expr r2k1,
   Ring r2k1, Ring (PreMul expr r2k1))

type family PreRescaleTreePow2 expr k r2 where
  PreRescaleTreePow2 expr 'O     r2 = r2
  PreRescaleTreePow2 expr ('S k) r2 =
    PreMul expr (PreDiv2 expr (PreRescaleTreePow2 expr k r2))

-- | For \( k \geq 1 \), the "rescaling tree" that rounds a
-- mod-@2^{k+1}@ value to a mod-@2@ value, over the same ring.  This
-- also works in a SIMD fashion over CRT slots, if all the
-- mod-@2^{k+1}@ CRT slots hold \( \Z_{2^{k+1}} \) values (otherwise,
-- the behavior is undefined).

rescaleTreePow2_ :: forall r2 k expr e . (RescaleTreePow2Ctx expr k r2)
  => Tagged k (expr e (PreRescaleTreePow2 expr k r2 -> r2))
rescaleTreePow2_ = case (sing :: SPos k) of
  SO     -> tag $ lam v0
  (SS _) -> rescaleTreePow2_'

rescaleTreePow2_' :: forall r2 k expr e . (RescaleTreePow2Ctx expr ('S k) r2)
  => Tagged ('S k) (expr e (PreRescaleTreePow2 expr ('S k) r2 -> r2))
rescaleTreePow2_' = tag $ lam $
  let v'    = v0 *: (one >+: v0)
      kval  = proxy value (Proxy::Proxy k) :: Int
      pDiv4 = 2^(kval-1)
  in let_ v' $ treeMul (Proxy::Proxy k) $
       map ((div2_ $:) . (>+: v0)) $ take pDiv4 $ [fromInteger $ y * (-y+1) | y <- [1..]]

class TreeMul expr (k :: Pos) r2 where
  treeMul :: Proxy k
          -> [expr env (PreRescaleTreePow2 expr k r2)]
          -> expr env r2

instance TreeMul expr 'O r2 where
  treeMul _ [x] = x
  treeMul _ _   = error "Internal error in TreeMul base case."

instance (TreeMul expr k r2, Lambda expr,
          Div2 expr (PreRescaleTreePow2 expr k r2),
          Mul expr (PreDiv2 expr (PreRescaleTreePow2 expr k r2)))
  => TreeMul expr ('S k) r2 where

  treeMul _ = treeMul (Proxy::Proxy k) .
              map ((div2_ $:) . uncurry (*:)) . pairs

pairs :: [a] -> [(a,a)]
pairs []       = []
pairs (a:b:xs) = (a,b) : pairs xs
pairs _        = error "pairs internal error: odd number of elements"
