{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-|

  \( \def\Z{\mathbb{Z}} \)

-}

module Crypto.Alchemy.Interpreter.RescaleToTree
( RescaleCycCRTCtx
, rescaleTreeCRT_, rescaleTreeCRT)
where

import Crypto.Alchemy.Interpreter.MapCRTSlots

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.RescaleZqPow2
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.TunnelCyc

import Crypto.Lol
-- EAC: shouldn't have to import this
import Crypto.Lol.Reflects
import Crypto.Lol.Types

import Control.Applicative

-- | Constraint synonym for rescaling the CRT slots of a Cyc.
-- This synonym is injective in zqenv and cyc2.
type RescaleCycCRTCtx env t m expr k z2 cyc2 cyc2k =
  (Lambda expr, Embed () env, RescaleCycCRTCtx' (RescaleToTree (MapCRTSlots expr t m)) t m k z2 cyc2 cyc2k)

type RescaleCycCRTCtx' rescaleexpr t m k z2 cyc2 cyc2k =
  (RescaleZqPow2 rescaleexpr k z2,
   cyc2k ~ Zq2Cyc t m (PreRescaleZqPow2 rescaleexpr k z2), cyc2 ~ Zq2Cyc t m z2)

-- | Convenience function: using the (arithmetic) rescaling tree,
-- rescale each \( \Z_{2^k} \) value in the mod-\( 2^k \) CRT slots of
-- \( R_{2^k} \) down to \( \Z_2 \), with the result in \( R_2 \).
-- (If the values in the slots are not restricted to \( \Z_{2^k} \),
-- the behavior is undefined.)
rescaleTreeCRT_ :: forall env t m expr k z2 cyc2 cyc2k rescaleexpr .
  (RescaleCycCRTCtx env t m expr k z2 cyc2 cyc2k,
   rescaleexpr ~ RescaleToTree (MapCRTSlots expr t m))
  => Tagged '(t,m,k) (expr env (cyc2k -> Zq2Cyc t m z2))
-- EAC: GHC is insisting on a type sig here, but I suspect it is unnecessary.
rescaleTreeCRT_ =  pure $ embedExpr $ proxy
  (mapCRTSlots <$> (rescaleToTree <$> rescaleZqPow2_ :: _ (MapCRTSlots expr t m () (PreRescaleZqPow2 rescaleexpr k z2 -> z2))))
  (Proxy::Proxy k)

rescaleTreeCRT :: (RescaleCycCRTCtx env t m expr k z2 cyc2 cyc2k)
  => Tagged '(t,m,k) (expr env cyc2k -> expr env (Zq2Cyc t m z2))
rescaleTreeCRT = ($:) <$> rescaleTreeCRT_

-- EAC: RescaleToTree *only* needs to be an instance of RescaleZqPow2, and no
-- other interpreters need to implement that class. It feels like that RescaleZqPow2
-- doesn't really need to be a class at all, and RescaleToTree doesn't need to be
-- a full interpreter.

-- However, I'm keeping it this way for now

newtype RescaleToTree expr env a = RT { rescaleToTree :: expr env a }
  --deriving (Lambda, List, Functor_, Applicative_, Monad_, MonadReader_, MonadWriter_)

type family PreRescale expr (k::Pos) z2 where
  PreRescale expr  'O    z2 = z2
  PreRescale expr ('S k) z2 = PreMul expr (PreDiv2 expr (PreRescale expr k z2))

-- CJP: rescaling anything from mod-2 to mod-2 is just the identity
-- function.  Is this instance head OK, or too general?

-- TODO: go back to specific instance heads for ZqBasic, PNoise,
-- Identity (if necessary)

instance (Lambda expr) => RescaleZqPow2 (RescaleToTree expr) 'O a where

  type PreRescaleZqPow2 (RescaleToTree expr) 'O a = a

  -- k = 1, so p = 2^k = 2
  rescaleZqPow2_ = pure $ RT $ lam v0

instance (RescaleToTreeCtx expr k (ZqBasic PP2 i))
  => RescaleZqPow2 (RescaleToTree expr) ('S k) (ZqBasic PP2 i) where

  type PreRescaleZqPow2 (RescaleToTree expr) ('S k) (ZqBasic PP2 i) =
    PreRescale expr ('S k) (ZqBasic PP2 i)

  rescaleZqPow2_ = rescaleZqPow2__

instance (RescaleToTreeCtx expr k (f (ZqBasic PP2 i)))
  => RescaleZqPow2 (RescaleToTree expr) ('S k) (f (ZqBasic PP2 i)) where

  type PreRescaleZqPow2 (RescaleToTree expr) ('S k) (f (ZqBasic PP2 i)) =
    PreRescale expr ('S k) (f (ZqBasic PP2 i))

  rescaleZqPow2_ = rescaleZqPow2__

type RescaleToTreeCtx expr k z2 =
  RescaleToTreeCtx' expr k z2 (PreRescale expr k z2)

type RescaleToTreeCtx' expr k z2 z2k =
  (Div2 expr z2k, RescaleToTreeCtx'' expr k z2 (PreDiv2 expr z2k))

type RescaleToTreeCtx'' expr k z2 prediv =
  (Lambda expr, AddLit expr prediv,
   PreRescaleZqPow2 (RescaleToTree expr) ('S k) z2 ~ PreMul expr prediv,
   Internal expr k z2, Reflects ('S k) Int, Mul expr prediv,
   Ring (PreMul expr prediv), Ring prediv, AddLit expr (PreMul expr prediv))

-- 'S k > 1, so p = 2^k >= 4, meaning we can divide p by 4
rescaleZqPow2__ :: forall expr k z2 e . (RescaleToTreeCtx expr k z2)
  => Tagged ('S k)
  (RescaleToTree expr e (PreRescaleZqPow2 (RescaleToTree expr) ('S k) z2 -> z2))
rescaleZqPow2__ = pure $ RT $ lam $
    let xprod = v0 *: (one >+: v0)
        lgpVal = proxy value (Proxy::Proxy ('S k)) :: Int
        pDiv4 = 2^(lgpVal-2)
    in internal (Proxy::Proxy k) $ take pDiv4 $
         map ((div2_ $:) . (>+: xprod)) [fromInteger $ y * (-y+1) | y <- [1..]]

class Internal expr (k :: Pos) z2 where
  internal :: Proxy k -> [expr env (PreRescale expr k z2)] -> expr env z2

instance Internal expr P1 z2 where
  internal _ [x] = x
  internal _ _   = error "Internal error in RescaleToTree (internal) base case."

instance (Internal expr k z2, Div2 expr (PreRescale expr k z2), Lambda expr,
          Mul expr (PreDiv2 expr (PreRescale expr k z2)))
  => Internal expr ('S k) z2 where
  internal _ = internal (Proxy::Proxy k) . map ((div2_ $:) . uncurry (*:)) . listToPairs

listToPairs :: [a] -> [(a,a)]
listToPairs []       = []
listToPairs (a:b:xs) = (a,b) : listToPairs xs
listToPairs _        = error "listToPairs internal error: odd number of elements"

{-
instance (Add expr a) => Add (RescaleToTree expr) a where
  add_ = RT add_
  neg_ = RT neg_

instance (AddLit expr a) => AddLit (RescaleToTree expr) a where
  addLit_ = RT . addLit_

instance (Mul expr a) => Mul (RescaleToTree expr) a where
  type PreMul (RescaleToTree expr) a = PreMul expr a
  mul_ = RT mul_

instance (MulLit expr a) => MulLit (RescaleToTree expr) a where
  mulLit_ = RT . mulLit_

instance (Div2 expr a) => Div2 (RescaleToTree expr) a where
  type PreDiv2 (RescaleToTree expr) a = PreDiv2 expr a

  div2_ = RT div2_

instance (SHE expr) => SHE (RescaleToTree expr) where
  type ModSwitchPTCtx   (RescaleToTree expr) ct zp' =
       ModSwitchPTCtx                  expr  ct zp'
  type RescaleLinearCtx (RescaleToTree expr) ct zq' =
       RescaleLinearCtx                expr  ct zq'
  type AddPublicCtx     (RescaleToTree expr) ct     =
       AddPublicCtx                    expr  ct
  type MulPublicCtx     (RescaleToTree expr) ct     =
       MulPublicCtx                    expr  ct
  type KeySwitchQuadCtx (RescaleToTree expr) ct gad =
       KeySwitchQuadCtx                expr  ct gad
  type TunnelCtx (RescaleToTree expr) t e r s e' r' s' zp zq gad =
       TunnelCtx                expr  t e r s e' r' s' zp zq gad

  modSwitchPT_   = RT   modSwitchPT_
  rescaleLinear_ = RT   rescaleLinear_
  addPublic_     = RT . addPublic_
  mulPublic_     = RT . mulPublic_
  keySwitchQuad_ = RT . keySwitchQuad_
  tunnel_        = RT . tunnel_

instance (TunnelCyc expr m) => TunnelCyc (RescaleToTree expr) m where
  type TunnelCycCtx (RescaleToTree expr) m t e r s zp =
       TunnelCycCtx                expr  m t e r s zp

  type PreTunnelCyc (RescaleToTree expr) m = PreTunnelCyc expr m

  tunnelCyc_ = RT . tunnelCyc_
-}