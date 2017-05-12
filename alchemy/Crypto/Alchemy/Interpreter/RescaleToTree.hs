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

module Crypto.Alchemy.Interpreter.RescaleToTree
(RescaleCycCRTCtx, rescaleCycCRT_, rescaleCycCRT
,RescaleToTree, rescaleToTree) where

import Crypto.Alchemy.Interpreter.MapCRTSlots
import Crypto.Alchemy.Interpreter.PT2CT.Noise hiding (take)

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.RescaleZqPow2
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.TunnelCyc

import Crypto.Lol
import Crypto.Lol.Reflects -- EAC: shouldn't have to import this
import Crypto.Lol.Types

import Control.Applicative

-- | Constraint synonym for rescaling the CRT slots of a Cyc.
-- This synonym is injective in zqenv.
type RescaleCycCRTCtx zqenv env t m expr k z2 cyc2k =
  RescaleCycCRTCtx' (RescaleToTree (MapCRTSlots expr t m)) zqenv env t m k z2 cyc2k

type RescaleCycCRTCtx' bigexpr zqenv env t m k z2 cyc2k =
  (env ~ Zq2Cyc t m zqenv, RescaleZqPow2 bigexpr k z2,
   Zq2Cyc t m (PreRescaleZqPow2 bigexpr k z2) ~ cyc2k)

rescaleCycCRT_ :: (RescaleCycCRTCtx zqenv env t m expr k z2 cyc2k)
  => Tagged k (expr env (cyc2k -> Zq2Cyc t m z2))
rescaleCycCRT_ = mapCRTSlots <$> rescaleToTree <$> rescaleZqPow2_

rescaleCycCRT :: (RescaleCycCRTCtx zqenv env t m expr k z2 cyc2k, Lambda expr)
  => Tagged k (expr env cyc2k -> expr env (Zq2Cyc t m z2))
rescaleCycCRT = ($:) <$> rescaleCycCRT_

newtype RescaleToTree expr env a = RT {rescaleToTree :: expr env a}
  deriving (Lambda, List, Functor_, Applicative_, Monad_, MonadReader_, MonadWriter_)

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
  type ModSwitchPTCtx   (RescaleToTree expr) ct zp' = ModSwitchPTCtx expr ct zp'
  type RescaleLinearCtx (RescaleToTree expr) ct zq' = RescaleLinearCtx expr ct zq'
  type AddPublicCtx     (RescaleToTree expr) ct     = AddPublicCtx expr ct
  type MulPublicCtx     (RescaleToTree expr) ct     = MulPublicCtx expr ct
  type KeySwitchQuadCtx (RescaleToTree expr) ct gad = KeySwitchQuadCtx expr ct gad
  type TunnelCtx (RescaleToTree expr) t e r s e' r' s' zp zq gad =
    TunnelCtx expr t e r s e' r' s' zp zq gad

  modSwitchPT_ = RT modSwitchPT_

  rescaleLinear_ = RT rescaleLinear_

  addPublic_ = RT . addPublic_

  mulPublic_ = RT . mulPublic_

  keySwitchQuad_ = RT . keySwitchQuad_

  tunnel_ = RT . tunnel_

instance (TunnelCyc expr m) => TunnelCyc (RescaleToTree expr) m where
  type TunnelCycCtx (RescaleToTree expr) m t e r s zp = TunnelCycCtx expr m t e r s zp

  type PreTunnelCyc (RescaleToTree expr) m = PreTunnelCyc expr m

  tunnelCyc_ = RT . tunnelCyc_


type family PreRescale expr (k::Pos) z2 where
  PreRescale expr 'O z2 = z2
  PreRescale expr ('S k) z2 = PreMul expr (PreDiv2 expr (PreRescale expr k z2))

instance (Lambda expr) => RescaleZqPow2 (RescaleToTree expr) 'O (ZqBasic PP2 i) where

  type PreRescaleZqPow2 (RescaleToTree expr) 'O (ZqBasic PP2 i) = ZqBasic PP2 i

  -- k = 1, so p = 2^k = 2
  rescaleZqPow2_ = pure $ RT $ lam v0

instance (Lambda expr) => RescaleZqPow2 (RescaleToTree expr) 'O (PNoise h (ZqBasic PP2 i)) where

  type PreRescaleZqPow2 (RescaleToTree expr) 'O (PNoise h (ZqBasic PP2 i)) =
    PNoise h (ZqBasic PP2 i)

  -- k = 1, so p = 2^k = 2
  rescaleZqPow2_ = pure $ RT $ lam v0

instance (RescaleToTreeCtx expr k (ZqBasic PP2 i))
  => RescaleZqPow2 (RescaleToTree expr) ('S k) (ZqBasic PP2 i) where

  type PreRescaleZqPow2 (RescaleToTree expr) ('S k) (ZqBasic PP2 i) =
    PreRescale expr ('S k) (ZqBasic PP2 i)

  rescaleZqPow2_ = rescaleZqPow2__

instance (RescaleToTreeCtx expr k (PNoise h (ZqBasic PP2 i)))
  => RescaleZqPow2 (RescaleToTree expr) ('S k) (PNoise h (ZqBasic PP2 i)) where

  type PreRescaleZqPow2 (RescaleToTree expr) ('S k) (PNoise h (ZqBasic PP2 i)) =
    PreRescale expr ('S k) (PNoise h (ZqBasic PP2 i))

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

-- k > 1, so p = 2^k >= 4, meaning we can divide p by 4
rescaleZqPow2__ :: forall expr k z2 e .
  (RescaleToTreeCtx expr k z2)
  => Tagged ('S k) (RescaleToTree expr e (PreRescaleZqPow2 (RescaleToTree expr) ('S k) z2 -> z2))
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
  internal _ _ = error "Internal error in RescaleToTree (internal) base case."

instance (Internal expr k z2, Div2 expr (PreRescale expr k z2), Lambda expr,
          Mul expr (PreDiv2 expr (PreRescale expr k z2)))
  => Internal expr ('S k) z2 where
  internal _ = internal (Proxy::Proxy k) . map ((div2_ $:) . uncurry (*:)) . listToPairs

listToPairs :: [a] -> [(a,a)]
listToPairs (a:b:xs) = (a,b):listToPairs xs