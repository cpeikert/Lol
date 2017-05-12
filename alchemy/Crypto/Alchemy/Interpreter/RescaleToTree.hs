{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Alchemy.Interpreter.RescaleToTree where

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.RescaleZqPow2
import Crypto.Alchemy.Language.SHE
import Crypto.Alchemy.Language.TunnelCyc

import Crypto.Lol
import Crypto.Lol.Types

import Data.Singletons

newtype RescaleToTree expr env a = RT (expr env a)
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

  tunnelCyc = RT . tunnelCyc


type family PreTunnel' expr (k::Pos) i where
  PreTunnel' expr 'O i = ZqBasic PP2 i
  PreTunnel' expr ('S k) i = PreMul expr (PreDiv2 expr (PreTunnel' expr k i))


instance (zp2k ~ ZqBasic ('PP '(Prime2, k)) i, prezp2k ~ PreMul expr zp2k,
  Lambda expr, Internal expr k i, Mul expr zp2k, PosC k, Ring (PreMul expr zp2k),
  AddLit expr prezp2k, AddLit expr zp2k, ToInteger i, PreMul expr zp2k ~ zp2k)
  => RescaleZqPow2 (RescaleToTree expr) k (ZqBasic PP2 i) where
  -- | The type corresponding to \( \Z_{2^k} \).  (The type should
  -- determine the exponent, hence the partially injectivity.)
  type PreRescaleZqPow2 (RescaleToTree expr) k (ZqBasic PP2 i) = ZqBasic ('PP '(Prime2, k)) i

  -- | Rescale (round) the argument from \( \Z_{2^k} \) to \( \Z_2 \).
  rescaleZqPow2_ :: forall env . RescaleToTree expr env (ZqBasic ('PP '(Prime2, k)) i -> ZqBasic PP2 i)
  rescaleZqPow2_ = case (sing :: SPos k) of
    -- k = 1, so p = 2^k = 2
    SO -> RT $ lam v0
    -- k > 1, so p = 2^k >= 4, meaning we can divide p by 4
    lgp@(SS k') -> RT $ lam $
      let xprod = v0 *: (one >+: v0) :: expr _ (ZqBasic ('PP '(Prime2, k)) i)
          lgpVal = posToInt $ fromSing lgp :: Int
          pDiv4 = 2^(lgpVal-2)
      in internal $ take pDiv4 $ map ((div2_ $:) . (>+: xprod)) [fromInteger $ y * (-y+1) | y <- [1..]]
{-
instance (zp2k ~ ZqBasic ('PP '(Prime2, k)) i, prezp2k ~ PreMul expr zp2k,
  Lambda expr, Internal expr k i, Mul expr zp2k, PosC k, Ring (PreMul expr zp2k),
  AddLit expr prezp2k, AddLit expr zp2k, ToInteger i, PreMul expr zp2k ~ zp2k)
  => RescaleZqPow2 (RescaleToTree expr) k (PNoise h (ZqBasic PP2 i)) where
  -- | The type corresponding to \( \Z_{2^k} \).  (The type should
  -- determine the exponent, hence the partially injectivity.)
  type PreRescaleZqPow2 (RescaleToTree expr) k (PNoise h (ZqBasic PP2 i)) = ZqBasic ('PP '(Prime2, k)) i

  -- | Rescale (round) the argument from \( \Z_{2^k} \) to \( \Z_2 \).
  rescaleZqPow2_ :: forall env . RescaleToTree expr env (ZqBasic ('PP '(Prime2, k)) i -> ZqBasic PP2 i)
  rescaleZqPow2_ = case (sing :: SPos k) of
    -- k = 1, so p = 2^k = 2
    SO -> RT $ lam v0
    -- k > 1, so p = 2^k >= 4, meaning we can divide p by 4
    lgp@(SS k') -> RT $ lam $
      let xprod = v0 *: (one >+: v0) :: expr _ (ZqBasic ('PP '(Prime2, k)) i)
          lgpVal = posToInt $ fromSing lgp :: Int
          pDiv4 = 2^(lgpVal-2)
      in internal $ take pDiv4 $ map ((div2_ $:) . (>+: xprod)) [fromInteger $ y * (-y+1) | y <- [1..]]
-}
class Internal expr (k :: Pos) i where
  internal :: [expr env (ZqBasic ('PP '(Prime2, k)) i)] -> expr env (ZqBasic PP2 i)

instance Internal expr P1 i where
  internal [x] = x
  internal _ = error "Internal error in RescaleToTree (internal) base case."

--instance