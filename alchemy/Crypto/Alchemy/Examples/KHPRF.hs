{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Examples.KHPRF where

import Crypto.Lol
import Crypto.Lol.Applications.Examples.HomomPRFParams
import Crypto.Lol.Cyclotomic.Tensor (TElt) -- EAC: I shouldn't need to explicitly import this
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types
import Crypto.Lol.Types.ZPP                -- EAC: I shouldn't need to explicitly import this...

import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT.Noise hiding (take)
import Crypto.Alchemy.Interpreter.RescaleTree
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.TunnelCyc

import Algebra.Additive as Additive (C(..))
import qualified Algebra.Ring as Ring (C(..))
import Control.Monad.Identity
import Data.Singletons.Prelude.List (Reverse)
import Data.Type.Natural (Nat(Z))

-- a concrete Z_2^e data type
type Z2E e i = ZqBasic ('PP '(Prime2, e)) i

-- EAC: these instances need a home
deriving instance (Additive a) => Additive.C (Identity a)
deriving instance (Ring a) => Ring.C (Identity a)

khprf_5hop :: forall t rngs k outputPNoise env z2k expr z2 h0 h1 h2 h3 h4 h5 preTunnelPNoise postTunnelPNoise .
  (z2 ~ Zq PP2,
   -- tunnel
   rngs ~ '[h0,h1,h2,h3,h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   RescaleCycCRTCtx env t h5 expr k (outputPNoise z2) (outputPNoise (Cyc t h5 z2)) (postTunnelPNoise (Cyc t h5 z2k)))
  => Proxy rngs -> Tagged k (expr env (preTunnelPNoise (Cyc t h0 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_5hop _ = return $ (untag $ rescaleTreeCRT_ @t @h5 @k) .:
  tunnelDecToCRT_ .: tunnelDecToCRT_ @h4 .: tunnelDecToCRT_ @h3 .: tunnelDecToCRT_ @h2 .: tunnelDecToCRT_ @h1 .: lam v0

-- khprf_1hop', but without point-free style
khprf_1hop'' :: forall t h4 h5 k outputPNoise env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs .
  (z2 ~ Zq PP2,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   RescaleCycCRTCtx env t h5 expr k (outputPNoise z2) (outputPNoise (Cyc t h5 z2)) (postTunnelPNoise (Cyc t h5 z2k)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop'' = return $ lam $ (untag $ rescaleTreeCRT_ @t @h5 @k) $: (tunnelDecToCRT_ $: v0)

-- khprf_1hop, but with generalized tunneling constraints
khprf_1hop' :: forall t h4 h5 k outputPNoise env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs .
  (z2 ~ Zq PP2,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   RescaleCycCRTCtx env t h5 expr k (outputPNoise z2) (outputPNoise (Cyc t h5 z2)) (postTunnelPNoise (Cyc t h5 z2k)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop' = return $ (untag $ rescaleTreeCRT_ @t @h5 @k) .: tunnelDecToCRT_

khprf_1hop :: forall t h4 h5 k outputPNoise env z2k expr z2 postTunnelPNoise preTunnelPNoise .
  (z2 ~ Zq PP2, Lambda expr,
    -- tunnel
   TunnelDecToCRTCtx expr postTunnelPNoise t h4 h5 z2k,
   PreTunnelCyc expr postTunnelPNoise ~ preTunnelPNoise,
   -- rescaleCycCRT
   RescaleCycCRTCtx env t h5 expr k (outputPNoise z2) (outputPNoise (Cyc t h5 z2)) (postTunnelPNoise (Cyc t h5 z2k)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop = return $ (untag $ rescaleTreeCRT_ @t @h5 @k) .: tunnelDecToCRT_

khprf_0hop :: forall t h5 k outputPNoise z2k env expr z2 postTunnelPNoise .
  (z2 ~ Zq PP2, Lambda expr,
   -- rescaleCycCRT
   RescaleCycCRTCtx env t h5 expr k (outputPNoise z2) (outputPNoise (Cyc t h5 z2)) (postTunnelPNoise (Cyc t h5 z2k)))
  => Tagged k (expr env (postTunnelPNoise (Cyc t h5 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_0hop = retag $ rescaleTreeCRT_ @t @h5 @k

main :: IO ()
main = do
  --let (exp1a, exp1b) = dup $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @(Zq PP8) @(PNoise 'Z) @() @('S ('S 'O)) @Int64 Proxy

  -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function
  putStrLn $ pprint $ untag $ khprf_0hop @CT @H5 @P3 @(PNoise 'Z)
  putStrLn $ pprint $ untag $ khprf_0hop @CT @H5 @P3 @Identity
  putStrLn $ pprint $ untag $ khprf_1hop @CT @H0 @H1 @P3 @(PNoise 'Z)
  putStrLn $ pprint $ untag $ khprf_1hop @CT @H0 @H1 @P3  @Identity
  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @(PNoise 'Z)
  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @Identity
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @(PNoise 'Z)
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @Identity
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) Proxy
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @Identity Proxy
  --putStrLn $ show $ eval exp1b 2
{-
  -- compile the up-applied function to CT, then print it out
  evalKeysHints (1.0 :: Double) $ do
    y <- argToReader (pt2ct
         @'[ '(H0, H0'), '(H1,H1'), '(H2, H2') ]
         @'[ Zq $(mkTLNatNat $ 2^(15 :: Int)),
             Zq $(mkTLNatNat $ 2^(15 :: Int)+2),
             Zq $(mkTLNatNat $ 2^(15 :: Int)+4) ]
         @TrivGad
         @Int64
         @Double)
         (tunn1 @CT @H0 @H1 @H2 @(Zq PP8) @(PNoise 'Z) Proxy)
    -- compile once, interpret with multiple ctexprs!!
    let (z1,z2) = dup y
    liftIO $ putStrLn $ pprint z1
    liftIO $ putStrLn $ pprint z2
    --liftIO $ putStrLn $ pprint $ dedupRescale z2
-}

-- given the output 'm' (Cyc wrapper) of a chain of tunnels, returns the input Cyc wrapper.
type family PreTunnelM expr m (rngs :: [Factored]) where
  PreTunnelM expr m '[x] = m
  PreTunnelM expr m (r ': rngs) = PreTunnelM expr (PreTunnelCyc expr m) rngs

-- | Context for a chaini of tunnels using the decToCRT linear function.
type TunnelChainCtx expr m t z2k (rngs :: [Factored]) = TunnelChainCtx' expr m t z2k (Reverse rngs)

-- | Helper family for TunnelChainCtx. Takes rings in *reverse* order so that
-- the Cyc wrapper `m` is applied appropriately.
type family TunnelChainCtx' expr m t z2k (rngs :: [Factored]) where
  TunnelChainCtx' expr t m z2k '[x] = (Lambda expr)
  -- EAC: Reverse r and s here because they are applied in reverse
  TunnelChainCtx' expr t m z2k (r ': s ': rngs) = (TunnelDecToCRTCtx expr m t s r z2k, TunnelChainCtx' expr t (PreTunnelCyc expr m) z2k (s ': rngs))

-- | Constraint synonym for tunnelCyc'
type TunnelDecToCRTCtx expr m t r s zp =
  (TunnelCyc expr m, TunnelCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)

tunnelDecToCRT_ :: forall s expr env m t r zp .
  (TunnelCyc expr m, TunnelCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)
  => expr env ((PreTunnelCyc expr m) (Cyc t r zp) -> m (Cyc t s zp))
tunnelDecToCRT_ = tunnelCyc_ decToCRT

-- | Tunnel with the decToCRT linear function.
tunnelDecToCRT :: forall s expr env m t r zp .
  (TunnelCyc expr m, TunnelCycCtx expr m t (FGCD r s) r s zp, Lambda expr, FunCtx t r s zp)
  => expr env ((PreTunnelCyc expr m) (Cyc t r zp)) -> expr env (m (Cyc t s zp))
tunnelDecToCRT a = tunnelCyc_ decToCRT $: a

-- | Constraint synonym for decToCRT
type FunCtx t r s zp = FunCtx' t (FGCD r s) r s zp

type FunCtx' t e r s zp =
  (e `Divides` r, e `Divides` s, CElt t zp,  -- linearDec
   ZPP zp, TElt t (ZpOf zp))

-- EAC: needs a home; currently replicated in several places
-- | Linear function mapping decoding basis coefficients to CRT slots
decToCRT :: forall s t r zp e . (FunCtx t r s zp, e ~ FGCD r s) => Linear t zp e r s
decToCRT =
  let crts = proxy crtSet (Proxy::Proxy e)
      r = proxy totientFact (Proxy::Proxy r)
      e = proxy totientFact (Proxy::Proxy e)
      dim = r `div` e
      -- only take as many crts as we need
      -- otherwise linearDec fails
  in linearDec $ take dim crts
