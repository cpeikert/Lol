{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
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

import Crypto.Alchemy.MonadAccumulator
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise hiding (take)
import Crypto.Alchemy.Interpreter.RescaleTree
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.TunnelCyc

import Algebra.Additive as Additive (C(..))
import qualified Algebra.Ring as Ring (C(..))
import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Singletons.Prelude.List (Reverse)
import Data.Type.Natural (Nat(Z))

-- a concrete Z_2^e data type
type Z2E e i = ZqBasic ('PP '(Prime2, e)) i

-- EAC: these instances need a home
deriving instance (Additive a) => Additive.C (Identity a)
deriving instance (Ring a) => Ring.C (Identity a)

-- EAC: This is a convenient function, but it needs a home.
argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask

khprf_5hop :: forall t rngs k outputPNoise i env z2k expr z2 h0 h1 h2 h3 h4 h5 preTunnelPNoise postTunnelPNoise k' .
  (z2 ~ Z2E 'O i,
   -- tunnel
   rngs ~ '[h0,h1,h2,h3,h4,h5],
   TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   k ~ 'S k',
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Proxy rngs -> Tagged k (expr env (preTunnelPNoise (Cyc t h0 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_5hop _ = do
  rescale <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescale .: tunnelDecToCRT_ .: tunnelDecToCRT_ @h4 .:
    tunnelDecToCRT_ @h3 .: tunnelDecToCRT_ @h2 .: tunnelDecToCRT_ @h1 .: lam v0

-- khprf_1hop', but without point-free style
khprf_1hop'' :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs k' .
  (z2 ~ Z2E 'O i,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   k ~ 'S k',
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop'' = do
  rescale <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ lam $ rescale $: (tunnelDecToCRT_ $: v0)

-- khprf_1hop, but with generalized tunneling constraints
khprf_1hop' :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs k' .
  (z2 ~ Z2E 'O i,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   k ~ 'S k',
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop' = do
  rescale <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescale .: tunnelDecToCRT_

khprf_1hop :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise k' .
  (z2 ~ Z2E 'O i, Lambda expr,
    -- tunnel
   TunnelDecToCRTCtx expr postTunnelPNoise t h4 h5 z2k,
   PreTunnelCyc expr postTunnelPNoise ~ preTunnelPNoise,
   -- rescaleCycCRT
   k ~ 'S k',
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop = do
  rescale <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescale .: tunnelDecToCRT_

khprf_0hop :: forall t h5 k outputPNoise i z2k env expr z2 postTunnelPNoise k' .
  (z2 ~ Z2E 'O i, Lambda expr,
   -- rescaleCycCRT
   k ~ 'S k',
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (postTunnelPNoise (Cyc t h5 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_0hop = rescaleTreePow2_

main :: IO ()
main = do
  -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function
  putStrLn $ pprint $ untag $ khprf_0hop @CT @H5 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_0hop @CT @H5 @P3 @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_1hop @CT @H0 @H1 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_1hop @CT @H0 @H1 @P3  @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @Identity @Int64 Proxy


  -- EAC: It's terrible that we can't use Dup here: PreDiv2 P and PreDiv2 E disagree
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy
  putStrLn $ show $ eval (untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy) 2

  -- compile the up-applied function to CT, then print it out
  evalKeysHints (1.0 :: Double) $ do
    y <- argToReader (pt2ct
                         @RngList -- from HomoPRFParams
                         @ZqList
                         @TrivGad
                         @Int64
                         @Double)
                         (untag $ khprf_1hop @CT @H4 @H5 @P3 @(PNoise 'Z) @Int64)
                         --(untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy)
    -- compile once, interpret with multiple ctexprs!!
    let (z1,z2) = dup y
    liftIO $ putStrLn $ pprint z1
    z2' <- readerToAccumulator $ writeErrorRates @_ @Int64 z2
    let (z2'',errors) = runWriter $ eval z2' $ return 2
    liftIO $ putStrLn $ show z2''
    liftIO $ print errors
    --liftIO $ putStrLn $ pprint $ dedupRescale z2

type ZQ1 = Zq $(mkTLNatNat 18869761)
type ZQ2 = Zq $(mkTLNatNat 19393921)
type ZQ3 = Zq $(mkTLNatNat 19918081)
type ZQ4 = Zq $(mkTLNatNat 25159681)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ5 = Zq $(mkTLNatNat 2149056001)
-- for rounding off after the first hop
type ZQ6 = Zq $(mkTLNatNat 3144961)
type ZQ7 = Zq $(mkTLNatNat 7338241)
type ZqList = '[ZQ1,ZQ2,ZQ3,ZQ4,ZQ5,ZQ6,ZQ7]



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
