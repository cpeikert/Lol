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

module HomomRLWR where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import RescaleTree
import LinearDec2CRT
import Crypto.Alchemy.MonadAccumulator
--import Crypto.Alchemy.Interpreter.DedupRescale
import Crypto.Alchemy.Interpreter.Depth
import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Interpreter.RescaleTree
import Crypto.Alchemy.Interpreter.Size
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.LinearCyc

import Algebra.Additive as Additive (C(..))
import qualified Algebra.Ring as Ring (C(..))
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Type.Natural (Nat(Z))

-- a concrete Z_2^e data type
type Z2E e i = ZqBasic ('PP '(Prime2, e)) i

-- EAC: these instances need a home
deriving instance (Additive a) => Additive.C (Identity a)
deriving instance (Ring a) => Ring.C (Identity a)

-- EAC: This is a convenient function, but it needs a home.
argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask
{-
khprf_5hop :: forall t rngs k outputPNoise i env z2k expr z2 h0 h1 h2 h3 h4 h5 preTunnelPNoise postTunnelPNoise .
  (z2 ~ Z2E 'O i,
   -- tunnel
   rngs ~ '[h0,h1,h2,h3,h4,h5],
   TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Proxy rngs -> Tagged k (expr env (preTunnelPNoise (Cyc t h0 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_5hop rngs = do
  rescaleTree <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescaleTree .: tunn5 rngs

-- khprf_1hop', but without point-free style
khprf_1hop'' :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs .
  (z2 ~ Z2E 'O i,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop'' = do
  rescaleTree <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ lam $ rescaleTree $: (tunnelDecToCRT_ $: v0)

-- khprf_1hop, but with generalized tunneling constraints
khprf_1hop' :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise rngs .
  (z2 ~ Z2E 'O i,
    -- tunnel
   rngs ~ '[h4,h5], TunnelChainCtx expr t postTunnelPNoise z2k rngs,
   PreTunnelM expr postTunnelPNoise rngs ~ preTunnelPNoise,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop' = do
  rescaleTree <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescaleTree .: tunnelDecToCRT_

khprf_1hop :: forall t h4 h5 k outputPNoise i env z2k expr z2 postTunnelPNoise preTunnelPNoise .
  (z2 ~ Z2E 'O i, Lambda expr,
    -- tunnel
   TunnelDecToCRTCtx expr postTunnelPNoise t h4 h5 z2k,
   PreTunnelCyc expr postTunnelPNoise ~ preTunnelPNoise,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (preTunnelPNoise (Cyc t h4 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_1hop = do
  rescaleTree <- rescaleTreePow2_ @(outputPNoise (Cyc t h5 z2))
  return $ rescaleTree .: tunnelDecToCRT_
-}
khprf_0hop :: forall t h5 k outputPNoise i z2k env expr z2 postTunnelPNoise .
  (z2 ~ Z2E 'O i, Lambda expr,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (postTunnelPNoise (Cyc t h5 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_0hop = rescaleTreePow2_

main :: IO ()
main = do
  -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function
{-
  putStrLn $ pprint $ untag $ khprf_0hop @CT @H0 @P3 @Identity @Int64
  let (ex01,ex0) = dup $ untag $ khprf_0hop @CT @H0 @P3 @(PNoise 'Z) @Int64
      (ex02,ex03) = dup ex0
  putStrLn $ "PT expression0: " ++ pprint ex01
  putStrLn $ "PT expression0 size: " ++ (show $ size ex02)
  putStrLn $ "PT expression0 depth: " ++ (show $ depth ex03)

  putStrLn $ pprint $ untag $ khprf_1hop @CT @H4 @H0 @P2 @Identity @Int64
  let (ex11,ex1) = dup $ untag $ khprf_1hop @CT @H4 @H0 @P2 @(PNoise 'Z) @Int64
      (ex12,ex13) = dup ex1
  putStrLn $ "PT expression1: " ++ pprint ex11
  putStrLn $ "PT expression1 size: " ++ (show $ size ex12)
  putStrLn $ "PT expression1 depth: " ++ (show $ depth ex13)

  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_1hop' @CT @H0 @H1 @P3 @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @(PNoise 'Z) @Int64
  putStrLn $ pprint $ untag $ khprf_1hop'' @CT @H0 @H1 @P3 @Identity @Int64
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @Identity @Int64 Proxy


  -- EAC: It's terrible that we can't use Dup here: PreDiv2 P and PreDiv2 E disagree
  putStrLn $ pprint $ untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy
  putStrLn $ show $ eval (untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy) 2
-}
  -- compile the up-applied function to CT, then print it out
  evalKeysHints (8.0 :: Double) $ do
    y <- argToReader (pt2ct
                         @'[ '(H0,H0)]
                         @ZqList
                         @TrivGad
                         @Int64
                         @Double)
                         --(rescale4to2 @CT @H0 @(PNoise 'Z)) -- 1 minute, 8 sec
                         (untag $ khprf_0hop @CT @H0 @P2 @(PNoise 'Z) @Int64) -- 1 minute, 6 sec
                         --(untag $ khprf_1hop @CT @H4 @H5 @P3 @(PNoise 'Z) @Int64)
                         --(untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy)
    -- compile once, interpret with multiple ctexprs!!

    let (z1,z2) = dup y
    liftIO $ putStrLn $ pprint z1
    z2' <- readerToAccumulator $ writeErrorRates @Int64 @() z2
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
type ZqList = '[ZQ1,ZQ2] -- ,ZQ3,ZQ4,ZQ5,ZQ6,ZQ7]

type Zq (q :: TLNatNat) = ZqBasic q Int64

type H0 = F128
type H1 = F64 * F7
type H2 = F32 * F7 * F13
type H3 = F8 * F5 * F7 * F13
type H4 = F4 * F3 * F5 * F7 * F13
type H5 = F9 * F5 * F7 * F13
type H0' = H0 * F7 * F13
type H1' = H1 * F13
type H2' = H2
type H3' = H3
type H4' = H4
type H5' = H5
type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]
