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

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

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
import Crypto.Alchemy.Interpreter.Params
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
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Type.Natural hiding (Nat(S))

-- a concrete Z_2^e data type
type Z2E e = ZqBasic ('PP '(Prime2, e)) Int64

-- EAC: these instances need a home
deriving instance (Additive a) => Additive.C (Identity a)
deriving instance (Ring a) => Ring.C (Identity a)

-- EAC: This is a convenient function, but it needs a home.
argToReader :: (MonadReader v mon) => (v -> a -> mon b) -> a -> mon b
argToReader f a = flip f a =<< ask

type K = P2
type Gad = TrivGad
type RescaleM'Map = '[ '(H5,H5)]
type RescaleZqs = '[Zq1,Zq2,Zq3,Zq4]
type PT2CT' m'map zqs a = PT2CT m'map zqs Gad Int64 P (StateT Keys (StateT Hints (ReaderT Double IO))) () a

main :: IO ()
main = do

  putStrLn $ "RescaleTree:"
  let (ex01,ex02) = dup $ untag $ rescaleTreePow2_ @(PNoise 'Z (Cyc CT H5 (ZqBasic PP2 Int64))) @K
  putStrLn $ "PT RescaleTree: " ++ pprint ex01
  putStrLn $ "PT RescaleTree size: " ++ (show $ size ex02)

  -- EAC: can remove type sig and use ptexpr as the argument to pt2ct below (which infers the signature),
  -- but this requires compiling PT2CT which takes a long time.
  let (ptrescale :: PT2CT' RescaleM'Map RescaleZqs _, paramsexpr1) = dup $ untag $ rescaleTreePow2_ @(PNoise 'Z (Cyc CT H5 (ZqBasic PP2 Int64))) @K
  putStrLn $ "PT expression params:\n" ++ params ptrescale paramsexpr1


  putStrLn $ "Tunnel:"
  -- EAC: 'Z noise is important here so that we can print the composition of P expr
  let (ex11,ex12) = dup $ linear5 @CT @PTRngs @(Z2E K) @(PNoise 'Z) Proxy
  putStrLn $ "PT Tunnel: " ++ pprint ex11
  putStrLn $ "PT Tunnel size: " ++ (show $ size ex12)

  -- EAC: This needs to have a non-zero output pNoise level!!
  -- EAC: can remove type sig and use ptexpr as the argument to pt2ct below (which infers the signature),
  -- but this requires compiling PT2CT which takes a long time.
  let (pttunnel :: PT2CT' CTRngs ZqList _, paramsexpr2) = dup $ linear5 @CT @PTRngs @(Z2E K) @(PNoise N9) Proxy
  putStrLn $ "PT expression params:\n" ++ params pttunnel paramsexpr2

  putStrLn $ "PT Composition: " ++ pprint (ex01 .: ex11)

  -- compile the un-applied function to CT, then print it out
  evalKeysHints 8.0 $ do

    roundTree <- argToReader (pt2ct
                  @RescaleM'Map
                  @RescaleZqs
                  @Gad
                  @Int64)
                  (untag $ rescaleTreePow2_ @(PNoise 'Z (Cyc CT H5 (ZqBasic PP2 Int64))) @K)

    tunn <- argToReader (pt2ct
                  @CTRngs
                  @ZqList
                  @Gad
                  @Int64)
                  (linear5 @CT @PTRngs @(Z2E K) @(PNoise N9) Proxy)

    let (r1,r) = dup roundTree
        (r2,r3) = dup r
        (s1,s) = dup tunn
        (s2,s3) = dup s

    liftIO $ putStrLn "CT Tunneling:"
    liftIO $ putStrLn $ pprint s1
    liftIO $ putStrLn $ params s1 s2

    liftIO $ putStrLn "CT Rounding Tree:"
    liftIO $ putStrLn $ pprint r1
    liftIO $ putStrLn $ params r1 r2

    liftIO $ putStrLn "CT Composition:"
    liftIO $ putStrLn $ pprint (r1 .: s1)

    ptin <- liftIO $ getRandom
    arg1 <- argToReader encrypt ptin

    f <- readerToAccumulator $ writeErrorRates @Int64 @() r3
    g <- readerToAccumulator $ writeErrorRates @Int64 @() s3
    let (_,errors) = runWriter $ eval (f .: g) (return arg1)

    liftIO $ print errors

-- these are ~ 2^15

type Zq1 = Zq $(mkTLNatNat 3144961)
type Zq2 = Zq $(mkTLNatNat 5241601)
type Zq3 = Zq $(mkTLNatNat 7338241)
type Zq4 = Zq $(mkTLNatNat 9959041)
type Zq5 = Zq $(mkTLNatNat 10483201)
type Zq6 = Zq $(mkTLNatNat 11531521)
type Zq7 = Zq $(mkTLNatNat 12579841)
type ZqList = '[Zq1,Zq2,Zq3,Zq4,Zq5,Zq6,Zq7]

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

type PTRngs = '[H0,H1,H2,H3,H4,H5]

type CTRngs = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]


{-

    -- example with rescale de-duplication when tunneling
  -- print the unapplied PT function

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

    tunn <- argToReader (pt2ct
                         @RngList
                         @ZqList
                         @TrivGad
                         @Int64)
                         --(rescale4to2 @CT @H0 @(PNoise 'Z)) -- 1 minute, 8 sec
                         (untag $ khprf_0hop @CT @H0 @P2 @(PNoise 'Z) @Int64)
                         --(rescale4to2 @CT @H5 @(PNoise 'Z)) -- 1 minute, 8 sec
                         --(untag $ khprf_1hop @CT @H4 @H5 @P3 @(PNoise 'Z) @Int64)
                         --(untag $ khprf_5hop @CT @'[H0,H1,H2,H3,H4,H5] @P3 @(PNoise 'Z) @Int64 Proxy)






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
-}




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

khprf_0hop :: forall t h5 k outputPNoise i z2k env expr z2 postTunnelPNoise .
  (z2 ~ Z2E 'O i, Lambda expr,
   -- rescaleCycCRT
   PreRescaleTreePow2 expr k (outputPNoise (Cyc t h5 z2)) ~ postTunnelPNoise (Cyc t h5 z2k),
   RescaleTreePow2Ctx expr k (outputPNoise (Cyc t h5 z2)))
  => Tagged k (expr env (postTunnelPNoise (Cyc t h5 z2k) -> outputPNoise (Cyc t h5 z2)))
khprf_0hop = rescaleTreePow2_
-}
