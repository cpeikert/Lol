{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module NoiseExperiment where

import Crypto.Alchemy.MonadAccumulator
import Crypto.Alchemy.Interpreter.KeysHints

import Crypto.Alchemy.Interpreter.Dup
import Crypto.Alchemy.Interpreter.ErrorRateWriter
import Crypto.Alchemy.Interpreter.Eval
import Crypto.Alchemy.Interpreter.Params
import Crypto.Alchemy.Interpreter.Print
import Crypto.Alchemy.Interpreter.PT2CT
import Crypto.Alchemy.Interpreter.PT2CT.Noise

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.LinearCyc
import Common
import LinearDec2CRT

import Crypto.Lol
--import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor.CPP
import Crypto.Lol.Types

import Control.Monad.Random
import Control.Monad.Writer

import Data.Type.Natural

expr :: forall t r s hout zp expr env hin preTunnelNoise .
  (Lambda expr,
   Mul expr (PreMul expr (preTunnelNoise (Cyc t r zp))),
   Mul expr (preTunnelNoise (Cyc t r zp)),
   LinearDecToCRTCtx expr hout t r s zp,
   preTunnelNoise ~ PreLinearCyc expr hout,
   PreMul expr (PreMul expr (preTunnelNoise (Cyc t r zp))) ~ hin (Cyc t r zp))
  => expr env (hin (Cyc t r zp) -> hout (Cyc t s zp))
expr = lam $ (lam $ linearDecToCRT_ @s @r $: (v0 *: v0)) $: (v0 *: v0)

expr2 :: forall a expr env . (Add expr a, Lambda expr) => expr env (a -> a)
expr2 = lam $ (lam $ v0 +: v0) $: (v0 +: v0)

type K = P4
type M'Map = '[ '(F8,F16)]
type Gad = TrivGad

main :: IO ()
main = do
  putStrLn $ "RescaleTree:"

  putStrLn $ "PT RescaleTree: " ++ (pprint $ expr2 @(PNoise 'Z (Cyc CT F8 (ZqBasic PP8 Int64))))

  let (ptexpr :: PT2CT' M'Map ZqList Gad _, paramsexpr) = dup $ expr2 @(PNoise 'Z (Cyc CT F8 (ZqBasic PP8 Int64)))
  putStrLn $ "PT expression params:\n" ++ params ptexpr paramsexpr

  putStrLn $ "test" ++ (show $ eval (expr2 @Int) 1)

  evalKeysHints (8.0 :: Double) $ do

    roundTree <- argToReader (pt2ct
                  @M'Map
                  @ZqList
                  @Gad
                  @Int64)
                  (expr2 @(PNoise 'Z (Cyc CT F8 (ZqBasic PP8 Int64))))

    let (r1,r) = dup roundTree
        (r2,r3) = dup r

    liftIO $ putStrLn $ pprint r1
    --liftIO $ putStrLn $ params r1 r2

    ptin <- liftIO $ getRandom
    arg1 <- argToReader encrypt ptin

    f <- readerToAccumulator $ writeErrorRates @Int64 @() r2
    let (_,errors) = runWriter $ eval f (return arg1)
    liftIO $ print errors


    liftIO $ putStrLn "--------"

    liftIO $ putStrLn $ show $ eval r3 arg1

{-
  putStrLn $ "RescaleTree:"
  let (ex01,ex02) = dup $ expr @CT @H4 @H5 @(PNoise 'Z) @(ZqBasic PP16 Int64)
  putStrLn $ "PT RescaleTree: " ++ pprint ex01
  putStrLn $ "PT RescaleTree size: " ++ (show $ size ex02)

  let (ptexpr, paramsexpr) = dup $ expr @CT @H4 @H5 @(PNoise 'Z) @(ZqBasic PP16 Int64)
  putStrLn $ "PT expression params:\n" ++ params ptexpr paramsexpr

  evalKeysHints (8.0 :: Double) $ do

    roundTree <- argToReader (pt2ct
                  @'[ '(H4,H4'), '(H5,H5)]
                  @ZqList
                  @TrivGad
                  @Int64)
                  ptexpr

    let (r1,r) = dup roundTree
        (r2,r3) = dup r

    liftIO $ putStrLn $ pprint r1
    liftIO $ putStrLn $ params r1 r2

    ptin <- liftIO $ getRandom
    arg1 <- argToReader encrypt ptin

    f <- readerToAccumulator $ writeErrorRates @Int64 @() r3
    let (_,errors) = runWriter $ eval f (return arg1)

    liftIO $ print errors
-}
-- these are ~ 2^15

type Zq1 = Zq $(mkTLNatNat 3144961)
type Zq2 = Zq $(mkTLNatNat 5241601)
type Zq3 = Zq $(mkTLNatNat 7338241)
type Zq4 = Zq $(mkTLNatNat 9959041)
type Zq5 = Zq $(mkTLNatNat 10483201)
type Zq6 = Zq $(mkTLNatNat 11531521)
type Zq7 = Zq $(mkTLNatNat 12579841)
type ZqList = '[Zq1,Zq2,Zq3] -- ,Zq4,Zq5,Zq6,Zq7]

type PTRngs = '[H4,H5]

type CTRngs = '[ '(H4,H4'), '(H5,H5') ]
