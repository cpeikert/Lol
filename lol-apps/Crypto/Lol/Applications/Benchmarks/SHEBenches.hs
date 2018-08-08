{-|
Module      : Crypto.Lol.Applications.Benchmarks.SHEBenches
Description : Benchmarks for SymmSHE.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Benchmarks for SymmSHE.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Benchmarks.SHEBenches
( decBenches
, keySwitchBenches
, rescaleBenches
, sheBenches
, tunnelBenches
) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (lift)

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Benchmarks           (Benchmark, bgroup, mkBench,
                                        mkBenchIO, showType)
import Crypto.Lol.Types
import Crypto.Lol.Types.ZPP
import Crypto.Random

sheBenches :: forall t m m' zp zq gen rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy gen -> Proxy t -> rnd Benchmark
sheBenches _ pgen _ = do
  let ptmmrr = Proxy::Proxy '(t,m,m',zp,zq)
      ptmmrrg = Proxy::Proxy '(t,m,m',zp,zq,gen)
  sk <- genSK (1 :: Double)
  return $ bgroup (showType ptmmrr ++ "/SymmSHE") $ [
    mkBenchIO "encrypt"   (bench_enc ptmmrrg sk zero),
    mkBenchIO "*"         (bench_mul ptmmrr zero zero sk),
    mkBenchIO "addPublic" (bench_addPublic ptmmrr zero zero sk),
    mkBenchIO "mulPublic" (bench_mulPublic ptmmrr zero zero sk)]

-- zq must be Liftable
decBenches :: forall t m m' zp zq rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy t -> rnd Benchmark
decBenches _ _ = do
  let ptmmrr = Proxy::Proxy '(t,m,m',zp,zq)
  sk <- genSK (1 :: Double)
  return $ bgroup (showType ptmmrr ++ "/SymmSHE")
                  [mkBenchIO "decrypt" (bench_dec ptmmrr zero sk)]

-- must be able to round from zq' to zq
rescaleBenches :: forall t m m' zp zq zq' rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq,zq') -> Proxy t -> rnd Benchmark
rescaleBenches _ _ = do
  let ptmmrrr = Proxy::Proxy '(t,m,m',zp,zq,zq')
  sk <- genSK (1 :: Double)
  return $ bgroup (showType ptmmrrr ++ "/SymmSHE")
                  [mkBenchIO "rescale" (bench_rescale ptmmrrr zero sk)]

keySwitchBenches :: forall t m m' zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy gad -> Proxy t -> rnd Benchmark
keySwitchBenches _ pgad _ = do
  let ptmmrr = Proxy::Proxy '(t,m,m',zp,zq)
      ptmmrrg = Proxy::Proxy '(t,m,m',zp,zq,gad)
  sk <- genSK (1 :: Double)
  return $ bgroup (showType ptmmrr ++ "/SymmSHE")
                  [mkBenchIO "keySwitchQuadCirc" (bench_keySwQ ptmmrrg zero sk)]

tunnelBenches :: forall t r r' s s' zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(r,r',s,s',zp,zq) -> Proxy gad -> Proxy t -> rnd Benchmark
tunnelBenches _ _ _ = do
  let p = Proxy::Proxy '(t,r,r',s,s',zp,zq,gad)
  -- These SKs are different types, so we gotta make 2 of them
  sk1 <- genSK (1 :: Double)
  sk2 <- genSK (1 :: Double)
  return $ bgroup (showType p ++ "/SymmSHE")
                  [mkBenchIO "tunnel" (bench_tunnel p zero sk1 sk2)]





bench_enc :: forall t m m' z zp (zq :: *) (gen :: *) . (z ~ LiftOf zp,  _)
  => Proxy '(t,m,m',zp,zq,gen)
     -> SK (Cyc t m' z)
     -> PT (Cyc t m zp)
     -> IO (CT m zp (Cyc t m' zq))
bench_enc _ sk pt = do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen

-- requires zq to be Liftable
bench_dec :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (PT (Cyc t m zp))
bench_dec _ pt sk = do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  evalRandIO $ return $ decrypt sk ct

bench_mul :: forall t m m' z zp zq rnd . (z ~ LiftOf zp, LiftOf zp ~ ModRep zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> PT (Cyc t m zp)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (CT m zp (Cyc t m' zq))
bench_mul _ pta ptb sk = do
  a :: CT m zp (Cyc t m' zq) <- encrypt sk pta
  b <- encrypt sk ptb
  evalRandIO $ return $ a * b

bench_addPublic :: forall t m m' z zp zq . (z ~ LiftOf zq, _)
  => Proxy '(t,m,m',zp,zq)
     -> Cyc t m zp
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (CT m zp (Cyc t m' zq))
bench_addPublic _ a pt sk = do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  evalRandIO $ return $ addPublic a ct

bench_mulPublic :: forall t m m' z zp zq . (z ~ LiftOf zq, _)
  => Proxy '(t,m,m',zp,zq)
     -> Cyc t m zp
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (CT m zp (Cyc t m' zq))
bench_mulPublic _ a pt sk = do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  evalRandIO $ return $ mulPublic a ct

bench_rescale :: forall t m m' z zp (zq :: *) (zq' :: *) . (z ~ LiftOf zq, _)
  => Proxy '(t,m,m',zp,zq,zq')
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (CT m zp (Cyc t m' zq))
bench_rescale _ pt sk = do
  ct <- encrypt sk pt
  evalRandIO $ return $ (modSwitch :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq)) ct

bench_keySwQ :: forall t m m' z zp zq (gad :: *) . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq,gad)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> IO (CT m zp (Cyc t m' zq))
bench_keySwQ _ pt sk = do
  x :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  ksqHint :: KSHint gad (Cyc t m' zq) <- ksQuadCircHint sk
  let y = x*x
  evalRandIO $ return $ keySwitchQuadCirc ksqHint y

-- possible bug: If I enable -XPartialTypeSigs and add a ",_" to the constraint list below, GHC
-- can't figure out that `e `Divides` s`, even when it's explicitly listed!
bench_tunnel :: forall c t e e' r r' s s' z zp zq gad .
  (c ~ Cyc t,
   z ~ LiftOf zp,
   Cyclotomic (Cyc t s zp),     -- linearDec
   TunnelHintCtx c e r s e' r' s' z zp zq gad,
   TunnelCtx c r s e' r' s' zp zq gad,
   EncryptCtx c r r' z zp zq,
   CRTSetCyc c zp,
   r `Divides` r',
   e ~ FGCD r s,
   Fact e)
  => Proxy '(t,r,r',s,s',zp,zq,gad)
     -> PT (Cyc t r zp)
     -> SK (Cyc t r' z)
     -> SK (Cyc t s' z)
     -> IO (CT s zp (Cyc t s' zq))
bench_tunnel _ pt skin skout = do
  x <- encrypt skin pt
  let crts :: [Cyc t s zp] = proxy crtSet (Proxy::Proxy e)
        \\ gcdDivides @r @s
      totr = totientFact @r
      tote = totientFact @e
      -- only take as many crts as we need
      -- otherwise linearDec fails
      linf :: Linear (Cyc t) e r s zp = linearDec (take (totr `div` tote) crts)
        \\ gcdDivides @r @s
  hints :: TunnelHint gad (Cyc t) e r s e' r' s' zp zq <- tunnelHint linf skout skin
  evalRandIO $ return $ (tunnel hints :: CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)) x
