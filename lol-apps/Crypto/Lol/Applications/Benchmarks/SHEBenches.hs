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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Benchmarks.SHEBenches (sheBenches, decBenches, rescaleBenches, tunnelBenches) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random hiding (lift)

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Benchmarks
import Crypto.Lol.Types
import Crypto.Lol.Types.ZPP
import Crypto.Random

addGen5 :: Proxy gen -> Proxy '(t,m,m',zp,zq) -> Proxy '(t,m,m',zp,zq,gen)
addGen5 _ _ = Proxy

addGen6 :: Proxy gad -> Proxy '(t,m,m',zp,zq,zq') -> Proxy '(t,m,m',zp,zq,zq',gad)
addGen6 _ _ = Proxy

sheBenches :: forall t m m' zp zq gen rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy gen -> Proxy t -> rnd Benchmark
sheBenches _ pgen _ =
  let ptmr = Proxy :: Proxy '(t,m,m',zp,zq)
  in benchGroup (showType ptmr ++ "/SymmSHE") $ ($ ptmr) <$> [
    genBenchArgs "encrypt"   bench_enc . addGen5 pgen,
    genBenchArgs "*"         bench_mul,
    genBenchArgs "addPublic" bench_addPublic,
    genBenchArgs "mulPublic" bench_mulPublic
    ]

-- zq must be Liftable
decBenches :: forall t m m' zp zq rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy t -> rnd Benchmark
decBenches _ _ =
  let ptmr = Proxy::Proxy '(t,m,m',zp,zq)
  in benchGroup (showType ptmr ++ "/SymmSHE") [genBenchArgs "decrypt" bench_dec ptmr]

-- must be able to round from zq' to zq
rescaleBenches :: forall t m m' zp zq zq' gad rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq,zq') -> Proxy gad -> Proxy t -> rnd Benchmark
rescaleBenches _ pgad _ =
  let ptmr = Proxy :: Proxy '(t,m,m',zp,zq,zq')
  in benchGroup (showType ptmr ++ "/SymmSHE") $ ($ ptmr) <$> [
       genBenchArgs "rescaleCT" bench_rescaleCT,
       genBenchArgs "keySwitchQuadCirc" bench_keySwQ . addGen6 pgad]

tunnelBenches :: forall t r r' s s' zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(r,r',s,s',zp,zq) -> Proxy gad -> Proxy t -> rnd Benchmark
tunnelBenches _ _ _ =
  let ptmr = Proxy :: Proxy '(t,r,r',s,s',zp,zq,gad)
  in benchGroup (showType ptmr ++ "/SymmSHE") [genBenchArgs "tunnel" bench_tunnel ptmr]


bench_enc :: forall t m m' z zp (zq :: *) (gen :: *) . (z ~ LiftOf zp,  _)
  => SK (Cyc t m' z) -> PT (Cyc t m zp) -> Bench '(t,m,m',zp,zq,gen)
bench_enc sk pt = benchIO $ do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen

-- requires zq to be Liftable
bench_dec :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> Bench '(t,m,m',zp,zq)
bench_dec pt sk = benchM $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  return $ bench (decrypt sk) ct

bench_mul :: forall t m m' z zp zq . (z ~ LiftOf zp, LiftOf zp ~ ModRep zp, _)
  => PT (Cyc t m zp) -> PT (Cyc t m zp) -> SK (Cyc t m' z) -> (Bench '(t,m,m',zp,zq))
bench_mul pta ptb sk = benchM $ do
  a :: CT m zp (Cyc t m' zq) <- encrypt sk pta
  b <- encrypt sk ptb
  return $ bench (*a) b

bench_addPublic :: forall t m m' z zp zq . (z ~ LiftOf zq, _)
  => Cyc t m zp -> PT (Cyc t m zp) -> SK (Cyc t m' z) -> Bench '(t,m,m',zp,zq)
bench_addPublic a pt sk = benchM $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  return $ bench (addPublic a) ct

bench_mulPublic :: forall t m m' z zp zq . (z ~ LiftOf zq, _)
  => Cyc t m zp -> PT (Cyc t m zp) -> SK (Cyc t m' z) -> Bench '(t,m,m',zp,zq)
bench_mulPublic a pt sk = benchM $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  return $ bench (mulPublic a) ct

bench_rescaleCT :: forall t m m' z zp (zq :: *) (zq' :: *) . (z ~ LiftOf zq, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> Bench '(t,m,m',zp,zq,zq')
bench_rescaleCT pt sk = benchM $ do
  ct <- encrypt sk pt
  return $ bench (rescaleLinearCT :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq)) ct

bench_keySwQ :: forall t m m' z zp zq (zq' :: *) (gad :: *) . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> Bench '(t,m,m',zp,zq,zq',gad)
bench_keySwQ pt sk = benchM $ do
  x :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  ksqHint :: KSQuadCircHint gad (Cyc t m' zq') <- ksQuadCircHint sk
  let y = x*x
  return $ bench (keySwitchQuadCirc ksqHint) y

-- possible bug: If I enable -XPartialTypeSigs and add a ",_" to the constraint list below, GHC
-- can't figure out that `e `Divides` s`, even when it's explicitly listed!
bench_tunnel :: forall t e e' r r' s s' z zp zq gad .
  (z ~ LiftOf zp,
   GenTunnelInfoCtx t e r s e' r' s' z zp zq gad,
   TunnelCtx t r s e' r' s' zp zq gad,
   e ~ FGCD r s,
   ZPP zp, Mod zp,
   z ~ ModRep zp,
   r `Divides` r',
   Fact e,
   NFData zp,
   CElt t (ZpOf zp))
  => PT (Cyc t r zp) -> SK (Cyc t r' z) -> SK (Cyc t s' z) -> Bench '(t,r,r',s,s',zp,zq,gad)
bench_tunnel pt skin skout = benchM $ do
  x <- encrypt skin pt
  let crts :: [Cyc t s zp] = proxy crtSet (Proxy::Proxy e) \\ gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)
      r = proxy totientFact (Proxy::Proxy r)
      e = proxy totientFact (Proxy::Proxy e)
      dim = r `div` e
      -- only take as many crts as we need
      -- otherwise linearDec fails
      linf :: Linear t zp e r s = linearDec (take dim crts) \\ gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)
  hints :: TunnelInfo gad t e r s e' r' s' zp zq <- tunnelInfo linf skout skin
  return $ bench (tunnelCT hints :: CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)) x
