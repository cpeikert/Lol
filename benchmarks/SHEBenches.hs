{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, 
             MultiParamTypeClasses, NoImplicitPrelude, RankNTypes, RebindableSyntax, 
             ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}

module SHEBenches (sheBenches) where

import Utils
import Harness.SHE
import SHETests

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State
import Control.Monad.Trans.State (StateT, evalStateT)
import Crypto.Lol.Types.Random
import Crypto.Random.DRBG

import Crypto.Lol hiding (CT)
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Applications.SymmSHE

import Data.Promotion.Prelude.List

sheBenches :: (MonadRandom rnd) => rnd Benchmark
sheBenches = bgroupRnd "SHE" [
   bgroupRnd "encrypt"   $ benchEnc (Proxy::Proxy EncParams) $ wrap' bench_enc,
   bgroupRnd "decrypt"   $ benchDec (Proxy::Proxy DecParams) $ wrap' bench_dec,
   bgroupRnd "*"         $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' bench_mul,
   bgroupRnd "addPublic" $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' bench_addPublic,
   bgroupRnd "mulPublic" $ benchCTFunc (Proxy::Proxy CTParams) $ wrap' bench_mulPublic,
   bgroupRnd "dec"       $ benchDec (Proxy::Proxy DecParams) $ wrap' bench_dec,
   bgroupRnd "rescaleCT" $ benchRescale (Proxy::Proxy RescaleParams) $ wrap' bench_rescaleCT,
   bgroupRnd "keySwitch" $ benchKSQ (Proxy::Proxy KSQParams) $ wrap' bench_keySwQ,
   bgroupRnd "tunnel"    $ benchTunn (Proxy::Proxy TunnParams)  $ wrap' bench_tunnel
   ]

bench_enc :: forall t m m' z zp zq gen . (EncryptCtx t m m' z zp zq, CryptoRandomGen gen, z ~ LiftOf zp)
  => SK (Cyc t m' z) -> PT (Cyc t m zp) -> NFValue' '(t,m,m',zp,zq,gen)
bench_enc sk pt = NFV $ nfIO $ do
  gen <- newGenIO
  return $ evalRand (encrypt sk pt :: Rand (CryptoRand gen) (CT m zp (Cyc t m' zq))) gen

bench_mul :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq)
bench_mul a = nfv (*a)

bench_addPublic :: (AddPublicCtx t m m' zp zq) => Cyc t m zp -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq)
bench_addPublic a ct = nfv (addPublic a) ct

bench_mulPublic :: (MulPublicCtx t m m' zp zq) => Cyc t m zp -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq)
bench_mulPublic a ct = nfv (mulPublic a) ct

-- requires zq to be Liftable
bench_dec :: (DecryptCtx t m m' z zp zq, z ~ LiftOf zp) 
  => SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq)
bench_dec sk ct = nfv (decrypt sk) ct

bench_rescaleCT :: forall t m m' zp zq zq' . 
  (RescaleCyc (Cyc t) zq' zq, ToSDCtx t m' zp zq', NFData (CT m zp (Cyc t m' zq)))
  => CT m zp (Cyc t m' zq') -> NFValue' '(t,m,m',zp,zq,zq')
bench_rescaleCT = nfv (rescaleLinearCT :: CT m zp (Cyc t m' zq') -> CT m zp (Cyc t m' zq))

bench_keySwQ :: (Ring (CT m zp (Cyc t m' zq)), NFData (CT m zp (Cyc t m' zq))) 
  => KSHint m zp t m' zq gad zq' -> CT m zp (Cyc t m' zq) -> NFValue' '(t,m,m',zp,zq,zq',gad)
bench_keySwQ (KeySwitch kswq) x = nfv kswq $ x*x

bench_tunnel :: (NFData (CT s zp (Cyc t s' zq))) 
  => Tunnel t r r' s s' zp zq gad -> CT r zp (Cyc t r' zq) -> NFValue' '(t,r,r',s,s',zp,zq,gad)
bench_tunnel (Tunnel f) x = nfv f x

type Gens    = '[HashDRBG]
type Gadgets = '[TrivGad, BaseBGad 2]
type Tensors = '[CT.CT,RT]
type MM'PQCombos = 
  '[ '(F4, F128, Zq 64, Zq 257),
     '(F4, F128, Zq 64, Zq (257 ** 641)),
     '(F12, F32 * F9, Zq 64, Zq 577),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 64, Zq 14401),
     '(F12, F32 * F9 * F25, Zq 64, Zq (14401 ** 21601))
    ]

type CTParams  = ( '(,) <$> Tensors) <*> MM'PQCombos
type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable MM'PQCombos))
type RescaleParams = ( '(,) <$> Tensors) <*> (Map AddZq (Filter NonLiftable MM'PQCombos))
type KSQParams = ( '(,) <$> Gadgets) <*> RescaleParams
type EncParams = ( '(,) <$> Gens) <*> CTParams

-- 3144961,5241601,7338241,9959041,10483201,11531521,12579841,15200641,18869761,19393921
type TunnParams = 
  ( '(,) <$> Gadgets) <*> 
  (( '(,) <$> Tensors) <*> 
  (( '(,) <$> TunnRings) <*> TunnMods))


type TunnRings = '[
  {- H0 -> H1 -} '(F128, F128 * F7 * F13, F64 * F7, F64 * F7 * F13),
  {- H1 -> H2 -} '(F64 * F7, F64 * F7 * F13, F32 * F7 * F13, F32 * F7 * F13),
  {- H2 -> H3 -} '(F32 * F7 * F13, F32 * F7 * F13, F8 * F5 * F7 * F13, F8 * F5 * F7 *F13),
  {- H3 -> H4 -} '(F8 * F5 * F7 * F13, F8 * F5 * F7 *F13, F4 * F3 * F5 * F7 * F13, F4 * F3 * F5 * F7 * F13),
  {- H4 -> H5 -} '(F4 * F3 * F5 * F7 * F13, F4 * F3 * F5 * F7 *F13, F9 * F5 * F7 * F13, F9 * F5 * F7 * F13)   
    --{- H0 -> H5 -} '(F128, F128 * F7 * F13, F9 * F5 * F7 * F13, F9 * F5 * F7 * F13)
    ]

type TunnMods = '[
  '(Zq PP32, Zq 3144961),
  '(Zq PP32, Zq (3144961 ** 5241601)),
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241)),
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241 ** 9959041)),
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241 ** 9959041 ** 10483201)),
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241 ** 9959041 ** 10483201 ** 11531521)),
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241 ** 9959041 ** 10483201 ** 11531521 ** 12579841)){-,
  '(Zq PP32, Zq (3144961 ** 5241601 ** 7338241 ** 9959041 ** 10483201 ** 11531521 ** 12579841 ** 15200641))-}
  ]