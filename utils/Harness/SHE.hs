{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Harness.SHE 
(DecMod
,KSHint(..)
,WrapCtx
,KSQCtx
,hideKSQ
,genSHEArgs
,benchKSQ
,wrap
,WrapFunc
,WrapOf
)where

import Utils

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State

import Crypto.Lol hiding (CT)
import Crypto.Lol.Applications.SymmSHE
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

type family DecMod zq where
  DecMod (a,b) = DecMod b -- correct since we use right-associative pairs
  DecMod a = a

newtype KSHint m zp t m' zq gad zq' = KeySwitch (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq))

type family SKOf bnch where
  SKOf (CT m zp (Cyc t m' zq) -> a) = SK (Cyc t m' (LiftOf zp))
  SKOf (SK (Cyc t m' z) -> a) = SK (Cyc t m' z)
  SKOf (KSHint m zp t m' zq gad zq' -> a) = SK (Cyc t m' (LiftOf zp))
  SKOf (a -> b) = SKOf b
type WrapCtx (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) zp zq rnd bnch = 
  (Monad rnd, ShowArgs '(t,m,m',zp,zq), Benchmarkable (StateT (Maybe (SKOf bnch)) rnd) bnch)


genSHEArgs :: forall t (m :: Factored) m' z zp zq bnch rnd res . 
  (z ~ LiftOf zp, Benchmarkable (StateT (Maybe (SK (Cyc t m' z))) rnd) bnch, Monad rnd,
   ResultOf bnch ~ res)
  => Proxy '(t,m,m',zp,zq) -> bnch -> rnd res
genSHEArgs _ f = evalStateT (genArgs f) (Nothing :: Maybe (SK (Cyc t m' z)))





data KSQCtxD
type KSQCtx t m m' zp zq zq' gad = 
  (EncryptCtx t m m' (LiftOf zp) zp zq,
   KeySwitchCtx gad t m' zp zq zq',
   KSHintCtx gad t m' (LiftOf zp) zq',
   -- ^ these provide the context to generate the parameters
   Ring (CT m zp (Cyc t m' zq)), 
   Eq (Cyc t m zp), 
   Fact m, Fact m', CElt t zp, m `Divides` m',
   Reduce (LiftOf zp) zq, Lift' zq, CElt t (LiftOf zp), ToSDCtx t m' zp zq, Reduce (LiftOf zq) zp,
   -- ^ these provide the context for tests
   Ring (CT m zp (Cyc t m' zq)),
   NFData (CT m zp (Cyc t m' zq)),
   ShowArgs '(t,m,m',zp,zq))
   -- ^ these provide the context for benchmarks

instance (params `Satisfy` KSQCtxD, KSQCtx t m m' zp zq zq' gad) 
  => ( '(gad, '(t, '(m,m',zp,zq,zq'))) ': params) `Satisfy` KSQCtxD where
  data ArgsCtx KSQCtxD where
    KSQD :: (KSQCtx t m m' zp zq zq' gad) 
      => Proxy '(t,m,m',zp,zq,zq',gad) -> ArgsCtx KSQCtxD
  runAll _ f = (f $ KSQD (Proxy::Proxy '(t,m,m',zp,zq,zq',gad))) : (runAll (Proxy::Proxy params) f)

hideKSQ :: (forall t m m' zp zq zq' gad . (KSQCtx t m m' zp zq zq' gad) 
  => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res) -> ArgsCtx KSQCtxD -> rnd res
hideKSQ f (KSQD p) = f p







type Gadgets = '[TrivGad, BaseBGad 2]
type Tensors = '[CT.CT,RT]
type MM'PQQ'Combos = 
  '[ '(F4, F128, Zq 64, Zq 257, Zq (257 ** 641)),
     '(F12, F32 * F9, Zq 64, Zq 577, Zq (577 ** 1153)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153), Zq (577 ** 1153 ** 2017)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017), Zq (577 ** 1153 ** 2017 ** 2593)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593), Zq (577 ** 1153 ** 2017 ** 2593 ** 3169)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169), Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457), Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337)),
     '(F12, F32 * F9, Zq 64, Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337), Zq (577 ** 1153 ** 2017 ** 2593 ** 3169 ** 3457 ** 6337 ** 7489)),
     '(F12, F32 * F9 * F25, Zq 64, Zq 14401, Zq (14401 ** 21601))
    ]

data RemoveZq' :: TyFun (Factored, Factored, *, *, *) (Factored, Factored, *, *) -> *
type instance Apply RemoveZq' '(m,m',zp,zq,zq') = '(m,m',zp,zq)

data Liftable :: TyFun (Factored, Factored, *, *) Bool -> *
type instance Apply Liftable '(m,m',zp,zq) = Int64 :== (LiftOf zq)

type CTParams = ( '(,) <$> Tensors) <*> (Nub (Map RemoveZq' MM'PQQ'Combos))
type DecParams = ( '(,) <$> Tensors) <*> (Nub (Filter Liftable (Map RemoveZq' MM'PQQ'Combos)))
type Zq'Params = ( '(,) <$> Tensors) <*> MM'PQQ'Combos
type KSQParams = ( '(,) <$> Gadgets) <*> Zq'Params

benchKSQ :: 
  (forall t m m' zp zq zq' gad . 
    (KSQCtx t m m' zp zq zq' gad) 
    => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res)
  -> [rnd res]
benchKSQ g = runAll (Proxy::Proxy KSQParams) $ hideKSQ g


class WrapFunc res where
  type WrapOf res

  wrap :: String -> res -> WrapOf res

instance WrapFunc NFValue where
  type WrapOf NFValue = Benchmark
  wrap = bench

instance WrapFunc Bool where
  type WrapOf Bool = Test
  wrap str = testProperty str . property



-- generates a secrete key with svar=1, using non-cryptographic randomness
instance (GenSKCtx t m z Double, 
          MonadRandom rnd, 
          MonadState (Maybe (SK (Cyc t m z))) rnd)
  => Generatable rnd (SK (Cyc t m z)) where
  genArg = do
    msk <- get
    sk <- case msk of
      Just sk -> return sk
      Nothing -> do
        sk <- genSK (1 :: Double)
        put $ Just sk
        return sk
    return sk

instance (EncryptCtx t m m' z zp zq,
          z ~ LiftOf zp,
          MonadRandom rnd,
          Generatable rnd (SK (Cyc t m' z)),
          Generatable rnd (Cyc t m zp)) 
  => Generatable rnd (CT m zp (Cyc t m' zq)) where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    pt <- genArg
    encrypt sk pt

instance (Generatable rnd (SK (Cyc t m' z)),
          z ~ LiftOf zp,
          KeySwitchCtx gad t m' zp zq zq',
          KSHintCtx gad t m' z zq', 
          MonadRandom rnd)
  => Generatable rnd (KSHint m zp t m' zq gad zq') where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    KeySwitch <$> proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad,zq'))