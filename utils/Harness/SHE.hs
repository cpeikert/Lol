{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, 
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Harness.SHE 
(KSHint(..)
,WrapCtx
,KSQCtx
,hideKSQ
,genSHEArgs
,benchKSQ
,wrapKSQ
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




genSHEArgs :: forall t (m :: Factored) m' z zp zq bnch rnd . 
  (z ~ LiftOf zp, Benchmarkable (StateT (Maybe (SK (Cyc t m' z))) rnd) bnch, Monad rnd)
  => Proxy '(t,m,m',zp,zq) -> bnch -> rnd (ResultOf bnch)
genSHEArgs _ f = evalStateT (genArgs f) (Nothing :: Maybe (SK (Cyc t m' z)))


type WrapCtx (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) zp zq rnd bnch = 
  (Monad rnd, 
   ShowArgs '(t,m,m',zp,zq), 
   Benchmarkable (StateT (Maybe (SK (Cyc t m' (LiftOf zp)))) rnd) bnch)





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
   ShowArgs '(t,m,m',zp,zq),
   ShowArgs '(t,m,m',zp,zq,gad))
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

wrapKSQ :: forall t m m' (zp :: *) zq zq' gad rnd bnch res c . 
  (WrapCtx t m m' zp zq rnd bnch, ShowArgs '(t,m,m',zp,zq,gad),
   WrapFunc res, res ~ ResultOf bnch, res ~ c '(t,m,m',zp,zq,zq',gad))
  => bnch
     -> Proxy '(t,m,m',zp,zq,zq',gad) 
     -> rnd (WrapOf res)
wrapKSQ f _ = 
  wrap (showArgs (Proxy::Proxy '(t,m,m',zp,zq,gad))) <$> genSHEArgs (Proxy::Proxy '(t,m,m',zp,zq)) f

benchKSQ :: (params `Satisfy` KSQCtxD) =>
  Proxy params ->
  (forall t m m' zp zq zq' gad . 
    (KSQCtx t m m' zp zq zq' gad) 
    => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res)
  -> [rnd res]
benchKSQ p g = runAll p $ hideKSQ g






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

-- use this data type in functions that need a circular key switch hint
newtype KSHint m zp t m' zq gad zq' = KeySwitch (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq))
instance (Generatable rnd (SK (Cyc t m' z)),
          z ~ LiftOf zp,
          KeySwitchCtx gad t m' zp zq zq',
          KSHintCtx gad t m' z zq', 
          MonadRandom rnd)
  => Generatable rnd (KSHint m zp t m' zq gad zq') where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    KeySwitch <$> proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad,zq'))