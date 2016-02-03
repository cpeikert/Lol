{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Harness.SHE 
(KSHint(..)
--,WrapCtx
,KSQCtxD
--,hideKSQ
,genSHEArgs

,wrap'
--,wrapKSQ
--,wrapRescale
--,wrapDec

,benchKSQ
,benchRescale
,benchDec
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

import GHC.Prim



genSHEArgs :: forall sk bnch rnd . 
  (Benchmarkable (StateT (Maybe sk) rnd) bnch, Monad rnd)
  => Proxy sk -> bnch -> rnd (ResultOf bnch)
genSHEArgs _ f = evalStateT (genArgs f) (Nothing :: Maybe sk)

--extract an SK type from a tuple of params
type family SKOf (a :: k) :: * where
  SKOf '(t,m,m',zp,zq)         = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq')     = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq',gad) = SK (Cyc t m' (LiftOf zp))

wrap' :: forall a rnd bnch res c . 
  (Benchmarkable (StateT (Maybe (SKOf a)) rnd) bnch, Monad rnd, ShowArgs a,
   WrapFunc res, res ~ ResultOf bnch, res ~ c a)
  => bnch -> Proxy a -> rnd (WrapOf res)
wrap' f p = wrap (showArgs p) <$> genSHEArgs (Proxy::Proxy (SKOf a)) f





-- allowed args: CT, KSHint, SK
-- context for (*), (==), decryptUnrestricted
data KSQCtxD


-- it'd be nice to make this associated to `Satsify`,
-- but we have to use a *ton* of kind signatures if we do
type family KSQCtx a where
  KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))) = 
    (EncryptCtx t m m' (LiftOf zp) zp zq,
     KeySwitchCtx gad t m' zp zq zq',
     KSHintCtx gad t m' (LiftOf zp) zq',
     -- ^ these provide the context to generate the parameters
     Ring (CT m zp (Cyc t m' zq)), 
     Eq (Cyc t m zp), 
     Fact m, Fact m', CElt t zp, m `Divides` m',
     Reduce (LiftOf zp) zq, Lift' zq, CElt t (LiftOf zp), ToSDCtx t m' zp zq, Reduce (LiftOf zq) zp,
     -- ^ these provide the context for tests
     NFData (CT m zp (Cyc t m' zq)),
     ShowArgs '(t,m,m',zp,zq,zq',gad))
     -- ^ these provide the context for benchmarks

instance (params `Satisfy` KSQCtxD, KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
  => ( '(gad , '(t, '(m, m', zp, zq, zq'))) ': params) `Satisfy` KSQCtxD where
  data ArgsCtx KSQCtxD where
    KSQD :: (KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
      => Proxy '(t,m,m',zp,zq,zq',gad) -> ArgsCtx KSQCtxD

  

  run _ f = (f $ KSQD (Proxy::Proxy '(t,m,m',zp,zq,zq',gad))) : (run (Proxy::Proxy params) f)

hideKSQ :: (forall t m m' zp zq zq' gad . (KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
  => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res) -> ArgsCtx KSQCtxD -> rnd res
hideKSQ f (KSQD p) = f p


benchKSQ :: (params `Satisfy` KSQCtxD) => 
  Proxy params ->
  (forall t m m' zp zq zq' gad . (KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
     => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res)
  -> [rnd res]
benchKSQ p g = run p $ hideKSQ g






data RescaleCtxD
type RescaleCtx t m m' zp zq zq' = 
  (EncryptCtx t m m' (LiftOf zp) zp zq',
   ShowArgs '(t,m,m',zp,zq,zq'),
   RescaleCyc (Cyc t) zq' zq,
   NFData (CT m zp (Cyc t m' zq)),
   ToSDCtx t m' zp zq')
instance (params `Satisfy` RescaleCtxD, RescaleCtx t m m' zp zq zq') 
  => ( '(t, '(m,m',zp,zq,zq')) ': params) `Satisfy` RescaleCtxD where
  data ArgsCtx RescaleCtxD where
    RD :: (RescaleCtx t m m' zp zq zq') 
      => Proxy '(t,m,m',zp,zq,zq') -> ArgsCtx RescaleCtxD
  run _ f = (f $ RD (Proxy::Proxy '(t,m,m',zp,zq,zq'))) : (run (Proxy::Proxy params) f)

hideRescale:: (forall t m m' zp zq zq' . (RescaleCtx t m m' zp zq zq') 
  => Proxy '(t,m,m',zp,zq,zq') -> rnd res) -> ArgsCtx RescaleCtxD -> rnd res
hideRescale f (RD p) = f p

benchRescale :: (params `Satisfy` RescaleCtxD) =>
  Proxy params ->
  (forall t m m' zp zq zq' . (RescaleCtx t m m' zp zq zq') 
    => Proxy '(t,m,m',zp,zq,zq') -> rnd res)
  -> [rnd res]
benchRescale p g = run p $ hideRescale g









data DecCtxD
type DecCtx t m m' zp zq = 
  (DecryptCtx t m m' (LiftOf zp) zp zq,
   ShowArgs '(t,m,m',zp,zq))
instance (params `Satisfy` DecCtxD, DecCtx t m m' zp zq) 
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` DecCtxD where
  data ArgsCtx DecCtxD where
    DecD :: (DecCtx t m m' zp zq) 
      => Proxy '(t,m,m',zp,zq) -> ArgsCtx DecCtxD
  run _ f = (f $ DecD (Proxy::Proxy '(t, m,m',zp,zq))) : (run (Proxy::Proxy params) f)

hideDec:: (forall t m m' zp zq . (DecCtx t m m' zp zq) 
  => Proxy '(t,m,m',zp,zq) -> rnd res) -> ArgsCtx DecCtxD -> rnd res
hideDec f (DecD p) = f p

benchDec :: (params `Satisfy` DecCtxD) =>
  Proxy params ->
  (forall t m m' zp zq . (DecCtx t m m' zp zq) 
        => Proxy '(t,m,m',zp,zq) -> rnd res)
     -> [rnd res]
benchDec p g = run p $ hideDec g









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