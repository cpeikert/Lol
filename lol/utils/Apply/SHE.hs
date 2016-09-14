{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Apply.SHE
(AddZq
,Liftable
,NonLiftable
,RoundDown
,applyKSQ
,applyRescale
,applyDec
,applyCTFunc
,applyEnc
,applyTunn
,applyCTTwEm
) where

import Apply
import GenArgs
import Utils

import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZPP

import Crypto.Random.DRBG

import Data.Promotion.Prelude.Eq
import Data.Singletons
import Data.Singletons.TypeRepStar ()

data AddZq :: TyFun (Factored, Factored, *, *) (Factored, Factored, *, *, *) -> *
type instance Apply AddZq '(m,m',zp,zq) = '(m,m',zp,RoundDown zq,zq)

data Liftable :: TyFun (Factored, Factored, *, *) Bool -> *
type instance Apply Liftable '(m,m',zp,zq) = Int64 :== (LiftOf zq)

data NonLiftable :: TyFun (Factored, Factored, *, *) Bool -> *
type instance Apply NonLiftable '(m,m',zp,zq) = Integer :== (LiftOf zq)

type family RoundDown zq where
  RoundDown (a,(b,c)) = (b,c)
  RoundDown ((a,b),c) = (a,b)
  RoundDown (a,b) = a

data DecCtxD
type DecCtx t m m' zp zq =
  (Random zp, NFElt zp,
   EncryptCtx t m m' (LiftOf zp) zp zq,
   -- ^ these provide the context to generate the parameters
   DecryptCtx t m m' (LiftOf zp) zp zq, Eq zp,
   ShowType '(t,m,m',zp,zq))
data instance ArgsCtx DecCtxD where
    DecD :: (DecCtx t m m' zp zq) => Proxy '(t,m,m',zp,zq) -> ArgsCtx DecCtxD
instance (params `Satisfy` DecCtxD, DecCtx t m m' zp zq)
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` DecCtxD where
  run _ f = f (DecD (Proxy::Proxy '(t,m,m',zp,zq))) : run (Proxy::Proxy params) f

applyDec :: (params `Satisfy` DecCtxD) =>
  Proxy params ->
  (forall t m m' zp zq . (DecCtx t m m' zp zq)
        => Proxy '(t,m,m',zp,zq) -> rnd res)
     -> [rnd res]
applyDec params g = run params $ \(DecD p) -> g p

data TunnCtxD
-- union of compatible constraints in benchmarks
type TunnCtx t r r' e e' s s' zp zq gad =
  (NFData (CT s zp (Cyc t s' zq)),
   ShowType '(t,r,r',s,s',zp,zq,gad),
   EncryptCtx t r r' (LiftOf zp) zp zq,
   EncryptCtx t s s' (LiftOf zp) zp zq,
   TunnelCtx t e r s e' r' s' (LiftOf zp) zp zq gad,
   e ~ FGCD r s,
   ZPP zp, Random zp,
   Fact e,
   CElt t (ZpOf zp))
data instance ArgsCtx TunnCtxD where
    TunnD :: (TunnCtx t r r' e e' s s' zp zq gad)
      => Proxy '(t,r,r',s,s',zp,zq,gad) -> ArgsCtx TunnCtxD
instance (params `Satisfy` TunnCtxD, TunnCtx t r r' e e' s s' zp zq gad)
  => ( '(gad, '(t, '( '(r,r',s,s'), '(zp,zq)))) ': params) `Satisfy` TunnCtxD where
  run _ f = f (TunnD (Proxy::Proxy '(t,r,r',s,s',zp,zq,gad))) : run (Proxy::Proxy params) f

applyTunn :: (params `Satisfy` TunnCtxD) =>
  Proxy params ->
  (forall t r r' e e' s s' zp zq gad . (TunnCtx t r r' e e' s s' zp zq gad)
       => Proxy '(t,r,r',s,s',zp,zq,gad) -> rnd res)
    -> [rnd res]
applyTunn params g = run params $ \(TunnD p) -> g p

data CTEmCtxD
-- union of compatible constraints in benchmarks
type CTEmCtx t r r' s s' zp zq =
  (Random zp, Eq zp,            -- CJP: added b/c CElt doesn't have them
   DecryptUCtx t r r' (LiftOf zp) zp zq,
   DecryptUCtx t s s' (LiftOf zp) zp zq,
   ShowType '(t,r,r',s,s',zp,zq),
   EncryptCtx t r r' (LiftOf zp) zp zq,
   r `Divides` s,
   r' `Divides` s',
   s `Divides` s',
   r ~ (FGCD r' s))
data instance ArgsCtx CTEmCtxD where
    TwEmD :: (CTEmCtx t r r' s s' zp zq)
      => Proxy '(t,r,r',s,s',zp,zq) -> ArgsCtx CTEmCtxD
instance (params `Satisfy` CTEmCtxD, CTEmCtx t r r' s s' zp zq)
  => ( '(t, '(r,r',s,s',zp,zq)) ': params) `Satisfy` CTEmCtxD where
  run _ f = f (TwEmD (Proxy::Proxy '(t,r,r',s,s',zp,zq))) : run (Proxy::Proxy params) f

applyCTTwEm :: (params `Satisfy` CTEmCtxD) =>
  Proxy params ->
  (forall t r r' s s' zp zq . (CTEmCtx t r r' s s' zp zq)
       => Proxy '(t,r,r',s,s',zp,zq) -> rnd res)
    -> [rnd res]
applyCTTwEm params g = run params $ \(TwEmD p) -> g p

-- allowed args: CT, KSHint, SK
-- context for (*), (==), decryptUnrestricted
data KSQCtxD
-- it'd be nice to make this associated to `Satsify`,
-- but we have to use a *ton* of kind signatures if we do
type KSQCtx gad t m m' zp zq zq' =
    (Random zp, Eq zp,          -- CJP: added b/c CElt doesn't have them
     EncryptCtx t m m' (LiftOf zp) zp zq,
     DecryptUCtx t m m' (LiftOf zp) zp zq,
     KeySwitchCtx gad t m' zp zq zq',
     KSHintCtx gad t m' (LiftOf zp) zq',
     -- ^ these provide the context to generate the parameters
     Ring (CT m zp (Cyc t m' zq)),
     -- Eq (Cyc t m zp),
     Fact m, Fact m', CElt t zp, m `Divides` m',
     Reduce (LiftOf zp) zq, Lift' zq, CElt t (LiftOf zp), ToSDCtx t m' zp zq, Reduce (LiftOf zq) zp,
     -- ^ these provide the context for tests
     NFData (CT m zp (Cyc t m' zq)),
     ShowType '(t,m,m',zp,zq,zq',gad))
     -- ^ these provide the context for benchmarks
data instance ArgsCtx KSQCtxD where
    KSQD :: (KSQCtx gad t m m' zp zq zq')
      => Proxy '(t,m,m',zp,zq,zq',gad) -> ArgsCtx KSQCtxD
instance (params `Satisfy` KSQCtxD, KSQCtx gad t m m' zp zq zq')
  => ( '(gad , '(t, '(m, m', zp, zq, zq'))) ': params) `Satisfy` KSQCtxD where
  run _ f = f (KSQD (Proxy::Proxy '(t,m,m',zp,zq,zq',gad))) : run (Proxy::Proxy params) f

applyKSQ :: (params `Satisfy` KSQCtxD) =>
  Proxy params ->
  (forall t m m' zp zq zq' gad . (KSQCtx gad t m m' zp zq zq')
     => Proxy '(t,m,m',zp,zq,zq',gad) -> rnd res)
  -> [rnd res]
applyKSQ params g = run params $ \(KSQD p) -> g p

data RescaleCtxD
type RescaleCtx t m m' zp zq zq' =
  (Random zp,
   EncryptCtx t m m' (LiftOf zp) zp zq',
   ShowType '(t,m,m',zp,zq,zq'),
   RescaleCyc (Cyc t) zq' zq,
   NFData (CT m zp (Cyc t m' zq)),
   ToSDCtx t m' zp zq')
data instance ArgsCtx RescaleCtxD where
    RD :: (RescaleCtx t m m' zp zq zq')
      => Proxy '(t,m,m',zp,zq,zq') -> ArgsCtx RescaleCtxD
instance (params `Satisfy` RescaleCtxD, RescaleCtx t m m' zp zq zq')
  => ( '(t, '(m,m',zp,zq,zq')) ': params) `Satisfy` RescaleCtxD where
  run _ f = f (RD (Proxy::Proxy '(t,m,m',zp,zq,zq'))) : run (Proxy::Proxy params) f

applyRescale :: (params `Satisfy` RescaleCtxD) =>
  Proxy params ->
  (forall t m m' zp zq zq' . (RescaleCtx t m m' zp zq zq')
    => Proxy '(t,m,m',zp,zq,zq') -> rnd res)
  -> [rnd res]
applyRescale params g = run params $ \(RD p) -> g p

data CTCtxD
-- union of compatible constraints in benchmarks
type CTCtx t m m' zp zq =
  (Random zp, Eq zp, NFElt zp, NFElt zq, -- CJP: CElt doesn't have these
   EncryptCtx t m m' (LiftOf zp) zp zq,
   Ring (CT m zp (Cyc t m' zq)),
   AddPublicCtx t m m' zp zq,
   DecryptUCtx t m m' (LiftOf zp) zp zq,
   MulPublicCtx t m m' zp zq,
   ShowType '(t,m,m',zp,zq))
data instance ArgsCtx CTCtxD where
    CTD :: (CTCtx t m m' zp zq)
      => Proxy '(t,m,m',zp,zq) -> ArgsCtx CTCtxD
instance (params `Satisfy` CTCtxD, CTCtx t m m' zp zq)
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` CTCtxD where
  run _ f = f (CTD (Proxy::Proxy '(t,m,m',zp,zq))) : run (Proxy::Proxy params) f

applyCTFunc :: (params `Satisfy` CTCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall t m m' zp zq . (CTCtx t m m' zp zq, Generatable (StateT (Maybe (SK (Cyc t m' (LiftOf zp)))) rnd) zp)
      => Proxy '(t,m,m',zp,zq) -> rnd res)
  -> [rnd res]
applyCTFunc params g = run params $ \(CTD p) -> g p

data EncCtxD
type EncCtx t m m' zp zq gen =
  (Random zp, NFElt zp, NFElt zq,
   EncryptCtx t m m' (LiftOf zp) zp zq,
   Ring (CT m zp (Cyc t m' zq)),
   AddPublicCtx t m m' zp zq,
   MulPublicCtx t m m' zp zq,
   ShowType '(t,m,m',zp,zq,gen),
   CryptoRandomGen gen)
data instance ArgsCtx EncCtxD where
    EncD :: (EncCtx t m m' zp zq gen)
      => Proxy '(t,m,m',zp,zq,gen) -> ArgsCtx EncCtxD
instance (params `Satisfy` EncCtxD, EncCtx t m m' zp zq gen)
  => ( '(gen, '(t, '(m,m',zp,zq))) ': params) `Satisfy` EncCtxD where
  run _ f = f (EncD (Proxy::Proxy '(t,m,m',zp,zq,gen))) : run (Proxy::Proxy params) f

applyEnc :: (params `Satisfy` EncCtxD) =>
  Proxy params
  -> (forall t m m' zp zq gen . (EncCtx t m m' zp zq gen)
      => Proxy '(t,m,m',zp,zq,gen) -> rnd res)
  -> [rnd res]
applyEnc params g = run params $ \(EncD p) -> g p
