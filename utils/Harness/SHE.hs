{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances,
             GADTs, MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, RankNTypes,
             RebindableSyntax, ScopedTypeVariables, 
             TypeFamilies, TypeOperators, UndecidableInstances #-}

module Harness.SHE 
(KSHint(..)
,Tunnel(..)
,KSLinear(..)
,PTCT(..)
,SKOf
,AddZq
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
)where

import Utils
import Gen
import Apply

import Control.Applicative
import Control.DeepSeq
import Control.Monad.Random
import Control.Monad.State

import Crypto.Lol hiding (CT)
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Linear
import Crypto.Lol.Types.ZPP
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT

import Crypto.Random.DRBG

import Data.Singletons
import Data.Promotion.Prelude.List
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar

--extract an SK type from a tuple of params
type family SKOf (a :: k) :: * where
  SKOf '(t,m,m',zp,zq)         = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq')     = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq',gad) = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,r,r',s,s',zp,zq)    = SK (Cyc t r' (LiftOf zp))
  SKOf '(t,r,r',s,s',zp,zq,gad) = SK (Cyc t r' (LiftOf zp))
  SKOf '(t,'(m,m',zp,zp',zq)) = SK (Cyc t m' (LiftOf zp))

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
instance (params `Satisfy` DecCtxD, DecCtx t m m' zp zq)
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` DecCtxD where
  data ArgsCtx DecCtxD where
    DecD :: (DecCtx t m m' zp zq) 
      => Proxy '(t,m,m',zp,zq) -> ArgsCtx DecCtxD
  run _ f = (f $ DecD (Proxy::Proxy '(t,m,m',zp,zq))) : (run (Proxy::Proxy params) f)

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
instance (params `Satisfy` TunnCtxD, TunnCtx t r r' e e' s s' zp zq gad) 
  => ( '(gad, '(t, '( '(r,r',s,s'), '(zp,zq)))) ': params) `Satisfy` TunnCtxD where
  data ArgsCtx TunnCtxD where
    TunnD :: (TunnCtx t r r' e e' s s' zp zq gad) 
      => Proxy '(t,r,r',s,s',zp,zq,gad) -> ArgsCtx TunnCtxD
  run _ f = (f $ TunnD (Proxy::Proxy '(t,r,r',s,s',zp,zq,gad))) : (run (Proxy::Proxy params) f)

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
instance (params `Satisfy` CTEmCtxD, CTEmCtx t r r' s s' zp zq) 
  => ( '(t, '(r,r',s,s',zp,zq)) ': params) `Satisfy` CTEmCtxD where
  data ArgsCtx CTEmCtxD where
    TwEmD :: (CTEmCtx t r r' s s' zp zq) 
      => Proxy '(t,r,r',s,s',zp,zq) -> ArgsCtx CTEmCtxD
  run _ f = (f $ TwEmD (Proxy::Proxy '(t,r,r',s,s',zp,zq))) : (run (Proxy::Proxy params) f)

applyCTTwEm :: (params `Satisfy` CTEmCtxD, MonadRandom rnd) =>
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
type family KSQCtx a where
  KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))) = 
    (Random zp, Eq zp,          -- CJP: added b/c CElt doesn't have them
     EncryptCtx t m m' (LiftOf zp) zp zq,
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

instance (params `Satisfy` KSQCtxD, KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
  => ( '(gad , '(t, '(m, m', zp, zq, zq'))) ': params) `Satisfy` KSQCtxD where
  data ArgsCtx KSQCtxD where
    KSQD :: (KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
      => Proxy '(t,m,m',zp,zq,zq',gad) -> ArgsCtx KSQCtxD
  run _ f = (f $ KSQD (Proxy::Proxy '(t,m,m',zp,zq,zq',gad))) : (run (Proxy::Proxy params) f)

applyKSQ :: (params `Satisfy` KSQCtxD) => 
  Proxy params ->
  (forall t m m' zp zq zq' gad . (KSQCtx '(gad, '(t, '(m,m',zp,zq,zq'))))
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
instance (params `Satisfy` RescaleCtxD, RescaleCtx t m m' zp zq zq') 
  => ( '(t, '(m,m',zp,zq,zq')) ': params) `Satisfy` RescaleCtxD where
  data ArgsCtx RescaleCtxD where
    RD :: (RescaleCtx t m m' zp zq zq') 
      => Proxy '(t,m,m',zp,zq,zq') -> ArgsCtx RescaleCtxD
  run _ f = (f $ RD (Proxy::Proxy '(t,m,m',zp,zq,zq'))) : (run (Proxy::Proxy params) f)

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
instance (params `Satisfy` CTCtxD, CTCtx t m m' zp zq) 
  => ( '(t, '(m,m',zp,zq)) ': params) `Satisfy` CTCtxD where
  data ArgsCtx CTCtxD where
    CTD :: (CTCtx t m m' zp zq) 
      => Proxy '(t,m,m',zp,zq) -> ArgsCtx CTCtxD
  run _ f = (f $ CTD (Proxy::Proxy '(t,m,m',zp,zq))) : (run (Proxy::Proxy params) f)

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
instance (params `Satisfy` EncCtxD, EncCtx t m m' zp zq gen) 
  => ( '(gen, '(t, '(m,m',zp,zq))) ': params) `Satisfy` EncCtxD where
  data ArgsCtx EncCtxD where
    EncD :: (EncCtx t m m' zp zq gen) 
      => Proxy '(t,m,m',zp,zq,gen) -> ArgsCtx EncCtxD
  run _ f = (f $ EncD (Proxy::Proxy '(t,m,m',zp,zq,gen))) : (run (Proxy::Proxy params) f)

applyEnc :: (params `Satisfy` EncCtxD) =>
  Proxy params
  -> (forall t m m' zp zq gen . (EncCtx t m m' zp zq gen) 
      => Proxy '(t,m,m',zp,zq,gen) -> rnd res)
  -> [rnd res]
applyEnc params g = run params $ \(EncD p) -> g p






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

instance (Generatable rnd (PTCT m zp (Cyc t m' zq)), Monad rnd) 
  => Generatable rnd (CT m zp (Cyc t m' zq)) where
  genArg = do
    (PTCT _ ct) :: PTCT m zp (Cyc t m' zq) <- genArg
    return ct

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

newtype Tunnel t r r' s s' zp zq gad = Tunnel (CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq))
instance (Generatable rnd (SK (Cyc t r' z)),
          z ~ LiftOf zp,
          TunnelCtx t e r s e' r' s' z zp zq gad, 
          e ~ FGCD r s,
          ZPP zp,
          Fact e,
          CElt t (ZpOf zp),
          MonadRandom rnd,
          Generatable (StateT (Maybe (SK (Cyc t s' z))) rnd) (SK (Cyc t s' z)))
  => Generatable rnd (Tunnel t r r' s s' zp zq gad) where
  genArg = do
    skin :: SK (Cyc t r' z) <- genArg
    -- EAC: bit of a hack for now
    skout <- evalStateT genArg (Nothing :: Maybe (SK (Cyc t s' z)))
    let crts :: [Cyc t s zp] = proxy crtSet (Proxy::Proxy e)\\ gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf :: Linear t zp e r s = linearDec (take dim crts) \\ gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)
    f <- proxyT (tunnelCT linf skout skin) (Proxy::Proxy gad)
    return $ Tunnel f

data KSLinear t m m' z zp zq (zq' :: *) (gad :: *) = KSL (CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' zq)) (SK (Cyc t m' z))
instance (KeySwitchCtx gad t m' zp zq zq', 
          KSHintCtx gad t m' z zq', 
          MonadRandom rnd,
          Generatable rnd (SK (Cyc t m' z)), -- for skin
          Generatable (StateT (Maybe (SK (Cyc t m' z))) rnd) (SK (Cyc t m' z))) -- for skout
  => Generatable rnd (KSLinear t m m' z zp zq zq' gad) where
  genArg = do
    skin <- genArg
    -- generate an independent key
    skout <- evalStateT genArg (Nothing :: Maybe (SK (Cyc t m' z)))
    ksl <- proxyT (keySwitchLinear skout skin) (Proxy::Proxy (gad,zq'))
    return $ KSL ksl skout

data PTCT m zp rq where
  PTCT :: Cyc t m zp -> CT m zp (Cyc t m' zq) -> PTCT m zp (Cyc t m' zq)
instance (EncryptCtx t m m' z zp zq,
          z ~ LiftOf zp,
          MonadRandom rnd,
          Generatable rnd (SK (Cyc t m' z)),
          Generatable rnd (Cyc t m zp),
          rq ~ Cyc t m' zq) 
  => Generatable rnd (PTCT m zp rq) where
  genArg = do
    sk :: SK (Cyc t m' z) <- genArg
    pt <- genArg
    ct <- encrypt sk pt
    return $ PTCT pt ct
