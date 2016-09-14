{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs,
             MultiParamTypeClasses, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances #-}

module GenArgs.SHE where

import GenArgs

import Control.Applicative
import Control.Monad.Random
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZPP

--extract an SK type from a tuple of params
type family SKOf (a :: k) :: * where
  SKOf '(t,m,m',zp,zq)         = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq')     = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,m,m',zp,zq,zq',gad) = SK (Cyc t m' (LiftOf zp))
  SKOf '(t,r,r',s,s',zp,zq)    = SK (Cyc t r' (LiftOf zp))
  SKOf '(t,r,r',s,s',zp,zq,gad) = SK (Cyc t r' (LiftOf zp))
  SKOf '(t,'(m,m',zp,zp',zq)) = SK (Cyc t m' (LiftOf zp))

-- generates a secrete key with svar=1, using non-cryptographic randomness
instance (GenSKCtx t m z Double,
          MonadRandom rnd,
          MonadState (Maybe (SK (Cyc t m z))) rnd)
  => Generatable rnd (SK (Cyc t m z)) where
  genArg = do
    msk <- get
    case msk of
      Just sk -> return sk
      Nothing -> do
        sk <- genSK (1 :: Double)
        put $ Just sk
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
    Tunnel <$> proxyT (tunnelCT linf skout skin) (Proxy::Proxy gad)

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
