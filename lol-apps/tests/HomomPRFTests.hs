{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module HomomPRFTests (homomPRFTests) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol

import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Tests
import Crypto.Lol.Types.ZPP

import Data.Promotion.Prelude.List

import MathObj.Matrix

import qualified Test.Framework as TF

homomPRFTests :: forall t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs rnd .
  (MonadRandom rnd, Random (SK (Cyc t r' z)),
   C t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs)
  => Int -> Proxy '(rngs,zp,zq,zqs,prfgad,ksgad) -> Proxy t -> [rnd TF.Test]
homomPRFTests size _ _ =
  let ptmr = Proxy::Proxy '(t,rngs,zp,zq,zqs,prfgad,ksgad)
  in [genTestArgs "matches in-the-clear" (prop_keyHomom size) ptmr]

type C t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs =
  (ptrngs ~ PTRings rngs, Head ptrngs ~ r, s ~ Last ptrngs,
   '(r,r') ~ Head rngs, Last rngs ~ '(Fst (Last rngs), Snd (Last rngs)),
   '(s,s') ~ Last rngs,
   z ~ LiftOf zp,

   Random (Cyc t r zp),                               -- randomFamily
   Decompose (prfgad :: *) (Cyc t r zp),              -- prfState
   RescaleCyc (Cyc t) zp (TwoOf zp),                  -- ringPRF
   PTTunnel t ptrngs (TwoOf zp),                      -- tunnel
   Tunnel rngs t z zp (ZqUp zq zqs) ksgad,            -- tunnelHints
   PTRound t s s' e zp (ZqDown zq zqs) z ksgad zqs,   -- roundHints
   EncryptCtx t r r' z zp zq,                         -- encrypt
   MulPublicCtx t r r' zp zq,                         -- homomPRF
   MultiTunnelCtx rngs r r' s s' t z zp zq ksgad zqs, -- homomPRF
   DecryptCtx t s s' z (TwoOf zp) (ZqResult e (ZqDown zq zqs) zqs), -- decrypt
   Eq (Cyc t s (TwoOf zp))                              -- ==
   )

-- +/-1 in every coefficient of the rounding basis
-- EAC: too complicated for PartialTypeSigs?
prop_keyHomom :: forall t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs .
  (C t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs)
  => Int -> Cyc t r zp -> SK (Cyc t r' z) -> Test '(t,rngs,zp,zq,zqs,prfgad,ksgad)
prop_keyHomom size s sk = testIO $ do
  family :: PRFFamily prfgad _ _ <- randomFamily size
  x <- ((`mod` (2^size)) . abs) <$> getRandom
  let st = prfState family Nothing
      prfclear = head $ head $ columns $ ringPRF s x st -- homomPRF only computes first elt
      hoppedClear = tunnel (Proxy::Proxy ptrngs) prfclear
  (tHints, skout) <- tunnelHints sk
  rHints <- roundHints skout
  let hints = Hints tHints rHints :: EvalHints t rngs z zp zq zqs ksgad
  ct <- encrypt sk s
  let encPRF = homomPRF hints ct x st
      decPRF = decrypt skout encPRF
  return $ decPRF == hoppedClear

type family PTRings xs where
  PTRings '[] = '[]
  PTRings ( '(a,b) ': rest ) = a ': (PTRings rest)

class PTTunnel t xs zp where
  tunnel :: (Head xs ~ r, Last xs ~ s) => Proxy xs -> Cyc t r zp -> Cyc t s zp

instance PTTunnel t '[r] z where
  tunnel _ = id

instance (e ~ FGCD r s, e `Divides` r, e `Divides` s, PTTunnel t (s ': rngs) zp,
          CElt t zp,  -- evalLin
          ZPP zp, TElt t (ZpOf zp) -- crtSet
          )
  => PTTunnel t (r ': s ': rngs) zp where
  tunnel _ =
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
    in tunnel (Proxy::Proxy (s ': rngs)) . evalLin linf