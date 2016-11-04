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

module HomomPRFTests (keyHomom_test) where

import Control.Applicative
import Control.Monad.Random

import Crypto.Lol

import Crypto.Lol.Applications.HomomPRF
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Cyclotomic.Tensor
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as Lol
import Crypto.Lol.Types
import Crypto.Lol.Types.ZPP

import Data.Promotion.Prelude.List

import MathObj.Matrix

import Tests
import qualified Test.Framework as TF

{-
sheTests :: forall t m zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(m,zp,zq,gad) -> Proxy t -> [rnd TF.Test]
sheTests _ _ =
  let ptmr = Proxy::Proxy '(t,m,zp,zq,gad)
  in ($ ptmr) <$> [
   genTestArgs "PRF_3bits" (prop_KeyHomom 3),
   genTestArgs "PRF_5bits" (prop_KeyHomom 5)]
-}

keyHomom_test :: (MonadRandom rnd) => rnd TF.Test
keyHomom_test = hideArgs "homomPRF" (prop_keyHomom 5 (1.0 :: Double))
  (Proxy::Proxy '(Lol.CT, RngList, ZP8, ZQ4, ZQSeq, PRFGad, KSGad))

type H0 = F128
type H1 = F64 * F7
type H2 = F32 * F7 * F13
type H3 = F8 * F5 * F7 * F13
type H4 = F4 * F3 * F5 * F7 * F13
type H5 = F9 * F5 * F7 * F13
type H0' = H0 * F7 * F13
type H1' = H1 * F13
type H2' = H2
type H3' = H3
type H4' = H4
type H5' = H5
type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]

type Zq (q :: k) = ZqBasic q Int64
-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = (Zq 19393921, ZQ1)
type ZQ3 = (Zq 19918081, ZQ2)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ4 = (Zq 2149056001, ZQ3)
-- for rounding off after the first hop
type ZQ5 = (Zq 3144961, ZQ4)
type ZQ6 = (Zq 7338241, ZQ5)
type ZQSeq = '[ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]

type ZP8 = Zq PP8

-- these need not be the same
type KSGad = BaseBGad 2
type PRFGad = BaseBGad 2


-- +/-1 in every coefficient of the rounding basis
-- EAC: too complicated for PartialTypeSigs?
prop_keyHomom :: forall v t e r r' s s' rngs z zp zq zqs prfgad ksgad ptrngs .
  (ptrngs ~ PTRings rngs, Head ptrngs ~ r, s ~ Last ptrngs,
   '(r,r') ~ Head rngs, Last rngs ~ '(Fst (Last rngs), Snd (Last rngs)),
   '(s,s') ~ Last rngs,
   z ~ LiftOf zp,

   Random (Cyc t r zp),                               -- randomFamily
   Decompose (prfgad :: *) (Cyc t r zp),              -- prfState
   RescaleCyc (Cyc t) zp (TwoOf zp),                  -- ringPRF
   PTTunnel t ptrngs (TwoOf zp),                      -- tunnel
   GenSKCtx t r' z v,                                 -- genSK
   Tunnel rngs t z zp (ZqUp zq zqs) ksgad,            -- tunnelHints
   PTRound t s s' e zp (ZqDown zq zqs) z ksgad zqs,   -- roundHints
   EncryptCtx t r r' z zp zq,                         -- encrypt
   MulPublicCtx t r r' zp zq,                         -- homomPRF
   MultiTunnelCtx rngs r r' s s' t z zp zq ksgad zqs, -- homomPRF
   DecryptCtx t s s' z (TwoOf zp) (ZqResult e (ZqDown zq zqs) zqs), -- decrypt
   Eq (Cyc t s (TwoOf zp))                              -- ==
   )
  => Int -> v -> Test '(t,rngs,zp,zq,zqs,prfgad,ksgad)
prop_keyHomom size v = testIO $ do
  family :: PRFFamily prfgad _ _ <- randomFamily size
  s <- getRandom
  x <- ((`mod` (2^size)) . abs) <$> getRandom
  let st = prfState family Nothing
      prfclear = head $ head $ columns $ ringPRF s x st -- homomPRF only computes first elt
      hoppedClear = tunnel (Proxy::Proxy ptrngs) prfclear
  sk <- genSK v
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