{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Cyclotomic.UCyc hiding (crtSet)
import Crypto.Lol.Reflects
import Crypto.Lol.Types hiding (CT)

import Tests

import qualified Test.Framework as TF

import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Cyclotomic.Tensor

import Data.List.Split (chunksOf)
import Data.Promotion.Prelude.List

import GHC.TypeLits hiding (type (*))

type Gad = BaseBGad 2

main :: IO ()
main = do
  let v = 1.0 :: Double
  x <- getRandom
  sk <- genSK v
  ct :: CT H0 ZP8 (Cyc RT H0' ZQ5) <- encrypt sk x
  res <- homomPRF v (Proxy::Proxy RngList) (Proxy::Proxy TrivGad) sk ct
  res `deepseq` return ()
  TF.defaultMain =<< (sequence
    [hideArgs
      "key homomorphism"
      (prop_keyHomom 10)
      (Proxy::Proxy '(RT, F32, Zq 2, Zq 64, Gad))] :: IO [TF.Test])













type ZQSeq = '[ZQ6, ZQ5, ZQ4, ZQ3, ZQ2, ZQ1]
type ZQUp zq = NextListElt zq (Reverse ZQSeq)
type ZQDown zq = NextListElt zq ZQSeq

type family NextListElt (x :: k) (xs :: [k]) :: k where
  NextListElt x (x ': y ': ys) = y
  NextListElt x '[x] = TypeError ('Text "There is no 'next'/'prev' list element for " ':<>: 'ShowType x ':<>: 'Text "!" ':$$:
                                  'Text "Try adding more moduli to the ciphertext modulus list")
  NextListElt x (y ': ys) = NextListElt x ys
  NextListElt x '[] = TypeError ('Text "Could not find type " ':<>: 'ShowType x ':<>: 'Text " in the list." ':$$:
                                 'Text "You must use parameters that are in the type lists!")

type RngList = '[ '(H0,H0'), '(H1,H1'), '(H2,H2'), '(H3,H3'), '(H4,H4'), '(H5,H5') ]

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

-- three 24-bit moduli, enough to handle rounding for p=32 (depth-4 circuit at ~17 bits per mul)
type ZQ1 = Zq 18869761
type ZQ2 = (Zq 19393921, ZQ1)
type ZQ3 = (Zq 19918081, ZQ2)
-- a 31-bit modulus, for rounding off after the last four hops
type ZQ4 = (Zq 2149056001, ZQ3)
-- for rounding off after the first hop
type ZQ5 = (Zq 3144961, ZQ4)
type ZQ6 = (Zq 7338241, ZQ5)


type Zq (q :: k) = ZqBasic q Int64

type ZP8 = Zq PP8

homomPRF :: (PTRound t s s' z e zp (ZQDown zq) gad,
             TunnelCtx' rngs r r' s s' t z zp zq v gad rnd)
  => v -> Proxy rngs -> Proxy (gad :: *) -> SK (Cyc t r' z) -> CT r zp (Cyc t r' zq) -> rnd (CT s (TwoOf zp) (Cyc t s' (ZqResult e (ZQDown zq))))
homomPRF v prngs pgad skin ctin = do
  (sk', ctout) <- tunnel pgad v prngs skin ctin
  ptRound pgad sk' ctout








type family TwoOf (a :: k) :: k
--type instance TwoOf (a :: Nat) = 2
--type instance TwoOf (a :: PrimeBin) = Prime2
type instance TwoOf (a :: PrimePower) = PP2
type instance TwoOf (ZqBasic q i) = ZqBasic (TwoOf q) i

-- For Rounding
type family Div2 (a :: k) :: k
type instance Div2 (ZqBasic q i) = ZqBasic (Div2 q) i
type instance Div2 (pp :: PrimePower) = Head (UnF (PpToF pp / F2))

-- EAC: Requires a strange use of `CharOf`
class (UnPP (CharOf zp) ~ '(Prime2,e)) => PTRound t m m' z e zp zq gad where
  type ZqResult e zq

  -- round coeffs near 0 to 0 and near q/2 to 1
  -- round(q/p*x) (with "towards infinity tiebreaking")
  -- = msb(x+q/4) = floor((x+q/4)/(q/2))
  ptRound :: (MonadRandom rnd) => Proxy gad -> SK (Cyc t m' z) -> CT m zp (Cyc t m' zq) -> rnd (CT m (TwoOf zp) (Cyc t m' (ZqResult e zq)))

  ptRoundInternal :: (MonadRandom rnd) => Proxy gad -> SK (Cyc t m' z) -> [CT m zp (Cyc t m' zq)] -> rnd (CT m (TwoOf zp) (Cyc t m' (ZqResult e zq)))

instance (UnPP p ~ '(Prime2, 'S e),                                         -- superclass constraint
          zqup ~ ZQUp zq, zq' ~ ZQDown zq, zp ~ ZqBasic p i, zp' ~ Div2 zp, -- convenience synonyms
          AddPublicCtx t m m' zp zq, AddPublicCtx t m m' zp zq',            -- addPublic
          Reflects p Int,                                                   -- value
          Ring (CT m zp (Cyc t m' zq)),                                     -- (*)
          KeySwitchCtx gad t m' zp zq zqup, KSHintCtx gad t m' z zqup,      -- keySwitchQuadCirc
          RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq,                    -- rescaleLinearCT
          ModSwitchPTCtx t m' zp zp' zq',                                   -- modSwitchPT
          PTRound t m m' z e zp' zq' gad)                                   -- recursive call
  => PTRound t m m' z ('S e) (ZqBasic p i) zq (gad :: *) where
  type ZqResult ('S e) zq = ZqResult e (ZQDown zq)

  ptRound pgad sk x = do
    ksq <- proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad, zqup))
    let x' = addPublic one x
        xprod = rescaleLinearCT $ ksq $ x*x'
        p = proxy value (Proxy::Proxy p)
        xs = map (\y->modSwitchPT $ addPublic (fromInteger $ y*(-y+1)) xprod) [1..] :: [CT m zp' (Cyc t m' zq')]
    ptRoundInternal pgad sk $ take (p `div` 4) xs

  ptRoundInternal pgad sk (xs :: [CT m (ZqBasic p i) (Cyc t m' zq)]) = do
    ksq <- proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad, zqup))
    let pairs = chunksOf 2 xs
        go [a,b] = modSwitchPT $ rescaleLinearCT $ ksq $ a*b :: CT m zp' (Cyc t m' zq')
    ptRoundInternal pgad sk (map go pairs)

instance PTRound t m m' z P1 (ZqBasic PP2 i) zq gad where
  type ZqResult P1 zq = zq

  ptRound _ _ x = return x

  ptRoundInternal _ _ [x] = return x



{-
-- EAC: Requires exposing 'PP constructor
class PTRound t m m' z zp zq gad where
  type ZqResult zp zq
  ptRound :: (MonadRandom rnd) => Proxy gad -> SK (Cyc t m' z) -> [CT m zp (Cyc t m' zq)] -> rnd (CT m (TwoOf zp) (Cyc t m' (ZqResult zp zq)))

instance (zp ~ ZqBasic ('PP '(Prime2, 'S e)) i, zq' ~ ZQUp zq,             -- convenience synonyms
          zp' ~ ZqBasic ('PP '(Prime2, e)) i,                                -- convenience synonyms
          Ring (CT m zp (Cyc t m' zq)),                                    -- (*)
          KeySwitchCtx gad t m' zp zq zq', KSHintCtx gad t m' z zq',       -- keySwitchQuadCirc
          RescaleCyc (Cyc t) zq (ZQDown zq), ToSDCtx t m' zp zq,           -- rescaleLinearCT
          ModSwitchPTCtx t m' zp zp' (ZQDown zq),                          -- modSwitchPT
          PTRound t m m' z (ZqBasic ('PP '(Prime2, e)) i) (ZQDown zq) gad, -- recursive call
          ZqResult zp zq ~ ZqResult zp' (ZQDown zq))                       -- recursive call
  => PTRound t m m' z (ZqBasic ('PP '(Prime2, 'S e)) i) zq (gad :: *) where
  type ZqResult (ZqBasic ('PP '(Prime2, 'S e)) i) zq = ZqResult (ZqBasic ('PP '(Prime2, e)) i) (ZQDown zq)
  ptRound pgad sk (xs :: [CT m (ZqBasic ('PP '(Prime2, 'S e)) i) (Cyc t m' zq)]) = do
    ksq <- proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad, ZQUp zq))
    let pairs = chunksOf 2 xs
        go [a,b] = modSwitchPT $ rescaleLinearCT $ ksq $ a*b :: CT m zp' (Cyc t m' (ZQDown zq))
    ptRound pgad sk (map go pairs)

instance PTRound t m m' z (ZqBasic ('PP '(Prime2, P1)) i) zq gad where
  type ZqResult (ZqBasic ('PP '(Prime2, P1)) i) zq = zq
  ptRound _ _ [x] = return x
-}

-- For tunneling

class Tunnel xs t z zp zq v gad where
  tunnelInternal :: (Head xs ~ '(r,r'), Last xs ~ '(s,s'), MonadRandom rnd) =>
    Proxy gad -> v -> Proxy xs -> SK (Cyc t r' z) -> CT r zp (Cyc t r' zq) -> rnd (SK (Cyc t s' z), CT s zp (Cyc t s' zq))

instance Tunnel '[ '(m,m') ] t z zp zq v gad where
  tunnelInternal _ _ _ sk ct = return (sk,ct)

instance (TunnelCtx t e r s e' r' s' z zp zq gad,                  -- tunnelCT
          e ~ FGCD r s, e `Divides` r, e `Divides` s,              -- linearDec
          ZPP zp, TElt t (ZpOf zp),                                -- crtSet
          GenSKCtx t s' z v,                                       -- genSK
          Tunnel ('(s,s') ': rngs) t z zp zq v gad)
  => Tunnel ('(r,r') ': '(s,s') ': rngs) t z zp zq v gad where
  tunnelInternal pgad v _ sk ct = do
    skout <- genSK v
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
    f <- proxyT (tunnelCT linf skout sk) pgad
    let ct' = f ct
    tunnelInternal pgad v (Proxy::Proxy ('(s,s') ': rngs)) skout ct'

-- EAC: Invalid warning on these functions reported as #12700
roundCTUp :: (RescaleCyc (Cyc t) zq (ZQUp zq), ToSDCtx t m' zp zq)
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZQUp zq))
roundCTUp = rescaleLinearCT

roundCTDown :: (RescaleCyc (Cyc t) zq (ZQDown zq), ToSDCtx t m' zp zq)
  => CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZQDown zq))
roundCTDown = rescaleLinearCT

type TunnelCtx' rngs r r' s s' t z zp zq v gad rnd =
  (Head rngs ~ '(r,r'), Last rngs ~ '(s,s'), Tunnel rngs t z zp (ZQUp zq) v gad, ZQDown (ZQUp zq) ~ zq,
   RescaleCyc (Cyc t) zq (ZQUp zq), RescaleCyc (Cyc t) (ZQUp zq) zq, RescaleCyc (Cyc t) zq (ZQDown zq),
   ToSDCtx t r' zp zq, ToSDCtx t s' zp (ZQUp zq),
   MonadRandom rnd)

-- EAC: why round down twice? We bump the modulus up to begin with to handle
-- the key switches, so we knock thatt off, then another for accumulated noise
tunnel :: (TunnelCtx' rngs r r' s s' t z zp zq v gad rnd)
  => Proxy gad -> v -> Proxy rngs -> SK (Cyc t r' z) -> CT r zp (Cyc t r' zq) -> rnd (SK (Cyc t s' z), CT s zp (Cyc t s' (ZQDown zq)))
tunnel pgad v prngs sk x = do
  (sk',y) <- tunnelInternal pgad v prngs sk $ roundCTUp x
  return (sk', roundCTDown $ roundCTDown y)
