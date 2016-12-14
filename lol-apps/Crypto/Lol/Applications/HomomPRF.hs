{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Applications.HomomPRF
(homomPRF, homomPRFM
,RoundHints(..), genRoundHints
,TunnelHintChain(..), genTunnelHints
,EvalHints(..)
,MultiTunnelCtx, ZqUp, ZqDown
,TwoOf
,Tunnel, Fst, Snd, PTRound, ZqResult
,PTRings, PTTunnel(..), TunnelFuncs(..)) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Except
import Control.Monad.Random hiding (fromList)
import Control.Monad.Reader
import Control.Monad.State

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF
import Crypto.Lol.Reflects
import Crypto.Lol.Types

import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Types.ZPP
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types.Proto
import qualified Crypto.Proto.RLWE.RqProduct as P
import qualified Crypto.Proto.SHEHint.LinearFuncChain as P
import qualified Crypto.Proto.SHEHint.RoundHintChain as P
import qualified Crypto.Proto.SHEHint.TunnelHint as P
import qualified Crypto.Proto.SHEHint.TunnelHintChain as P

import Data.List.Split (chunksOf)
import Data.Promotion.Prelude.List

import GHC.TypeLits hiding (type (*))

import MathObj.Matrix (columns)

import Data.Sequence (empty, (<|), ViewL(..), viewl)

type ZqUp zq zqs = NextListElt zq (Reverse zqs)
type ZqDown zq zqs = NextListElt zq zqs

type family Fst (a :: (k1,k2)) :: k1 where
  Fst '(a,b) = a
type family Snd (a :: (k1,k2)) :: k2 where
  Snd '(a,b) = b

type family NextListElt (x :: k) (xs :: [k]) :: k where
  NextListElt x (x ': y ': ys) = y
  NextListElt x '[x] = TypeError ('Text "There is no 'next'/'prev' list element for " ':<>: 'ShowType x ':<>: 'Text "!" ':$$:
                                  'Text "Try adding more moduli to the ciphertext modulus list")
  NextListElt x (y ': ys) = NextListElt x ys
  NextListElt x '[] = TypeError ('Text "Could not find type " ':<>: 'ShowType x ':<>: 'Text " in the list." ':$$:
                                 'Text "You must use parameters that are in the type lists!")

data EvalHints t rngs z zp zq zqs gad where
  Hints :: (UnPP (CharOf zp) ~ '(Prime2, e))
        => TunnelHintChain gad t rngs zp (ZqUp zq zqs)
        -> RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad
        -> EvalHints t rngs z zp zq zqs gad

instance (UnPP (CharOf zp) ~ '(Prime2, e),
          NFData (TunnelHintChain gad t rngs zp (ZqUp zq zqs)),
          NFData (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad))
  => NFData (EvalHints t rngs z zp zq zqs gad) where
  rnf (Hints t r) = rnf t `seq` rnf r

homomPRFM ::
  (MonadReader (EvalHints t rngs z zp zq zqs gad) mon,
   MonadState (PRFState (Cyc t r zp) (Cyc t r (TwoOf zp))) mon,
   MulPublicCtx t r r' zp zq,
   MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs,
   PTRound t s s' e zp (ZqDown zq zqs) z gad zqs)
  => CT r zp (Cyc t r' zq)   -- encryption of PRF secret
     -> Int                  -- PRF input
     -> mon (CT s (TwoOf zp) (Cyc t s' (ZqResult e (ZqDown zq zqs) zqs)))
homomPRFM ct x = do
  hints <- ask
  state $ homomPRF' hints ct x

homomPRF :: (MulPublicCtx t r r' zp zq,
             MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs,
             PTRound t s s' e zp (ZqDown zq zqs) z gad zqs)
    => EvalHints t rngs z zp zq zqs gad
       -> CT r zp (Cyc t r' zq)
       -> Int
       -> PRFState (Cyc t r zp) (Cyc t r (TwoOf zp))
       -> CT s (TwoOf zp) (Cyc t s' (ZqResult e (ZqDown zq zqs) zqs))
homomPRF hs ct x = fst . homomPRF' hs ct x

homomPRF' :: forall t e r s r' s' rngs z zp zq zqs gad .
  (MulPublicCtx t r r' zp zq,
   MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs,
   PTRound t s s' e zp (ZqDown zq zqs) z gad zqs)
  => EvalHints t rngs z zp zq zqs gad
  -> CT r zp (Cyc t r' zq)
  -> Int
  -> PRFState (Cyc t r zp) (Cyc t r (TwoOf zp))
  -> (CT s (TwoOf zp) (Cyc t s' (ZqResult e (ZqDown zq zqs) zqs)),
      PRFState (Cyc t r zp) (Cyc t r (TwoOf zp)))
homomPRF' (Hints tHints rHints) ct x st =
  let (atx,st') = evalTree x st
      firstElt = head $ head $ columns atx
      ctMatrix1 = mulPublic firstElt ct
      ctMatrix2 = tunnel (Proxy::Proxy zqs) tHints ctMatrix1
  in (ptRound rHints ctMatrix2, st')

type family TwoOf (a :: k) :: k
type instance TwoOf (a :: PrimePower) = PP2
type instance TwoOf (ZqBasic q i) = ZqBasic (TwoOf q) i

-- For Rounding
type family Div2 (a :: k) :: k
type instance Div2 (ZqBasic q i) = ZqBasic (Div2 q) i
type instance Div2 (pp :: PrimePower) = Head (UnF (PpToF pp / F2))

roundCTUp :: (RescaleCyc (Cyc t) zq (ZqUp zq zqs), ToSDCtx t m' zp zq)
  => Proxy zqs -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZqUp zq zqs))
roundCTUp _ = rescaleLinearCT

roundCTDown :: (RescaleCyc (Cyc t) zq (ZqDown zq zqs), ToSDCtx t m' zp zq)
  => Proxy zqs -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZqDown zq zqs))
roundCTDown _ = rescaleLinearCT



















data RoundHints t m m' z e zp zq zqs gad where
  RHNil :: RoundHints t m m' z e zp zq zqs gad
  RHCons :: KSQuadCircHint gad (Cyc t m' (ZqUp zq zqs))
           -> RoundHints t m m' z e (Div2 zp) (ZqDown zq zqs) zqs gad
           -> RoundHints t m m' z ('S e) zp zq zqs gad

instance NFData (RoundHints t m m' z P1 zp zq zqs gad) where
  rnf RHNil = ()

instance (NFData (KSQuadCircHint gad (Cyc t m' (ZqUp zq zqs))),
          NFData (RoundHints t m m' z e (Div2 zp) (ZqDown zq zqs) zqs gad))
  => NFData (RoundHints t m m' z ('S e) zp zq zqs gad) where
  rnf (RHCons h hs) = rnf h `seq` rnf hs

instance Protoable (RoundHints t m m' z P1 zp zq zqs gad) where
  type ProtoType (RoundHints t m  m' z P1 zp zq zqs gad) = P.RoundHintChain
  toProto RHNil = P.RoundHintChain empty
  fromProto (P.RoundHintChain xs) | xs == empty = return RHNil
  fromProto _ = throwError $ "Got non-empty chain on fromProto for RoundHints"

instance (Protoable (KSQuadCircHint gad (Cyc t m' (ZqUp zq zqs))),
          Protoable (RoundHints t m m' z e (Div2 zp) (ZqDown zq zqs) zqs gad),
          ProtoType (RoundHints t m m' z e (Div2 zp) (ZqDown zq zqs) zqs gad) ~ P.RoundHintChain)
  => Protoable (RoundHints t m m' z ('S e) zp zq zqs gad) where
  type ProtoType (RoundHints t m m' z ('S e) zp zq zqs gad) = P.RoundHintChain
  toProto (RHCons f fs) =
    let f' = toProto f
        (P.RoundHintChain fs') = toProto fs
    in P.RoundHintChain $ f' <| fs'
  fromProto (P.RoundHintChain fs) = do
    when (fs == empty) $ throwError "fromProto RoundHints: expected at least one element"
    let (a :< as) = viewl fs
    b <- fromProto a
    bs <- fromProto $ P.RoundHintChain as
    return $ RHCons b bs

class (UnPP (CharOf zp) ~ '(Prime2,e)) => PTRound t m m' e zp zq z gad zqs where
  type ZqResult e zq (zqs :: [*])

  genRoundHints :: (MonadRandom rnd)
             => SK (Cyc t m' z) -> rnd (RoundHints t m m' z e zp zq zqs gad)

  -- round coeffs near 0 to 0 and near q/2 to 1
  -- round(q/p*x) (with "towards infinity tiebreaking")
  -- = msb(x+q/4) = floor((x+q/4)/(q/2))
  ptRound :: RoundHints t m m' z e zp zq zqs gad -> CT m zp (Cyc t m' zq) -> CT m (TwoOf zp) (Cyc t m' (ZqResult e zq zqs))

  ptRoundInternal :: RoundHints t m m' z e zp zq zqs gad -> [CT m zp (Cyc t m' zq)] -> CT m (TwoOf zp) (Cyc t m' (ZqResult e zq zqs))

instance PTRound t m m' P1 (ZqBasic PP2 i) zq z gad zqs where
  type ZqResult P1 zq zqs = zq

  genRoundHints _ = return RHNil

  ptRound RHNil x = x

  ptRoundInternal RHNil [x] = x

instance (UnPP p ~ '(Prime2, 'S e),                                                 -- superclass constraint
          zqup ~ ZqUp zq zqs, zq' ~ ZqDown zq zqs, zp ~ ZqBasic p i, zp' ~ Div2 zp, -- convenience synonyms
          AddPublicCtx t m m' zp zq, AddPublicCtx t m m' zp zq',                    -- addPublic
          KeySwitchCtx gad t m' zp zq zqup, KSHintCtx gad t m' z zqup,              -- for quadratic key switch
          Reflects p Int,                                                           -- value
          Ring (CT m zp (Cyc t m' zq)),                                             -- (*)
          RescaleCyc (Cyc t) zq zq', ToSDCtx t m' zp zq,                            -- rescaleLinearCT
          ModSwitchPTCtx t m' zp zp' zq',                                           -- modSwitchPT
          PTRound t m m' e zp' zq' z gad zqs,                                       -- recursive call
          Protoable (KSQuadCircHint gad (Cyc t m' (ZqUp zq zqs))))                  -- toProto
  => PTRound t m m' ('S e) (ZqBasic p i) (zq :: *) z gad zqs where
  type ZqResult ('S e) zq zqs = ZqResult e (ZqDown zq zqs) zqs

  genRoundHints sk = do
    ksq <- genKSQuadCircHint sk
    rest <- genRoundHints sk
    return $ RHCons ksq rest

  ptRound (RHCons ksqHint rest) x =
    let x' = addPublic one x
        xprod = rescaleLinearCT $ keySwitchQuadCirc ksqHint $ x*x'
        p = proxy value (Proxy::Proxy p)
        xs = map (\y->modSwitchPT $ addPublic (fromInteger $ y*(-y+1)) xprod) [1..] :: [CT m zp' (Cyc t m' zq')]
    in ptRoundInternal rest $ take (p `div` 4) xs

  ptRoundInternal (RHCons ksqHint rest) (xs :: [CT m (ZqBasic p i) (Cyc t m' zq)]) =
    let pairs = chunksOf 2 xs
        go [a,b] = modSwitchPT $ rescaleLinearCT $ keySwitchQuadCirc ksqHint $ a*b :: CT m zp' (Cyc t m' zq')
    in ptRoundInternal rest (map go pairs)







































-- For (homomorphic) tunneling

data TunnelHintChain gad t xs zp zq where
  THNil :: TunnelHintChain gad t '[ '(m,m') ] zp zq
  THCons :: (xs ~ ('(r,r') ': '(s,s') ': rngs), e' ~ (e * (r' / r)), e ~ FGCD r s)
         => TunnelHint gad t e' r' s' zp zq
         -> TunnelHintChain gad t (Tail xs) zp zq
         -> TunnelHintChain gad t xs zp zq

instance NFData (TunnelHintChain gad t '[ '(m,m') ] zp zq) where
  rnf THNil = ()

instance (e' ~ (e * (r' / r)), e ~ FGCD r s,
          NFData (TunnelHint gad t e' r' s' zp zq),
          NFData (TunnelHintChain gad t ('(s,s') ': rngs) zp zq))
  => NFData (TunnelHintChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) where
  rnf (THCons t ts) = rnf t `seq` rnf ts

instance Protoable (TunnelHintChain gad t '[ '(m,m') ] zp zq) where
  type ProtoType (TunnelHintChain gad t '[ '(m,m') ] zp zq) = P.TunnelHintChain
  toProto THNil = P.TunnelHintChain empty
  fromProto (P.TunnelHintChain xs) | xs == empty = return THNil
  fromProto _ = throwError $ "Got non-empty chain on fromProto for TunnelHintChain"

instance (e' ~ (e * (r' / r)), e ~ FGCD r s,
          Protoable (TunnelHint gad t e' r' s' zp zq),
          Protoable (TunnelHintChain gad t ('(s,s') ': rngs) zp zq),
          ProtoType (TunnelHintChain gad t ('(s,s') ': rngs) zp zq) ~ P.TunnelHintChain)
  => Protoable (TunnelHintChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) where
  type ProtoType (TunnelHintChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) = P.TunnelHintChain
  toProto (THCons hint rest) =
    let h' = toProto hint
        (P.TunnelHintChain hs') = toProto rest
    in P.TunnelHintChain $ h' <| hs'

  fromProto (P.TunnelHintChain zs) = do
    when (zs == empty) $ throwError "fromProto TunnelHint: expected at least one element"
    let (x :< xs) = viewl zs
    y <- fromProto x
    ys <- fromProto $ P.TunnelHintChain xs
    return $ THCons y ys

class Tunnel xs t zp zq gad where

  genTunnelHints :: (MonadRandom rnd, Head xs ~ '(r,r'), Last xs ~ '(s,s'),
                  Lift zp z, CElt t z, ToInteger z, Reduce z zq) -- constraints involving 'z' from GenTunnelHintCtx
              => SK (Cyc t r' z) -> rnd (TunnelHintChain gad t xs zp zq, SK (Cyc t s' z)) -- , TunnelFuncs t (PTRings xs) zp

  tunnelInternal :: (Head xs ~ '(r,r'), Last xs ~ '(s,s')) =>
    TunnelHintChain gad t xs zp zq -> CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)

instance Tunnel '[ '(m,m') ] t zp zq gad where

  genTunnelHints sk = return (THNil,sk)

  tunnelInternal _ = id

-- EAC: I expand the GenTunnelHintCtx synonym here to remove all occurrences of 'z',
-- which instead only occur in the context of genTunnelHints. This is because 'z' is
-- not relevant for tunnelInternal nor TunnelHintChain, so we would have to pass
-- a proxy for 'z'. This is a problem I have had many times, and still don't have a
-- good solution for: the class exists only to match on the type list, all of
-- of the other class params exist only so I can write constrints involving the changing
-- params (here, the cyc indices) and the static params (here, the moduli/base rings).
-- One solution is to split the class in two: one with a 'z' param, and one without.
-- Another option is to pass in a meaningless proxy for 'z' to functions that don't need it.
-- Neither of these are good solutions, so instead I took a third approach: factor 'z'
-- out of the constraint synonyms. This requires explicitly listing the remaining
-- constraints, which is also ugly.
-- The root of this problem seems to be that the functions hold constant some
-- parameters, but change others, while the constraint synonyms refer to all of
-- them, even though some are orthogonal constraints.
instance (ExtendLinIdx e r s e' r' s', -- genTunnelHints
          e' ~ (e * (r' / r)),         -- genTunnelHints
          e' `Divides` r',             -- genTunnelHints
          CElt t zp, Ring zq, Random zq, CElt t zq,   -- genTunnelHints
          Reduce (DecompOf zq) zq, Gadget gad zq,     -- genTunnelHints
          NFElt zq, CElt t (DecompOf zq),             -- genTunnelHints
          TunnelCtx t r s e' r' s' zp zq gad,         -- tunnelCT
          e ~ FGCD r s, e `Divides` r, e `Divides` s, -- linearDec
          ZPP zp, TElt t (ZpOf zp),                   -- crtSet
          Tunnel ('(s,s') ': rngs) t zp zq gad,       -- recursive call
          Protoable (TunnelHint gad t e' r' s' zp zq), ProtoType (TunnelHint gad t e' r' s' zp zq) ~ P.TunnelHint) -- toProto
  => Tunnel ('(r,r') ': '(s,s') ': rngs) t zp zq gad where

  genTunnelHints sk = do
    skout <- genSKWithVar sk
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
    thint :: TunnelHint gad t e' r' s' zp zq <- genTunnelHint linf skout sk
    (thints,sk') <- genTunnelHints skout
    return (THCons thint thints, sk')

  tunnelInternal (THCons thint rest) = tunnelInternal rest . tunnelCT thint

type MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs =
  (Head rngs ~ '(r,r'), Last rngs ~ '(s,s'), Tunnel rngs t zp (ZqUp zq zqs) gad, ZqDown (ZqUp zq zqs) zqs ~ zq,
   RescaleCyc (Cyc t) zq (ZqUp zq zqs), RescaleCyc (Cyc t) (ZqUp zq zqs) zq, RescaleCyc (Cyc t) zq (ZqDown zq zqs),
   ToSDCtx t r' zp zq, ToSDCtx t s' zp (ZqUp zq zqs))

-- EAC: why round down twice? We bump the modulus up to begin with to handle
-- the key switches, so we knock thatt off, then another for accumulated noise
tunnel :: forall rngs r r' s s' t z zp zq gad zqs . (MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs)
  => Proxy zqs -> TunnelHintChain gad t rngs zp (ZqUp zq zqs) -> CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' (ZqDown zq zqs))
tunnel pzqs hints x =
  let y = tunnelInternal hints $ roundCTUp pzqs x
  in roundCTDown pzqs $ roundCTDown pzqs y




























-- | Linear functions used for in-the-clear tunneling
data TunnelFuncs t xs zp where
  TFNil :: TunnelFuncs t '[m] zp
  TFCons :: (xs ~ (r ': s ': rngs))
    => Linear t zp (FGCD r s) r s
    -> TunnelFuncs t (Tail xs) zp
    -> TunnelFuncs t xs zp

instance NFData (TunnelFuncs t '[m] zp) where
  rnf TFNil = ()

instance (NFData (Linear t zp (FGCD r s) r s), NFData (TunnelFuncs t (s ': xs) zp))
  => NFData (TunnelFuncs t (r ': s ': xs) zp) where
  rnf (TFCons f fs) = rnf f `seq` rnf fs

instance Protoable (TunnelFuncs t '[m] zp) where
  type ProtoType (TunnelFuncs t '[m] zp) = P.LinearFuncChain
  toProto TFNil = P.LinearFuncChain empty
  fromProto (P.LinearFuncChain s) | s == empty = return TFNil
  fromProto _ = throwError $ "Got non-empty chain on fromProto for TunnelFuncs"

instance (ProtoType (t s zp) ~ P.RqProduct,
          Protoable (Linear t zp e r s),
          Protoable (TunnelFuncs t (s ': rngs) zp),
          ProtoType (TunnelFuncs t (s ': rngs) zp) ~ P.LinearFuncChain,
          Tensor t, e ~ FGCD r s, Fact e, Fact r, Fact s)
  => Protoable (TunnelFuncs t (r ': s ': rngs) zp) where
  type ProtoType (TunnelFuncs t (r ': s ': rngs) zp) = P.LinearFuncChain
  toProto (TFCons f fs) =
    let f' = toProto f
        (P.LinearFuncChain fs') = toProto fs
    in P.LinearFuncChain $ f' <| fs'
  fromProto (P.LinearFuncChain fs) = do
    when (fs == empty) $ throwError "fromProto TunnelFuncs: expected at least one element"
    let (a :< as) = viewl fs
    b <- fromProto a
    bs <- fromProto $ P.LinearFuncChain as
    return $ TFCons b bs

type family PTRings xs where
  PTRings '[] = '[]
  PTRings ( '(a,b) ': rest ) = a ': (PTRings rest)

class PTTunnel t xs zp where
  ptTunnelHints :: TunnelFuncs t xs zp
  ptTunnel :: (Head xs ~ r, Last xs ~ s) => TunnelFuncs t xs zp -> Cyc t r zp -> Cyc t s zp

instance PTTunnel t '[r] z where
  ptTunnelHints = TFNil
  ptTunnel TFNil = id

instance (e ~ FGCD r s, e `Divides` r, e `Divides` s, PTTunnel t (s ': rngs) zp,
          CElt t zp,  -- evalLin
          ZPP zp, TElt t (ZpOf zp) -- crtSet
          )
  => PTTunnel t (r ': s ': rngs) zp where
  ptTunnelHints =
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
        linfs = ptTunnelHints
    in TFCons linf linfs
  ptTunnel (TFCons linf fs) = ptTunnel fs . evalLin linf
