{-|
Module      : Crypto.Lol.Applications.HomomPRF
Description : Homomorphic evaluation of the PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)

Homomorphic evaluation of the PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
-}

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
,RoundHints(..), roundHints
,TunnelInfoChain(..), tunnelInfoChain
,EvalHints(..)
,MultiTunnelCtx, ZqUp, ZqDown
,TwoOf
,Tunnel, PTRound, ZqResult
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
import qualified Crypto.Proto.HomomPRF.LinearFuncChain as P
import qualified Crypto.Proto.HomomPRF.RoundHintChain as P
import qualified Crypto.Proto.HomomPRF.TunnelInfoChain as P
import qualified Crypto.Proto.Lol.RqProduct as P
import qualified Crypto.Proto.SHE.TunnelInfo as P

import Data.List.Split (chunksOf)
import Data.Promotion.Prelude

import GHC.TypeLits hiding (type (*))

import MathObj.Matrix (columns)

import Data.Sequence (empty, (<|), ViewL(..), viewl)

-- | The element before @zq@ in type list @zqs@.
type ZqUp zq zqs = NextListElt zq (Reverse zqs)
-- | The element after @zq@ in type list @zqs@.
type ZqDown zq zqs = NextListElt zq zqs

type family NextListElt (x :: k) (xs :: [k]) :: k where
  NextListElt x (x ': y ': ys) = y
  NextListElt x '[x] = TypeError ('Text "There is no 'next'/'prev' list element for " ':<>: 'ShowType x ':<>: 'Text "!" ':$$:
                                  'Text "Try adding more moduli to the ciphertext modulus list")
  NextListElt x (y ': ys) = NextListElt x ys
  NextListElt x '[] = TypeError ('Text "Could not find type " ':<>: 'ShowType x ':<>: 'Text " in the list." ':$$:
                                 'Text "You must use parameters that are in the type lists!")

-- | The offline data needed for homomorphic PRF evaluation.
data EvalHints t rngs z zp zq zqs gad where
  Hints :: (UnPP (CharOf zp) ~ '(Prime2, e))
        => TunnelInfoChain gad t rngs zp (ZqUp zq zqs)
        -> RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad
        -> EvalHints t rngs z zp zq zqs gad

instance (UnPP (CharOf zp) ~ '(Prime2, e),
          NFData (TunnelInfoChain gad t rngs zp (ZqUp zq zqs)),
          NFData (RoundHints t (Fst (Last rngs)) (Snd (Last rngs)) z e zp (ZqDown zq zqs) zqs gad))
  => NFData (EvalHints t rngs z zp zq zqs gad) where
  rnf (Hints t r) = rnf t `seq` rnf r

-- | Monadic version of 'homomPRF'.
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

-- | Evaluates the PRF family indexed by the encrypted secret on the input,
-- relative to some PRF state. Note that the algorithm in
-- <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]> outputs a
-- /vector/; this function only outputs the encryption of the first coefficient
-- of that vector.
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

-- | \(\Z_2\) for ZqBasic with a 'PrimePower' modulus.
type family TwoOf (a :: k) :: k
type instance TwoOf (ZqBasic (q :: PrimePower) i) = ZqBasic PP2 i

-- For rounding
type family Div2 (a :: k) :: k
type instance Div2 (ZqBasic q i) = ZqBasic (Div2 q) i
type instance Div2 (pp :: PrimePower) = Head (UnF (PpToF pp / F2))

-- type-restricted versions of rescaleLinearCT
roundCTUp :: (RescaleCyc (Cyc t) zq (ZqUp zq zqs), ToSDCtx t m' zp zq)
  => Proxy zqs -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZqUp zq zqs))
roundCTUp _ = rescaleLinearCT

roundCTDown :: (RescaleCyc (Cyc t) zq (ZqDown zq zqs), ToSDCtx t m' zp zq)
  => Proxy zqs -> CT m zp (Cyc t m' zq) -> CT m zp (Cyc t m' (ZqDown zq zqs))
roundCTDown _ = rescaleLinearCT


















-- | Quadratic key switch hints for the rounding phase of PRF evaluation.
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

-- | Functions related to homomorphically rounding from 2^k to 2
class (UnPP (CharOf zp) ~ '(Prime2,e)) => PTRound t m m' e zp zq z gad zqs where
  type ZqResult e zq (zqs :: [*])

  -- | Generate hints for rounding from \(R_p=R_{2^k}\) to \(R_2\).
  roundHints :: (MonadRandom rnd)
             => SK (Cyc t m' z) -> rnd (RoundHints t m m' z e zp zq zqs gad)

  -- round(q/p*x) (with "towards infinity tiebreaking")
  -- = msb(x+q/4) = floor((x+q/4)/(q/2))
  -- | Round coeffs in CRT slots near 0 to 0 and near q/2 to 1, with a short-cut
  -- on the first level of the rounding tree.
  ptRound :: RoundHints t m m' z e zp zq zqs gad -> CT m zp (Cyc t m' zq) -> CT m (TwoOf zp) (Cyc t m' (ZqResult e zq zqs))

  -- | All levels other than the first level of the rounding tree.
  ptRoundInternal :: RoundHints t m m' z e zp zq zqs gad -> [CT m zp (Cyc t m' zq)] -> CT m (TwoOf zp) (Cyc t m' (ZqResult e zq zqs))

instance PTRound t m m' P1 (ZqBasic PP2 i) zq z gad zqs where
  type ZqResult P1 zq zqs = zq

  roundHints _ = return RHNil

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

  roundHints sk = do
    ksq <- ksQuadCircHint sk
    rest <- roundHints sk
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

-- | Sequence of 'TunnelInfo' for consecutive ring tunnels.
data TunnelInfoChain gad t xs zp zq where
  THNil :: TunnelInfoChain gad t '[ '(m,m') ] zp zq
  THCons :: (xs ~ ('(r,r') ': '(s,s') ': rngs), e' ~ (e * (r' / r)), e ~ FGCD r s)
         => TunnelInfo gad t e r s e' r' s' zp zq
         -> TunnelInfoChain gad t (Tail xs) zp zq
         -> TunnelInfoChain gad t xs zp zq

instance NFData (TunnelInfoChain gad t '[ '(m,m') ] zp zq) where
  rnf THNil = ()

instance (e' ~ (e * (r' / r)), e ~ FGCD r s,
          NFData (TunnelInfo gad t e r s e' r' s' zp zq),
          NFData (TunnelInfoChain gad t ('(s,s') ': rngs) zp zq))
  => NFData (TunnelInfoChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) where
  rnf (THCons t ts) = rnf t `seq` rnf ts

instance Protoable (TunnelInfoChain gad t '[ '(m,m') ] zp zq) where
  type ProtoType (TunnelInfoChain gad t '[ '(m,m') ] zp zq) = P.TunnelInfoChain
  toProto THNil = P.TunnelInfoChain empty
  fromProto (P.TunnelInfoChain xs) | xs == empty = return THNil
  fromProto _ = throwError $ "Got non-empty chain on fromProto for TunnelInfoChain"

instance (e' ~ (e * (r' / r)), e ~ FGCD r s,
          Protoable (TunnelInfo gad t e r s e' r' s' zp zq),
          Protoable (TunnelInfoChain gad t ('(s,s') ': rngs) zp zq),
          ProtoType (TunnelInfoChain gad t ('(s,s') ': rngs) zp zq) ~ P.TunnelInfoChain)
  => Protoable (TunnelInfoChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) where
  type ProtoType (TunnelInfoChain gad t ('(r,r') ': '(s,s') ': rngs) zp zq) = P.TunnelInfoChain
  toProto (THCons hint rest) =
    let h' = toProto hint
        (P.TunnelInfoChain hs') = toProto rest
    in P.TunnelInfoChain $ h' <| hs'

  fromProto (P.TunnelInfoChain zs) = do
    when (zs == empty) $ throwError "fromProto TunnelInfo: expected at least one element"
    let (x :< xs) = viewl zs
    y <- fromProto x
    ys <- fromProto $ P.TunnelInfoChain xs
    return $ THCons y ys

-- | Functions related to homomorphic ring tunneling.
class Tunnel xs t zp zq gad where

  -- | Generates 'TunnelInfo' for each tunnel step from @Head xs@ to @Last xs@.
  tunnelInfoChain :: (MonadRandom rnd, Head xs ~ '(r,r'), Last xs ~ '(s,s'),
                  Lift zp z, CElt t z, ToInteger z, Reduce z zq) -- constraints involving 'z' from GenTunnelInfoCtx
              => SK (Cyc t r' z) -> rnd (TunnelInfoChain gad t xs zp zq, SK (Cyc t s' z)) -- , TunnelFuncs t (PTRings xs) zp

  -- | Tunnel from @Head xs@ to @Last xs@.
  tunnelInternal :: (Head xs ~ '(r,r'), Last xs ~ '(s,s')) =>
    TunnelInfoChain gad t xs zp zq -> CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' zq)

instance Tunnel '[ '(m,m') ] t zp zq gad where

  tunnelInfoChain sk = return (THNil,sk)

  tunnelInternal _ = id

-- EAC: I expand the GenTunnelInfoCtx synonym here to remove all occurrences of 'z',
-- which instead only occur in the context of tunnelInfoChain. This is because 'z' is
-- not relevant for tunnelInternal nor TunnelInfoChain, so we would have to pass
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
instance (ExtendLinIdx e r s e' r' s', -- tunnelInfoChain
          e' ~ (e * (r' / r)),         -- tunnelInfoChain
          e' `Divides` r',             -- tunnelInfoChain
          CElt t zp, Ring zq, Random zq, CElt t zq,   -- tunnelInfoChain
          Reduce (DecompOf zq) zq, Gadget gad zq,     -- tunnelInfoChain
          NFElt zq, CElt t (DecompOf zq),             -- tunnelInfoChain
          TunnelCtx t r s e' r' s' zp zq gad,         -- tunnelCT
          e ~ FGCD r s, e `Divides` r, e `Divides` s, -- linearDec
          ZPP zp, TElt t (ZpOf zp),                   -- crtSet
          Tunnel ('(s,s') ': rngs) t zp zq gad,       -- recursive call
          Protoable (TunnelInfo gad t e r s e' r' s' zp zq),
          ProtoType (TunnelInfo gad t e r s e' r' s' zp zq) ~ P.TunnelInfo) -- toProto
  => Tunnel ('(r,r') ': '(s,s') ': rngs) t zp zq gad where

  tunnelInfoChain sk = do
    skout <- genSKWithVar sk
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
    thint :: TunnelInfo gad t e r s e' r' s' zp zq <- tunnelInfo linf skout sk
    (thints,sk') <- tunnelInfoChain skout
    return (THCons thint thints, sk')

  tunnelInternal (THCons thint rest) = tunnelInternal rest . tunnelCT thint

-- | Context for multi-step homomorphic tunneling.
type MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs =
  (Head rngs ~ '(r,r'), Last rngs ~ '(s,s'), Tunnel rngs t zp (ZqUp zq zqs) gad, ZqDown (ZqUp zq zqs) zqs ~ zq,
   RescaleCyc (Cyc t) zq (ZqUp zq zqs), RescaleCyc (Cyc t) (ZqUp zq zqs) zq, RescaleCyc (Cyc t) zq (ZqDown zq zqs),
   ToSDCtx t r' zp zq, ToSDCtx t s' zp (ZqUp zq zqs))

-- EAC: why round down twice? We bump the modulus up to begin with to handle
-- the key switches, so we knock thatt off, then another for accumulated noise
-- | End-to-end multi-step homomorphic tunneling. First add a modulus to the
-- ciphertext for the key switches, then 'tunnelInternal', then round down twice:
-- once to remove the key switch modulus, and once to dampen the noise incurred
-- while tunneling.
tunnel :: forall rngs r r' s s' t z zp zq gad zqs . (MultiTunnelCtx rngs r r' s s' t z zp zq gad zqs)
  => Proxy zqs -> TunnelInfoChain gad t rngs zp (ZqUp zq zqs) -> CT r zp (Cyc t r' zq) -> CT s zp (Cyc t s' (ZqDown zq zqs))
tunnel pzqs hints x =
  let y = tunnelInternal hints $ roundCTUp pzqs x
  in roundCTDown pzqs $ roundCTDown pzqs y




























-- | Linear functions used for in-the-clear tunneling.
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

-- | The plaintext rings for a list of plaintext/ciphertext ring pairs.
type family PTRings xs where
  PTRings '[] = '[]
  PTRings ( '(a,b) ': rest ) = a ': (PTRings rest)

-- | Functions for in-the-clear tunneling, needed to test the correctness of
-- homomorphic PRF evaluation.
class PTTunnel t xs zp where
  -- | Generate the linear functions to apply when tunneling from @Head xs@ to @Last xs@.
  ptTunnelFuncs :: TunnelFuncs t xs zp
  -- | Tunnel a ring element from @Head xs@ to @Last xs@.
  ptTunnel :: (Head xs ~ r, Last xs ~ s) => TunnelFuncs t xs zp -> Cyc t r zp -> Cyc t s zp

instance PTTunnel t '[r] z where
  ptTunnelFuncs = TFNil
  ptTunnel TFNil = id

instance (e ~ FGCD r s, e `Divides` r, e `Divides` s, PTTunnel t (s ': rngs) zp,
          CElt t zp,  -- evalLin
          ZPP zp, TElt t (ZpOf zp) -- crtSet
          )
  => PTTunnel t (r ': s ': rngs) zp where
  ptTunnelFuncs =
    let crts = proxy crtSet (Proxy::Proxy e)
        r = proxy totientFact (Proxy::Proxy r)
        e = proxy totientFact (Proxy::Proxy e)
        dim = r `div` e
        -- only take as many crts as we need
        -- otherwise linearDec fails
        linf = linearDec (take dim crts) :: Linear t zp e r s
        linfs = ptTunnelFuncs
    in TFCons linf linfs
  ptTunnel (TFCons linf fs) = ptTunnel fs . evalLin linf
