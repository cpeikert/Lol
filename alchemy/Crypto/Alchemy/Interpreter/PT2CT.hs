{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Alchemy.Interpreter.PT2CT
( PT2CT, PNoiseTag
, pt2ct, encrypt, decrypt
, KSPNoise
) where

import Control.Applicative
import Control.Monad.Random
import Control.Monad.Reader
import Data.Dynamic
import Data.Type.Natural    hiding ((:+))
import GHC.TypeLits         hiding (type (*), Nat)

import           Crypto.Lol                      hiding (Pos (..))
import qualified Crypto.Lol                      as Lol
import           Crypto.Lol.Applications.SymmSHE hiding (AddPublicCtx,
                                                  MulPublicCtx, TunnelCtx,
                                                  decrypt, encrypt)
import qualified Crypto.Lol.Applications.SymmSHE as SHE
import           Crypto.Lol.Types

import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.SHE            as LSHE
import Crypto.Alchemy.Language.LinearCyc
import Crypto.Alchemy.MonadAccumulator

-- | Interprets plaintext operations as their corresponding
-- (homomorphic) ciphertext operations.  The represented plaintext
-- types should have the form 'PNoiseTag h (Cyc t m zp)'.
newtype PT2CT
  m'map    -- | list (map) of (plaintext index m, ciphertext index m')
  zqs      -- | list of pairwise coprime Zq components for ciphertexts
  gad      -- | gadget type for key-switch hints
  z        -- | integral type for secret keys
  ctex     -- | interpreter of ciphertext operations
  mon      -- | monad for creating keys/noise
  e        -- | environment
  a        -- | plaintext type; should be of the form 'PNoiseTag h (Cyc t m zp)'
  = PC { unPC :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)) }

-- | Transform a plaintext expression to a ciphertext expression.
pt2ct :: forall m'map zqs gad z e ctex a mon .
  (MonadAccumulator Keys mon, MonadAccumulator Hints mon) =>
      -- this forall is for use with TypeApplications at the top level
  Double   -- | Gaussian parameter \( r \) of decoding-basis coeffs of
           -- keys/errors.  (Scaled variance over \( R^\vee \) is \( r
           -- / \sqrt{\varphi(m')} \).)
  -> PT2CT m'map zqs gad z ctex (ReaderT Double mon) e a -- | plaintext expression
  -> mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)) -- | (monadic) ctex expression
pt2ct r = flip runReaderT r . unPC

svar :: (Fact m') => Proxy m' -> Double -> Double
svar pm' r = r / (sqrt $ fromIntegral $ proxy totientFact pm')

-- | Encrypt a plaintext (using the given scaled variance) under an
-- appropriate key (from the monad), generating one if necessary.
encrypt :: forall mon t m m' zp zq z .
  (MonadRandom mon, MonadAccumulator Keys mon,
   -- CJP: DON'T LOVE THIS CHOICE OF z HERE; IT'S ARBITRARY
   EncryptCtx t m m' z zp zq, z ~ LiftOf zp, GenSKCtx t m' z Double,
   Typeable t, Typeable m', Typeable z)
  => Double -- | Gaussian parameter of decoding-basis coeffs of errors
  -> Cyc t m zp                  -- | plaintext
  -> mon (CT m zp (Cyc t m' zq)) -- | (monadic) ciphertext
encrypt r x =
  let v = svar (Proxy::Proxy m') r
  in flip runReaderT v $ do
    -- generate key if necessary
    (sk :: SK (Cyc t m' z)) <- getKey
    SHE.encrypt sk x

-- | Decrypt a ciphertext under an appropriate key (from the monad),
-- if one exists.
decrypt :: forall mon t m m' z zp zq .
  (MonadReader Keys mon,
   -- CJP: DON'T LOVE THIS CHOICE OF z HERE; IT'S ARBITRARY
   DecryptCtx t m m' z zp zq, z ~ LiftOf zp,
   Typeable t, Typeable m', Typeable z)
  => CT m zp (Cyc t m' zq) -> mon (Maybe (Cyc t m zp))
decrypt x = do
  sk :: Maybe (SK (Cyc t m' z)) <- lookupKey
  return $ flip SHE.decrypt x <$> sk

instance (Lambda ctex, Applicative mon)
  => Lambda (PT2CT m'map zqs gad z ctex mon) where

  lam (PC f) = PC $ lam <$> f
  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

instance (List ctex, Applicative mon)
  => List (PT2CT m'map zqs gad z ctex mon) where
  nil_  = PC $ pure nil_
  cons_ = PC $ pure cons_

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs gad z ctex mon) a where

  add_ = PC $ pure add_
  neg_ = PC $ pure neg_

instance (SHE ctex, Applicative mon,
          AddPublicCtx ctex (Cyc2CT m'map zqs (PNoiseTag h (Cyc t m zp)))) =>
  AddLit (PT2CT m'map zqs gad z ctex mon) (PNoiseTag h (Cyc t m zp)) where

  addLit_ (PTag a) = PC $ pure $ addPublic_ a

instance (SHE ctex, Applicative mon,
          MulPublicCtx ctex (Cyc2CT m'map zqs (PNoiseTag h (Cyc t m zp)))) =>
  MulLit (PT2CT m'map zqs gad z ctex mon) (PNoiseTag h (Cyc t m zp)) where

  mulLit_ (PTag a) = PC $ pure $ mulPublic_ a
-- EAC: BUG!/FIXME
-- Previously: ZqPairsWithUnits zqs (KSPNoise gad zqs (PNoise2Units p))
type PNoise2KSZq gad zqs p = ZqPairsWithUnits zqs (PNoise2Units (KSPNoise gad zqs p))

-- | pNoise of a key-switch hint for a particular gadget, given the
-- pNoise of the input ciphertext.
type family KSPNoise gad (zqs :: [*]) (p :: PNoise) :: PNoise -- PNoise to PNoise
-- EAC: FIXME: we are adding "units" to a PNoise here, not sure if that's what you meant.
-- For simplicity, MaxUnits
-- returns a Nat, but it should probably return Units for safety. Then we add 2
-- to Units to get Units, and then we try to add a PNoise, which should be an error.
type instance KSPNoise TrivGad      zqs p = p :+ (N2 :+: MaxUnits zqs (PNoise2Units p))
type instance KSPNoise (BaseBGad 2) zqs p = p :+ N2

-- The pNoise for the key-switch hint depends on the gadget, so we define
-- gadget-specifc instances of Mul below

type PT2CTMulCtx m'map p zqs m zp gad ctex t z mon =
  PT2CTMulCtx' m zp p zqs gad (PNoise2KSZq gad zqs p) ctex t z mon (Lookup m m'map)

type PT2CTMulCtx' m zp p zqs gad hintzq ctex t z mon m' =
  PT2CTMulCtx'' p zqs gad hintzq ctex t z mon m' (CT m zp (Cyc t m'
  -- EAC: BUG!/FIXME
  -- Previously: (PNoise2Zq zqs (Units2PNoise (TotalUnits zqs (p :+: N3))))))
  -- but this asks for the TotalUnits of a PNoise
    (PNoise2Zq zqs (Units2PNoise (TotalUnits zqs (PNoise2Units (p :+ N3)))))))
    (CT m zp (Cyc t m' hintzq))

type PT2CTMulCtx'' p zqs gad hintzq ctex t z mon m' ctin hintct =
  (Lambda ctex, Mul ctex ctin, PreMul ctex ctin ~ ctin, SHE ctex,
   ModSwitchCtx ctex ctin hintzq, -- zqin -> zq (final modulus)
   ModSwitchCtx ctex hintct (PNoise2Zq zqs p), -- zqin -> zq (final modulus)
   KeySwitchQuadCtx ctex hintct gad, -- hint over zqin
   KSHintCtx gad t m' z hintzq,
   GenSKCtx t m' z Double,
   Typeable (Cyc t m' z), Typeable (KSQuadCircHint gad (Cyc t m' hintzq)),
   MonadRandom mon, MonadReader Double mon,
   MonadAccumulator Keys mon, MonadAccumulator Hints mon)

instance (PT2CTMulCtx m'map p zqs m zp gad ctex t z mon)
  => Mul (PT2CT m'map zqs gad z ctex mon) (PNoiseTag p (Cyc t m zp)) where

  type PreMul (PT2CT m'map zqs gad z ctex mon) (PNoiseTag p (Cyc t m zp)) =
    -- EAC: BUG!/FIXME
    -- Previously: (PNoise2Zq zqs (Units2PNoise (TotalUnits zqs (p :+: N3))))))
    -- but this asks for the TotalUnits of a PNoise
    -- Note that this must be fixed in the same way as the problem in PT2CTMulCtx'
    PNoiseTag (Units2PNoise (TotalUnits zqs (PNoise2Units (p :+ N3)))) (Cyc t m zp)

  mul_ :: forall m' env pin hintzq .
    (-- EAC: BUG! (same as previous two)
     pin ~ Units2PNoise (TotalUnits zqs (PNoise2Units (p :+ N3))),
     hintzq ~ PNoise2KSZq gad zqs p,
     m' ~ Lookup m m'map,
     PT2CTMulCtx m'map p zqs m zp gad ctex t z mon) =>
    PT2CT m'map zqs gad z ctex mon env
    (PNoiseTag pin (Cyc t m zp) -> PNoiseTag pin (Cyc t m zp) -> PNoiseTag p (Cyc t m zp))
  mul_ = PC $ do
    hint :: KSQuadCircHint gad (Cyc t m' hintzq) <-
      -- the reader stores r, so use errors with svar = r/sqrt(phi(m'))
      local (svar (Proxy::Proxy m')) $ getQuadCircHint (Proxy::Proxy z)
    return $ lam $ lam $
      modSwitch_ $:
      (keySwitchQuad_ hint $:
        (modSwitch_ $:
         (v1 *: v0 :: ctex _ (CT m zp (Cyc t m' (PNoise2Zq zqs pin)))))
        :: ctex _ (CT m zp (Cyc t m' hintzq)))

instance (SHE ctex, Applicative mon,
          LSHE.ModSwitchPTCtx ctex
           (CT m (ZqBasic ('PP '(Prime2, 'Lol.S e)) i) (Cyc t (Lookup m m'map) (PNoise2Zq zqs p)))
           (ZqBasic ('PP '(Prime2, e)) i)) =>
  Div2 (PT2CT m'map zqs gad z ctex mon)
  (PNoiseTag p (Cyc t m (ZqBasic ('PP '(Prime2, e)) i))) where

  type PreDiv2 (PT2CT m'map zqs gad z ctex mon)
       (PNoiseTag p (Cyc t m (ZqBasic ('PP '(Prime2, e)) i))) =
    PNoiseTag p (Cyc t m (ZqBasic ('PP '(Prime2, 'Lol.S e)) i))

  div2_ = PC $ pure modSwitchPT_

type PT2CTLinearCtx ctex mon m'map zqs p t e r s r' s' z zp zq zqin gad =
  PT2CTLinearCtx' ctex mon m'map zqs p t e r s r' s' z zp zq zqin (PNoise2KSZq gad zqs p) gad

type PT2CTLinearCtx' ctex mon m'map zqs p t e r s r' s' z zp zq zqin hintzq gad =
  (SHE ctex, Lambda ctex, Fact s',
   MonadAccumulator Keys mon, MonadRandom mon, MonadReader Double mon,
   -- output ciphertext type
   CT s zp (Cyc t s' zq)   ~ Cyc2CT m'map zqs (PNoiseTag p (Cyc t s zp)),
   -- input ciphertext type
   CT r zp (Cyc t r' zqin) ~ Cyc2CT m'map zqs (PNoiseTag (p :+ N1) (Cyc t r zp)),
   TunnelCtx ctex t e r s (e * (r' / r)) r' s'   zp hintzq gad,
   TunnelHintCtx       t e r s (e * (r' / r)) r' s' z zp hintzq gad,
   GenSKCtx t r' z Double, GenSKCtx t s' z Double,
   ModSwitchCtx ctex (CT r zp (Cyc t r' zqin)) hintzq,
   ModSwitchCtx ctex (CT s zp (Cyc t s' hintzq))  zq,
   Typeable t, Typeable r', Typeable s', Typeable z)

-- multiple LinearCyc instances, one for each type of gad we might use

instance LinearCyc (PT2CT m'map zqs gad z ctex mon) (PNoiseTag p) where

  -- EAC: Danger: as far as GHC is concerned, ('S h) is not the same as (h :+: N1)
  type PreLinearCyc (PT2CT m'map zqs gad z ctex mon) (PNoiseTag p) =
    PNoiseTag (p :+ N1)

  type LinearCycCtx (PT2CT m'map zqs gad z ctex mon) (PNoiseTag p) t e r s zp =
    (PT2CTLinearCtx ctex mon m'map zqs p t e r s (Lookup r m'map) (Lookup s m'map)
      z zp (PNoise2Zq zqs p) (PNoise2Zq zqs (p :+ N1)) gad)

  linearCyc_ :: forall t zp e r s env expr rp r' s' zq .
    (expr ~ PT2CT m'map zqs gad z ctex mon, s' ~ Lookup s m'map,
     PT2CTLinearCtx ctex mon m'map zqs p t e r s (Lookup r m'map) (Lookup s m'map)
     z zp (PNoise2Zq zqs p) (PNoise2Zq zqs (p :+ N1)) gad,
     Cyc2CT m'map zqs (PNoiseTag p (Cyc t r zp)) ~ CT r zp (Cyc t r' zq), rp ~ Cyc t r zp)
      => Linear t zp e r s -> expr env (PNoiseTag (p :+ N1) rp -> PNoiseTag p (Cyc t s zp))
  linearCyc_ f = PC $ do
    -- the reader stores r, so run the hint generation with s/sqrt(n)
    hint <- local (svar (Proxy::Proxy s')) $
            getTunnelHint @gad @(PNoise2KSZq gad zqs p) (Proxy::Proxy z) f
    return $ lam $
      modSwitch_ $:    -- then scale back to the target modulus zq
      (tunnel_ hint $:     -- linear w/ the hint
        (modSwitch_ $: -- then scale (up) to the hint modulus zq'
          (v0 :: ctex _ (Cyc2CT m'map zqs (PNoiseTag (p :+ N1) rp)))))

----- Type families -----

-- | The number of units a ciphertext with pNoise @p@ must have
type family PNoise2Units (p :: PNoise) where
  PNoise2Units ('PN p) = 'Units (p :+: N2)

-- | (An upper bound on) the pNoise of a ciphertext whose modulus has
-- exactly the given number of units
type family Units2PNoise (h :: Units) where
  Units2PNoise ('Units h) = 'PN (h :-: N2)

-- | The modulus (nested pairs) for a ciphertext with pNoise @p@
type PNoise2Zq zqs (p :: PNoise) = ZqPairsWithUnits zqs (PNoise2Units p)

type family Cyc2CT (m'map :: [(Factored, Factored)]) zqs e = cte | cte -> e where

  Cyc2CT m'map zqs (PNoiseTag p (Cyc t m zp)) =
    CT m zp (Cyc t (Lookup m m'map) (PNoise2Zq zqs p))

  -- for environments
  Cyc2CT m'map zqs (a,b)    = (Cyc2CT m'map zqs a,   Cyc2CT m'map zqs b)
  Cyc2CT m'map zqs ()       = ()

  -- for lists
  Cyc2CT m'map zqs [a]      = [Cyc2CT m'map zqs a]

  -- for functions
  Cyc2CT m'map zqs (a -> b) = (Cyc2CT m'map zqs a -> Cyc2CT m'map zqs b)

  Cyc2CT m'map zqs c = Tagged c
    (TypeError ('Text "Type family 'Cyc2CT' can't convert type '"
                ':<>: 'ShowType c ':<>: 'Text "'."
                ':$$: 'Text "It only converts types of the form 'PNoiseTag p (Cyc t m zp) and pairs/lists/functions thereof."))


-- type-level map lookup
type family Lookup m (map :: [(Factored, Factored)]) where
  Lookup m ( '(m,m') ': rest) = m'
  Lookup r ( '(m,m') ': rest) = Lookup r rest
  Lookup a '[] =
    TypeError ('Text "Could not find " ':<>: 'ShowType a ':$$: 'Text " in a map Lookup.")
