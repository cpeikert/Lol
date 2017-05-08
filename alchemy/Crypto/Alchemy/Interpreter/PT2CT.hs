{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Alchemy.Interpreter.PT2CT
( PT2CT, PNoise
, pt2ct, encrypt, decrypt
) where

import Control.Monad.Random
import Control.Monad.Reader
import Data.Dynamic
import Data.Type.Natural    ((:+:), N2, Nat (..))
import GHC.TypeLits         hiding (type (*), Nat)

import           Crypto.Lol                      hiding (Pos (..))
import           Crypto.Lol.Applications.SymmSHE hiding (decrypt, encrypt)
import qualified Crypto.Lol.Applications.SymmSHE as SHE

import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.SHE as LSHE
import Crypto.Alchemy.Language.Tunnel as T
import Crypto.Alchemy.MonadAccumulator

-- | Interprets plaintext operations as their corresponding
-- (homomorphic) ciphertext operations.  The represented plaintext
-- types should have the form 'PNoise h (Cyc t m zp)'.
newtype PT2CT
  m'map    -- | list (map) of (plaintext index m, ciphertext index m')
  zqs      -- | list of pairwise coprime Zq components for ciphertexts
  (kszq :: *)     -- | additional Zq component for key switches; must be
           -- coprime to all moduli in 'zqs'
  (gad :: *)      -- | gadget type for key-switch hints
  (z :: *)        -- | integral type for secret keys
  v        -- | scaled-variance type for secret keys/noise
  ctex     -- | interpreter of ciphertext operations
  mon      -- | monad for creating keys/noise
  e        -- | environment
  a        -- | plaintext type; should be of the form 'PNoise h (Cyc t m zp)'
  = PC { unPC :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)) }

-- | Transform a plaintext expression to a ciphertext expression.
pt2ct :: forall m'map zqs kszq gad z v ctex a mon .
      -- this forall is for use with TypeApplications at the top level
  v   -- | scaled variance to used for generating keys/hints
  -> PT2CT m'map zqs kszq gad z v ctex (ReaderT v mon) () a -- | plaintext expression
  -> mon (ctex () (Cyc2CT m'map zqs a)) -- | (monadic) ctex expression
pt2ct v = flip runReaderT v . unPC

-- | Encrypt a plaintext (using the given scaled variance) under an
-- appropriate key (from the monad), generating one if necessary.
encrypt :: forall mon t m m' zp zq z v .
  (MonadRandom mon, MonadAccumulator Keys mon,
   -- CJP: DON'T LOVE THIS CHOICE OF z HERE; IT'S ARBITRARY
   EncryptCtx t m m' z zp zq, z ~ LiftOf zp, GenSKCtx t m' z v,
   Typeable t, Typeable m', Typeable z)
  => v                           -- | scaled variance for keys and error
  -> Cyc t m zp                  -- | plaintext
  -> mon (CT m zp (Cyc t m' zq)) -- | (monadic) ciphertext
encrypt v x = flip runReaderT v $ do
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
  => Lambda (PT2CT m'map zqs kszq gad z v ctex mon) where

  lam (PC f) = PC $ lam <$> f
  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

instance (List ctex, Applicative mon)
  => List (PT2CT m'map zqs kszq gad z v ctex mon) where
  nil_  = PC $ pure nil_
  cons_ = PC $ pure cons_

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs kszq gad z v ctex mon) a where

  add_ = PC $ pure add_
  neg_ = PC $ pure neg_

instance (m' ~ Lookup m m'map,
          zqin ~ PNoise2Zq zqs (h :+: N2), zq' ~ (kszq, zq), zq ~ PNoise2Zq zqs h,
          ctin ~ CT m zp (Cyc t m' zqin), ct' ~ CT m zp (Cyc t m' zq'), ct ~ CT m zp (Cyc t m' zq),
          Lambda ctex, Mul ctex ct, PreMul ctex ct ~ ct, SHE ctex,
          RescaleLinearCtx ctex ctin zq,  -- input -> zq (final modulus)
          RescaleLinearCtx ctex ct   zq', -- zq    -> scaled-up zq' (hint)
          RescaleLinearCtx ctex ct'  zq, -- zq'   -> zq, finally
          KeySwitchQuadCtx ctex ct' gad,  -- hint over zq'
          KSHintCtx gad t m' z zq', GenSKCtx t m' z v,
          Typeable (Cyc t m' z), Typeable (KSQuadCircHint gad (Cyc t m' zq')),
          MonadRandom mon, MonadReader v mon,
          MonadAccumulator Keys mon, MonadAccumulator Hints mon)

  -- CJP: recall that the types have to be fully spelled out here and
  -- in the associated type; we can't use the shorthand rp, ct, etc.
  => Mul (PT2CT m'map zqs kszq gad z v ctex mon) (PNoise h (Cyc t m zp)) where

  -- ditto
  type PreMul (PT2CT m'map zqs kszq gad z v ctex mon) (PNoise h (Cyc t m zp)) =
    PNoise (h :+: N2) (Cyc t m zp)

  mul_ = PC $ do
    hint :: KSQuadCircHint gad (Cyc t m' zq') <- getQuadCircHint (Proxy::Proxy z)
    return $ lam $ lam $
      rescaleLinear_ $:
      (keySwitchQuad_ hint $:
        (rescaleLinear_ $:
         -- CJP: GHC should infer the types of v0,v1 -- but here we are
         (((rescaleLinear_ $: (v0 :: ctex _ ctin)) *:
            (rescaleLinear_ $: (v1 :: ctex _ ctin))) :: ctex _ ct)))

type PT2CTTunnCtx ctex m'map zqs h t e r s r' s' z zp zq zqin kszq v gad =
  (PT2CTTunnCtx' ctex m'map zqs h t e r s r' s' z zp zq zqin (kszq,zq) v gad)

type PT2CTTunnCtx' ctex m'map zqs h t e r s r' s' z zp zq zqin zq' v gad =
  (-- output ciphertext type
   CT s zp (Cyc t s' zq)   ~ Cyc2CT m'map zqs (PNoise h (Cyc t s zp)),
   -- input ciphertext type: plaintext has one-larger pnoise
   CT r zp (Cyc t r' zqin) ~ Cyc2CT m'map zqs (PNoise ('S h) (Cyc t r zp)),
   LSHE.TunnelCtx ctex t e r s (e * (r' / r)) r' s'   zp zq' gad,
   TunnelHintCtx       t e r s (e * (r' / r)) r' s' z zp zq' gad,
   GenSKCtx t r' z v, GenSKCtx t s' z v,
   RescaleLinearCtx ctex (CT r zp (Cyc t r' zqin)) zq,
   RescaleLinearCtx ctex (CT r zp (Cyc t r' zq))   zq',
   RescaleLinearCtx ctex (CT s zp (Cyc t s' zq'))  zq,
   Typeable t, Typeable r', Typeable s', Typeable z)

type family ZqOfCT ct where ZqOfCT (CT r zp (Cyc t r' zq)) = zq

instance (SHE ctex, Lambda ctex, MonadRandom mon, MonadReader v mon, MonadAccumulator Keys mon)
  => Tunnel (PT2CT m'map zqs kszq gad z v ctex mon) (PNoise h) where

  type PreTunnel (PT2CT m'map zqs kszq gad z v ctex mon) (PNoise h) = PNoise ('S h)

  type TunnelCtx (PT2CT m'map zqs kszq gad z v ctex mon) (PNoise h) t e r s zp =
    (PT2CTTunnCtx
      ctex m'map zqs h t e r s
      (Lookup r m'map)
      (Lookup s m'map)
      z zp
      (ZqOfCT (Cyc2CT m'map zqs (PNoise h (Cyc t s zp))))
      (ZqOfCT (Cyc2CT m'map zqs (PNoise ('S h) (Cyc t r zp))))
      kszq v gad)

  tunnel :: forall t zp e r s env expr zq' rp r' zq .
    (expr ~ PT2CT m'map zqs kszq gad z v ctex mon, T.TunnelCtx expr (PNoise h) t e r s zp,
     Cyc2CT m'map zqs (PNoise h (Cyc t r zp)) ~ CT r zp (Cyc t r' zq),
     zq' ~ (kszq, zq), rp ~ Cyc t r zp)
    => Linear t zp e r s -> expr env (PNoise ('S h) rp -> PNoise h (Cyc t s zp))
  tunnel f = PC $ do
    hint <- getTunnelHint @gad @zq' (Proxy::Proxy z) f
    return $ lam $
      rescaleLinear_ $: -- then scale back to the target modulus zq
      (tunnel_ hint $:   -- tunnel w/ the hint
        (rescaleLinear_ $: -- then scale (up) to the hint modulus zq'
        -- first (possibly) rescale down to the target modulus zq
         (rescaleLinear_ $: (v0 :: ctex _ (Cyc2CT m'map zqs (PNoise ('S h) rp)))
          :: ctex _ (CT r zp (Cyc t r' zq)))))

----- Type families -----


type family Cyc2CT m'map zqs e where

  Cyc2CT m'map zqs (PNoise h (Cyc t m zp)) =
    CT m zp (Cyc t (Lookup m m'map) (PNoise2Zq zqs h))

  -- for environments
  Cyc2CT m'map zqs (a,b)    = (Cyc2CT m'map zqs a,   Cyc2CT m'map zqs b)
  Cyc2CT m'map zqs ()       = ()

  -- for lists
  Cyc2CT m'map zqs [a]      = [Cyc2CT m'map zqs a]

  -- for functions
  Cyc2CT m'map zqs (a -> b) = (Cyc2CT m'map zqs a -> Cyc2CT m'map zqs b)

  Cyc2CT m'map zqs c =
    TypeError ('Text "Type family 'Cyc2CT' can't convert type '"
               ':<>: 'ShowType c ':<>: 'Text "'."
               ':$$: 'Text "It only converts types of the form 'PNoise h (Cyc t m zp) and pairs/lists/functions thereof.")

-- type-level map lookup
type family Lookup m map where
  Lookup m ( '(m,m') ': rest) = m'
  Lookup r ( '(m,m') ': rest) = Lookup r rest
  Lookup a '[] =
    TypeError ('Text "Could not find " ':<>: 'ShowType a ':$$: 'Text " in a map Lookup.")

-- Type-level index.  singletons exports (:!!), which takes a TypeLit
-- index, but we use TypeNatural.
type family (xs :: [k1]) !! (d :: Nat) :: k1 where
  (x ': xs) !! 'Z     = x
  (x ': xs) !! ('S i) = xs !! i
  '[]       !! i =
    TypeError ('Text "Out-of-bounds error for type-level indexing (!!).")
