{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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

import Crypto.Alchemy.Interpreter.PT2CT.Environment
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE                  as LSHE
import Crypto.Alchemy.MonadAccumulator

-- | Interprets plaintext operations as their corresponding
-- (homomorphic) ciphertext operations.  The represented plaintext
-- types should have the form 'PNoise h (Cyc t m zp)'.

newtype PT2CT
  m'map    -- | list (map) of (plaintext index m, ciphertext index m')
  zqs      -- | list of pairwise coprime Zq components for ciphertexts
  kszq     -- | additional Zq component for key switches; must be
           -- coprime to all moduli in 'zqs'
  gad      -- | gadget type for key-switch hints
  v        -- | scaled-variance type for secret keys/noise
  ctex     -- | interpreter of ciphertext operations
  mon      -- | monad for creating keys/noise
  e        -- | environment
  a        -- | plaintext type; should be of the form 'PNoise h (Cyc t m zp)'
  = PC {
    -- | Transform a plaintext expression to a ciphertext expression.
    pt2ct :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a))
    }

encrypt :: forall mon t m m' z zp zq v .
  (MonadRandom mon, MonadReader v mon, MonadAccumulator Keys mon,
   EncryptCtx t m m' z zp zq, z ~ LiftOf zp, GenSKCtx t m' z v,
   Typeable t, Typeable m', Typeable z)
  => Cyc t m zp -> mon (CT m zp (Cyc t m' zq))
encrypt x = do
  -- EAC: We need to allow key generation in `encrypt`. Consider the
  -- expression `lam v0` representing `id :: CT -> CT`. We should be
  -- able to evaluate this function on an encrypted input, but there's
  -- no way to generate a key in the definition of `lam` or `v0`.
  (sk :: SK (Cyc t m' z)) <- getKey
  SHE.encrypt sk x

decrypt :: forall mon t m m' z zp zq .
  (MonadReader Keys mon,
   DecryptCtx t m m' z zp zq, z ~ LiftOf zp,
   Typeable t, Typeable m', Typeable z)
  => CT m zp (Cyc t m' zq) -> mon (Maybe (Cyc t m zp))
decrypt x = do
  sk :: SK (Cyc t m' z) <- lookupKey
  return $ flip SHE.decrypt x <$> sk

instance (Lambda ctex, Applicative mon)
  => Lambda (PT2CT m'map zqs kszq gad v ctex mon) where

  lam (PC f) = PC $ fmap lam f

  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

-- CJP: does *every* operation on Cyc need to ensure that a key has
-- been generated for the corresponding CT type?  Currently this is
-- only done "where necessary" to generate hints and to encrypt
-- inputs.  This might already be enough...

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs kszq gad v ctex mon) a where

  add_ = PC $ pure add_
  neg_ = PC $ pure neg_

instance (Lambda ctex, Mul ctex ct, SHE ctex, PreMul ctex ct ~ ct,
          ct ~ Cyc2CT m'map zqs (PNoise h (Cyc t m zp)),
          ct ~ CT m zp (Cyc t m' zq),
          z ~ LiftOf zp, zq' ~ (kszq, zq),
          KSHintCtx gad t m' z zq', GenSKCtx t m' z v,
          RescaleLinearCtx ctex (CT m zp (Cyc t m' zq)) (PNoise2Zq zqs (h :+: N2)),
          KeySwitchQuadCtx ctex (CT m zp (Cyc t m' zq)) (kszq, zq) gad,

          -- EAC: Should be able to write (only) the two constraints below, but can't:
          -- (Typeable (Cyc t m' z), Typeable (KSQuadCircHint gad (Cyc t m' zq')))
          -- See https://ghc.haskell.org/trac/ghc/ticket/13490
          Typeable t, Typeable zq, Typeable kszq, Typeable gad, Typeable z, Typeable m',
          MonadRandom mon, MonadAccumulator Keys mon, MonadAccumulator Hints mon, MonadReader v mon)
  => Mul (PT2CT m'map zqs kszq gad v ctex mon) (PNoise h (Cyc t m zp)) where

  type PreMul (PT2CT m'map zqs kszq gad v ctex mon) (PNoise h (Cyc t m zp)) = PNoise (h :+: N2) (Cyc t m zp)

  mul_ :: forall a b e expr rp .
          (rp ~ Cyc t m zp, a ~ PNoise h rp, b ~ PNoise (h :+: N2) rp,
           expr ~ PT2CT m'map zqs kszq gad v ctex mon) =>
          expr e (b -> b -> a)
  mul_ = PC $ do
    hint :: KSQuadCircHint gad (Cyc t (Lookup m m'map) zq') <-
      getQuadCircHint (Proxy::Proxy (LiftOf zp))
    return $ lam $ lam $
      keySwitchQuad hint $ (LSHE.rescaleLinear v0) *: (LSHE.rescaleLinear v1)

----- Type families -----


type family Cyc2CT m'map zqs e where

  Cyc2CT m'map zqs (PNoise h (Cyc t m zp)) =
    CT m zp (Cyc t (Lookup m m'map) (PNoise2Zq zqs h))

  -- for environments
  Cyc2CT m'map zqs (a,b)    = (Cyc2CT m'map zqs a,   Cyc2CT m'map zqs b)
  Cyc2CT m'map zqs ()       = ()

  -- for functions
  Cyc2CT m'map zqs (a -> b) = (Cyc2CT m'map zqs a -> Cyc2CT m'map zqs b)

  Cyc2CT m'map zqs c =
    TypeError ('Text "Type family 'Cyc2CT' can't convert type '"
               ':<>: 'ShowType c ':<>: 'Text "'."
               ':$$: 'Text "It only converts types of the form 'PNoise h (Cyc t m zp)"
               ':<>: 'Text "and pairs/functions thereof.")

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
