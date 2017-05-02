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
( PT2CT
, PNoise
, P2CState
, pt2ct, encryptP2C, decryptP2C
) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.Type.Natural (Nat (..), type (:+:), N2)
import GHC.TypeLits      hiding (type (*), Nat)

import Crypto.Lol                      hiding (Pos (..))
import Crypto.Lol.Applications.SymmSHE

import Crypto.Alchemy.Interpreter.PT2CT.Environment
import Crypto.Alchemy.Interpreter.PT2CT.Noise
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.SHE

encryptP2C :: forall t m m' z zp zq rnd .
  (EncryptCtx t m m' z zp zq, z ~ LiftOf zp, Typeable t, Typeable m', Typeable z, MonadRandom rnd)
  => P2CState -> Cyc t m zp -> Maybe (rnd (CT m zp (Cyc t m' zq)))
encryptP2C st x = flip evalState st $ do
  (sk :: Maybe (SK (Cyc t m' z))) <- lookupKey -- ONLY lookup the key, do NOT generate!
                  -- my feeling is that this should never fail, but we don't have static proof of that
  return $ (flip encrypt x) <$> sk

decryptP2C :: forall t m m' z zp zq . (DecryptCtx t m m' z zp zq, z ~ LiftOf zp, Typeable t, Typeable m', Typeable z)
  => P2CState -> CT m zp (Cyc t m' zq) -> Maybe (Cyc t m zp)
decryptP2C st x = flip evalState st $ do
  (sk :: Maybe (SK (Cyc t m' z))) <- lookupKey -- ONLY lookup the key, do NOT generate
  return $ flip decrypt x <$> sk

-- explicit forall is for use with TypeApplications at the top level

-- | Transform a plaintext expression to a ciphertext expression.
pt2ct :: forall m'map zqs kszq gad v ctex a rnd mon .
  (MonadRandom rnd, mon ~ ReaderT v (StateT P2CState rnd))
  => v                          -- | scaled variance for generated keys, hints
  -> PT2CT m'map zqs kszq gad v ctex mon () a
  -> rnd (ctex () (Cyc2CT m'map zqs a), P2CState)
pt2ct v (PC a) = flip runStateT newP2CState $ flip runReaderT v a

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
  a        -- | plaintext type, of form 'PNoise h (Cyc t m zp)'
  = PC (mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)))

instance (Lambda ctex, Applicative mon)
  => Lambda (PT2CT m'map zqs kszq gad v ctex mon) where

  lam (PC f) = PC $ fmap lam f

  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs kszq gad v ctex mon) a where

  (PC a) +: (PC b) = PC $ (+:) <$> a <*> b
  neg (PC a) = PC $ neg <$> a

instance (Mul ctex ct, SHE ctex, PreMul ctex ct ~ ct,
          ct ~ Cyc2CT m'map zqs (PNoise h (Cyc t m zp)), ct ~ CT m zp (Cyc t m' zq),
          z ~ LiftOf zp, zq' ~ (kszq, zq),
          KSHintCtx gad t m' z zq', GenSKCtx t m' z v,
          RescaleLinearCtx ctex (CT m zp (Cyc t m' zq)) (PNoise2Zq zqs (h :+: N2)),
          KeySwitchQuadCtx ctex (CT m zp (Cyc t m' zq)) (kszq, zq) gad,

          -- EAC: Should be able to write (only) the two constraints below, but can't:
          -- (Typeable (Cyc t m' z), Typeable (KSQuadCircHint gad (Cyc t m' zq')))
          -- See https://ghc.haskell.org/trac/ghc/ticket/13490
          Typeable t, Typeable zq, Typeable kszq, Typeable gad, Typeable z, Typeable m',
          MonadRandom mon, MonadReader v mon, MonadState P2CState mon)
  => Mul (PT2CT m'map zqs kszq gad v ctex mon) (PNoise h (Cyc t m zp)) where

  type PreMul (PT2CT m'map zqs kszq gad v ctex mon) (PNoise h (Cyc t m zp)) = PNoise (h :+: N2) (Cyc t m zp)

  (*:) :: forall a b e expr rp .
       (rp ~ Cyc t m zp, a ~ PNoise h rp, b ~ PNoise (h :+: N2) rp,
        expr ~ PT2CT m'map zqs kszq gad v ctex mon)
       => expr e b -> expr e b -> expr e a
  (PC a) *: (PC b) = PC $ do
    a' <- a
    b' <- b
    hint :: KSQuadCircHint gad (Cyc t (Lookup m m'map) _) <- getKSHint (Proxy::Proxy kszq) (Proxy::Proxy (LiftOf zp)) (Proxy::Proxy zq)
    return $ keySwitchQuad hint $ (rescaleLinear a') *: (rescaleLinear b')

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
