{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Alchemy.Interpreter.PT2CT
( PT2CT, pt2ct
, PNoise
, PT2CTState
, compile
)
where

import Algebra.Additive as Additive (C)
import Algebra.Ring as Ring (C)
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Dynamic
import Data.Maybe
import Data.Type.Natural (Nat (..), type (:+:))
import GHC.TypeLits      hiding (type (*), Nat)

import Crypto.Lol                      hiding (Pos (..))
import Crypto.Lol.Applications.SymmSHE

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

-- | Holds keys and hints generated during the compilation process.
newtype PT2CTState = St ([Dynamic],[Dynamic])

-- explicit forall is for use with TypeApplications at the top level
-- | Compile a plaintext expression to a ciphertext expression.
compile :: forall m'map zqs ksmod gad v ctexpr a rnd mon .
  (MonadRandom rnd, mon ~ ReaderT v (StateT ([Dynamic],[Dynamic]) rnd))
  => v -> PT2CT m'map zqs ksmod gad v ctexpr mon () a -> rnd (ctexpr () (Cyc2CT m'map zqs a), PT2CTState)
compile v (PC a) = do
  (b,st) <- flip runStateT ([],[]) $ flip runReaderT v a
  return (b, St st)

-- | Interprets plaintext operations as their corresponding
-- (homomorphic) ciphertext operations.  The represented plaintext
-- types should have the form 'PNoise h (Cyc t m zp)'.

newtype PT2CT
  m'map           -- | list of (plaintext index m, ciphertext index m')
  zqs             -- | list of ciphertext moduli, in increasing order.
  --   E.g., '[ Zq 7, (Zq 11, Zq 7), (Zq 13, (Zq 11, Zq 7))].  Nesting
  --   order should match efficient 'RescaleCyc' instances.
  (ksmod :: *)         -- | additional modulus to use for key switches. Must be
  -- coprime to all moduli in `zqs`
  (gad :: *)      -- | gadget type for key-switch hints
  v               -- | variance type for secret keys/noise
  ctex            -- | interpreter of ciphertext ops
  mon             -- | monad for creating keys/noise
  e               -- | environment
  a               -- | plaintext type, of form 'PNoise h (Cyc t m zp)'
  = PC { pt2ct :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)) }

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN a deriving (Additive.C, Ring.C)

instance (Lambda ctex, Applicative mon)
  => Lambda (PT2CT m'map zqs ksmod gad v ctex mon) where

  lam (PC f) = PC $ fmap lam f

  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

instance (DB ctex (Cyc2CT m'map zqs a), Applicative mon)
  => DB (PT2CT m'map zqs ksmod gad v ctex mon) a where

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs ksmod gad v ctex mon) a where

  (PC a) +: (PC b) = PC $ (+:) <$> a <*> b

instance (Mul ctexpr ct, SymCT ctexpr, PreMul ctexpr ct ~ ct,
          ct ~ Cyc2CT m'map zqs (PNoise h (Cyc t m zp)), ct ~ CT m zp (Cyc t m' zq),
          z ~ LiftOf zp, zq' ~ (ksmod, zq),
          KSHintCtx gad t m' z zq', GenSKCtx t m' z v,

          -- EAC: Should be able to write (only) the two constraints below, but can't:
          -- (Typeable (Cyc t m' z), Typeable (KSQuadCircHint gad (Cyc t m' zq')))
          -- See https://ghc.haskell.org/trac/ghc/ticket/13490
          Typeable t, Typeable zq, Typeable ksmod, Typeable gad, Typeable z, Typeable m',

          MonadRandom mon, MonadReader v mon, MonadState ([Dynamic],[Dynamic]) mon)
  => Mul (PT2CT m'map zqs ksmod gad v ctexpr mon) (PNoise h (Cyc t m zp)) where

  type PreMul (PT2CT m'map zqs ksmod gad v ctexpr mon) (PNoise h (Cyc t m zp)) = PNoise (h :+: ('S ('S 'Z))) (Cyc t m zp)

  (*:) :: forall a b e expr rp .
       (rp ~ Cyc t m zp, a ~ PNoise h rp, b ~ PNoise (h :+: ('S ('S 'Z))) rp,
        expr ~ PT2CT m'map zqs ksmod gad v ctexpr mon)
       => expr e b -> expr e b -> expr e a
  (PC a) *: (PC b) = PC $ do
    a' <- a
    b' <- b
    hint :: KSQuadCircHint gad (Cyc t (Lookup m m'map) _) <- getKSHint (Proxy::Proxy ksmod) (Proxy::Proxy (LiftOf zp)) (Proxy::Proxy zq)
    return $ keySwitchQuadCT hint $ (rescaleCT a') *: (rescaleCT b')

class SymCT expr where

  rescaleCT :: expr e (CT m zp (Cyc t m' zq)) -> expr e (CT m zp (Cyc t m' zq'))

  keySwitchQuadCT :: (ct ~ CT m zp (Cyc t m' zq))
                  => KSQuadCircHint gad (Cyc t m' zq') -> expr e ct -> expr e ct



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
    TypeError ('Text "Cyc2CT can't convert type " ':$$: 'ShowType c)

-- Maps PNoise to a modulus from a list, according to a heuristic
type family PNoise2Zq zqs h where
  -- pNoise unit ~= 8 bits; so 0--3 fit into a 32-bit modulus.
  PNoise2Zq (zq ': rest) 'Z                     = zq
  PNoise2Zq (zq ': rest) ('S 'Z)                = zq
  PNoise2Zq (zq ': rest) ('S ('S 'Z))           = zq
  PNoise2Zq (zq ': rest) ('S ('S ('S 'Z)))      = zq
  PNoise2Zq (zq ': rest) ('S ('S ('S ('S i))))  = PNoise2Zq rest i


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

---- Monad helper functions

-- retrieve the scaled variance parameter from the Reader
getSvar :: (MonadReader v mon) => mon v
getSvar = ask

-- retrieve a key from the state, or generate a new one otherwise
getKey :: forall z v mon t m' . (MonadReader v mon, MonadState ([Dynamic], [Dynamic]) mon,
           MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z))
  => mon (SK (Cyc t m' z))
getKey = keyLookup >>= \case
  (Just t) -> return t
  -- generate a key with the variance stored in the Reader monad
  Nothing -> genSK =<< getSvar

-- not memoized right now, but could be if we also store the linear function as part of the lookup key
-- EAC: https://ghc.haskell.org/trac/ghc/ticket/13490
genTunnHint :: forall gad zq mon t e r s e' r' s' z zp v .
  (MonadReader v mon, MonadState ([Dynamic], [Dynamic]) mon, MonadRandom mon,
   GenSKCtx t r' z v, Typeable (Cyc t r' (LiftOf zp)),
   GenSKCtx t s' z v, Typeable (Cyc t s' (LiftOf zp)),
   GenTunnelInfoCtx t e r s e' r' s' z zp zq gad,
   z ~ LiftOf zp)
  => Linear t zp e r s -> mon (TunnelInfo gad t e r s e' r' s' zp zq)
genTunnHint linf = do
  skout <- getKey @z
  sk <- getKey @z
  tunnelInfo linf skout sk

-- retrieve a key-switch hint from the state, or generate a new one otherwise
getKSHint :: forall v mon t z gad m' (zq :: *) zq' ksmod .
  (-- constraints for getKey
   MonadReader v mon, MonadState ([Dynamic], [Dynamic]) mon,
   MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z),
   -- constraints for hintLookup
   Typeable (KSQuadCircHint gad (Cyc t m' zq')),
   -- constraints for ksQuadCircHint
   KSHintCtx gad t m' z zq', zq' ~ (ksmod, zq)) -- EAC: Note that order matches the optimized RescaleCyc instance
  => Proxy ksmod -> Proxy z -> Proxy zq -> mon (KSQuadCircHint gad (Cyc t m' zq'))
getKSHint _ _ _ = hintLookup >>= \case
  (Just h) -> return h
  Nothing -> do
    sk :: SK (Cyc t m' z) <- getKey
    ksQuadCircHint sk

-- lookup a key in the state
keyLookup :: (Typeable a, MonadState ([Dynamic], b) mon) => mon (Maybe a)
keyLookup = (dynLookup . fst) <$> get

-- lookup a hint in the state
hintLookup :: (Typeable a, MonadState (b, [Dynamic]) mon) => mon (Maybe a)
hintLookup = (dynLookup . snd) <$> get

-- lookup an item in a dynamic list
dynLookup :: (Typeable a) => [Dynamic] -> Maybe a
dynLookup ds = case mapMaybe fromDynamic ds of
  [] -> Nothing
  [x] -> Just x
