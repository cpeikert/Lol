{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.Interpreter.PT2CT
( PT2CT, pt2ct
, PNoise
)
where

import Data.Type.Natural (Nat (..))
import GHC.TypeLits      hiding (type (*), Nat)

import Crypto.Lol                      hiding (Pos (..))
import Crypto.Lol.Applications.SymmSHE

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

-- | Interprets plaintext operations as their corresponding
-- (homomorphic) ciphertext operations.  The represented plaintext
-- types should have the form 'PNoise h (Cyc t m zp)'.

newtype PT2CT
  m'map           -- | list of (plaintext index m, ciphertext index m')
  zqs             -- | list of ciphertext moduli, in increasing order.
  --   E.g., '[ Zq 7, (Zq 11, Zq 7), (Zq 13, (Zq 11, Zq 7))].  Nesting
  --   order should match efficient 'RescaleCyc' instances.
  zq'map          -- | map from ciphertext modulus to corresponding
  -- key-switch hint modulus.  E.g., '[ '(Zq 7, (Zq 11, Zq 7))].
  -- Nesting order should match efficient 'RescaleCyc' instances.
  gad             -- | gadget type for key-switch hints
  v               -- | variance type for secret keys/noise
  ctex            -- | interpreter of ciphertext ops
  mon             -- | monad for creating keys/noise
  e               -- | environment
  a               -- | plaintext type, of form 'PNoise h (Cyc t m zp)'
  = PC { pt2ct :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a)) }

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise h a = PN a

instance (Lambda ctex, Applicative mon)
  => Lambda (PT2CT m'map zqs zq'map gad v ctex mon) where

  lam (PC f) = PC $ fmap lam f

  (PC f) $: (PC a) = PC $ ($:) <$> f <*> a

instance (DB ctex (Cyc2CT m'map zqs a), Applicative mon)
  => DB (PT2CT m'map zqs zq'map gad v ctex mon) a where

  v0       = PC $ pure v0
  s (PC a) = PC $ s <$> a

instance (Add ctex (Cyc2CT m'map zqs a), Applicative mon)
  => Add (PT2CT m'map zqs zq'map gad v ctex mon) a where

  (PC a) +: (PC b) = PC $ (+:) <$> a <*> b

-- CJP: TODO: Mul instance.  The instance head and constraints will be
-- significantly more complex, and will include SHE class and constraints






----- Type families -----

type family Cyc2CT m'map zqs e where
  Cyc2CT m'map zqs (PNoise h (Cyc t m zp)) =
    CT m zp (Cyc t (Lookup m m'map) (PNoise2Zq zqs h))

  -- for environments
  Cyc2CT m'map zqs (a,b)    = (Cyc2CT m'map zqs a,   Cyc2CT m'map zqs b)

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


{- Old GADT-style definition of PT2CT; apparently unneeded.
newtype PT2CT ::
  [(Factored,Factored)] -- | map of (plaintext index m, ciphertext index m')
  -> [*]
        -- | list of ciphertext moduli, in increasing order.
        --   E.g., '[ Zq 7, (Zq 11, Zq 7), (Zq 13, (Zq 11, Zq 7))].
        --   Nesting order should match efficient 'RescaleCyc' instances.
  -> [(*,*)]
        -- | map from ciphertext modulus to corresponding key-switch
        -- hint modulus.  E.g., '[ '(Zq 7, (Zq 11, Zq 7))].  Nesting
        -- order should match efficient 'RescaleCyc' instances.
  -> *                          -- | gadget type for key-switch hints
  -> *                          -- | variance type
  -> (* -> * -> *)              -- | ciphertext interpreter
  -> (* -> *)                   -- | monad
  -> *                          -- | environment
  -> *                          -- | type the expression represents
  -> *
  where
  PC :: mon (ctex (Cyc2CT m'map zqs e) (Cyc2CT m'map zqs a))
     -> PT2CT m'map zqs zq'map gad v ctex mon e a
-}

