{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds  #-}

-- should be a hidden/internal module
module Crypto.Alchemy.Interpreter.PT2CT.Noise
( PNoise(..), ZqPairsWithUnits, TotalUnits, MaxUnits
, TLNatNat, mkTLNatNat) where

import           Algebra.Additive          as Additive (C)
import           Algebra.Ring              as Ring (C)
import           Data.Functor.Trans.Tagged
import           Data.Singletons.Prelude   hiding ((:<))
import           Data.Singletons.Prelude.List (Sum, Maximum)
import           Data.Singletons.TH        hiding ((:<))
import           Data.Type.Natural
import qualified GHC.TypeLits              as TL (Nat)
import           GHC.TypeLits              hiding (Nat)
import           Language.Haskell.TH

import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN {unPN :: a}
  -- EAC: Okay to derive Functor and Applicative? It makes life easier because
  -- we can define a single instance (e.g., of E) rather than one for Identity
  -- and one for (PNoise h)
  deriving (Additive.C, Ring.C, Functor, Show)

instance Applicative (PNoise h) where
  pure = PN
  (PN f) <*> (PN a) = PN $ f a

-- CJP: why should this be defined here?
type family Modulus zq :: k
type instance Modulus (ZqBasic q i) = q

-- | Convert a list to nested pairs, in "reverse inside out" order:
-- last element of list is first and outermost.
type List2Pairs a = L2P (Reverse a)

-- auxiliary family
type family L2P a where
  L2P '[a] = a
  L2P (a ': b) = (a, L2P b)

-- | Maps  'Modulus' over a type list.
type family MapModulus zqs where
  MapModulus '[] = '[]
  MapModulus (x ': xs) = (Modulus x) ': (MapModulus xs)

-- | A type-lit 'TL.Nat' with a type-natural 'Nat'.
data TLNatNat = NN TL.Nat Nat

type family NatOf a where
  NatOf ('NN a b) = b

-- | Maps 'NatOf' over a type list.
type family MapNatOf a where
  MapNatOf '[] = '[]
  MapNatOf (a ': rest) = (NatOf a) ': MapNatOf rest

singletons [d|
             -- | (Singletons version takes a TypeLit, rather than a 'Nat'.)
             take :: Nat -> [a] -> [a]
             take Z _ = []
             take (S n) (x : xs) = x : take n xs

             -- | Given a list and a threshold h, output the length of
             -- the shortest nonempty prefix whose sum is >= h.
             prefixLen :: [Nat] -> Nat -> Nat
             prefixLen (a : rest) h = if a >= h then S Z
                                      else S (prefixLen rest (h - a))
             prefixLen [] _ =
               -- this should be caught by the PNoise2ZqError type family
               error "prefixLen: threshold is larger than sum of list entries"
           |]

type family NatToLit x where
  NatToLit 'Z = 0
  NatToLit ('S n) = 1 + (NatToLit n)

-- | The number of noise units of the largest modulus among the first
-- of those that in total have at least @h@ units.
type MaxUnits zqs h = Maximum (MapNatOf (MapModulus (ZqsWithUnits zqs h)))

-- | For a list of moduli @zqs@, nested pairs representing moduli that
-- have a total of at least @h@ units.
type ZqPairsWithUnits zqs h = List2Pairs (ZqsWithUnits zqs h)

-- | For a list of moduli @zqs@, a list representing moduli that have
-- a total of at least @h@ units.
type ZqsWithUnits zqs h =
  ZqsWithUnits' (h :<= (Sum (MapNatOf (MapModulus zqs)))) h zqs

-- | The total noise units among the first of the moduli having at
-- least @h@ units.
type TotalUnits zqs h = Sum (MapNatOf (MapModulus (ZqsWithUnits zqs h)))

type family ZqsWithUnits' b h zqs where
  ZqsWithUnits' 'True h zqs = Take (PrefixLen (MapNatOf (MapModulus zqs)) h) zqs
  -- error case
  ZqsWithUnits' 'False h zqs =
    TypeError ('Text "ZqsWithUnits: Modulus needs to support at least " ':<>:
               'ShowType (NatToLit h) ':<>:
               'Text " noise units, but it only supports " ':<>:
               'ShowType (NatToLit (Sum (MapNatOf (MapModulus zqs)))) ':<>:
               'Text " units." ':$$:
               'Text "You need more/bigger moduli!")

-- | "Bits" per noise unit.
pNoiseUnit :: Double
pNoiseUnit = 6.3

mkTypeNat :: Int -> TypeQ
mkTypeNat x | x < 0 = error $ "mkTypeNat: negative argument " ++ show x
mkTypeNat 0 = conT 'Z
mkTypeNat x = conT 'S `appT` (mkTypeNat $ x-1)

mkTypeLit :: Integer -> TypeQ
mkTypeLit = litT . numTyLit

-- | TH splice for a 'TLNatNat' of the given integer with the number
-- of noise units it can hold.
mkTLNatNat :: Integer -> TypeQ
mkTLNatNat q =
  let units = floor $ logBase 2 (fromInteger q) / pNoiseUnit
  in conT 'NN `appT` (mkTypeLit q) `appT` (mkTypeNat units)

instance (Reflects x i) => Reflects ('NN x y) i where
  value = retag $ value @x
