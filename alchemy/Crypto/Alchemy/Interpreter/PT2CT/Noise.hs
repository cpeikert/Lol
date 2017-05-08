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

-- should be a hidden/internal module
module Crypto.Alchemy.Interpreter.PT2CT.Noise where

import           Algebra.Additive          as Additive (C)
import           Algebra.Ring              as Ring (C)
import           Data.Functor.Trans.Tagged
import           Data.Singletons.Prelude   hiding ((:<))
import           Data.Singletons.Prelude.List (Sum)
import           Data.Singletons.TH        hiding ((:<))
import           Data.Type.Natural
import qualified GHC.TypeLits              as TL (Nat)
import           GHC.TypeLits              hiding (Nat)
import           Language.Haskell.TH

import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.ZqBasic

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN a
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
type family List2Pairs a where List2Pairs a = L2P (Reverse a)

-- auxiliary family
type family L2P a where
  L2P '[a] = a
  L2P (a ': b) = (a, List2Pairs b)

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

-- | Given a list of moduli @zqs@, construct nested pairs representing
-- a product of moduli large enough to contain the given pNoise @h@.
type PNoise2Zq zqs h =
  PNoise2ZqError (h :<= (Sum (MapNatOf (MapModulus zqs)))) (Sum (MapNatOf (MapModulus zqs))) h zqs

type family NatToLit x where
  NatToLit 'Z = 0
  NatToLit ('S n) = 1 + (NatToLit n)

type family PNoise2ZqError b sum h zqs where
  -- h <= sum: we're good to go
  PNoise2ZqError 'True sum h zqs = List2Pairs (Take (PrefixLen (MapNatOf (MapModulus zqs)) h) zqs)
  -- error if sum < h
  PNoise2ZqError 'False sum h zqs =
    TypeError ('Text "PNoise2Zq: Modulus needs to support at least " ':<>: 'ShowType (NatToLit h) ':<>:
      'Text " noise units, but it only supports " ':<>: 'ShowType (NatToLit sum) ':<>: 'Text " units." ':$$:
      'Text "You need a bigger modulus!")

-- | "Bits" per noise unit.
pNoiseUnit :: Double
pNoiseUnit = 7.5

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
