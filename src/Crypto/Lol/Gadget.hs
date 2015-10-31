{-# LANGUAGE ConstraintKinds, DataKinds, DeriveDataTypeable,
             FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, ScopedTypeVariables,
             TupleSections, TypeFamilies, UndecidableInstances #-}

-- | Interfaces for "gadgets," decomposition, and error correction.

module Crypto.Lol.Gadget
( Gadget(..), Decompose(..), Correct(..)
, TrivGad, BaseBGad
) where

import Crypto.Lol.LatticePrelude

import Control.Applicative
import Data.Typeable

-- | Dummy type representing the gadget @[1]@.
data TrivGad deriving (Typeable)
-- | Dummy type representing the gadget @[1,b,b^2,...]@.
data BaseBGad b deriving (Typeable)

-- | "Gadget" vectors, parameterized by an index type.

class Ring u => Gadget gad u where
  -- | The gadget vector over @u@.
  gadget :: Tagged gad [u]

  -- | Yield an error-tolerant encoding of an element with respect to
  -- the gadget.  (Mathematically, this should just be the product of
  -- the input with the gadget, but it is a class method to allow for
  -- optimized implementations.)
  encode :: u -> Tagged gad [u]
  encode s = ((* s) <$>) <$> gadget

-- | Decomposition relative to a gadget.

class (Gadget gad u, Reduce (DecompOf u) u) => Decompose gad u where
  -- | The ring that @u@ decomposes over.
  type DecompOf u

  -- | Yield a short vector @x@ such that @\<g, x\> = u@.
  decompose :: u -> Tagged gad [DecompOf u]

-- | Error correction relative to a gadget.

class Gadget gad u => Correct gad u where

  -- | Correct a "noisy" encoding of an element (see 'encode').
  correct :: Tagged gad [u] -> u



-- instances for products

instance (Gadget gad a, Gadget gad b) => Gadget gad (a,b) where

  gadget = (++) <$> (map (,zero) <$> gadget) <*> (map (zero,) <$> gadget)

instance (Decompose gad a, Decompose gad b, DecompOf a ~ DecompOf b)
         => Decompose gad (a,b) where

  type DecompOf (a,b) = DecompOf a

  decompose (a,b) = (++) <$> decompose a <*> decompose b


-- TODO: need some extra constraints on a,b, like Mod and maybe Rescale.
-- instance (Correct gad a, Correct gad b) => Correct gad (a,b) where



{- CJP: strawman class for the more general view of LWE secrets as
"module characters," i.e., module homomorphisms into a particular
range.  This is probably wrong, though.

class Character u where       -- Module superclass(es)?
  type CharRange u
  data Char u                   -- need data for injectivity

  evalChar :: Char u -> u -> CharRange u

class (Gadget gad u, Character u) => Correct gad u where

  -- | Correct a "noisy" encoding of an LWE secret (i.e., a
  -- 'ModuleHomom' on 'u').
  correct :: Tagged gad [CharRange u] -> Char u

encode :: (Correct gad u) => Char u -> Tagged gad [CharRange u]
encode s = pasteT $ evalMH s <$> peelT gadget

-}

