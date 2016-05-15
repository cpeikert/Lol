{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, FlexibleContexts, GADTs,
             InstanceSigs, KindSignatures, PartialTypeSignatures, PolyKinds,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Crypto.Lol.FullTree
( augmentBitString
, flipBit
, FullTree(..)
, rootValue
) where

import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude
import Crypto.Lol.MMatrix
import Crypto.Lol.PosBinDefs
import Crypto.Lol.Reflects
import Crypto.Lol.SafeBitString

import Crypto.Lol.Types.Numeric as N

import Data.Functor.Trans.Tagged

import qualified MathObj.Matrix as M

-- n = numLeaves, l = leafType, v = vectorRq
data FullTree (n :: Pos) l v where
  Leaf :: l -> v -> FullTree O l v
  Internal :: v -> FullTree nl l v
                -> FullTree nr l v
                -> FullTree (AddPos nl nr) l v

-- | Returns the vector attached to the FullTree.
rootValue :: FullTree n l v -> v
rootValue (Leaf b v) = v
rootValue (Internal v l r) = v

-- | Augments the leaves of the FullTree with Bool values.
augmentBitString :: FullTree n () () -> -- ^ Full tree T (topology)
                    SafeBitString n -> -- Bit string x
                    FullTree n Bool () -- ^ Full tree T (bit on each leaf)
augmentBitString (Leaf _ _) (Bit b) = Leaf b ()
--augmentBitString (Internal _ left right) bits =
  --let (leftBits, rightBits) = splitSBS bits
  --in Internal () (augmentBitString left leftBits) (augmentBitString right rightBits)

-- | Augments the nodes of the FullTree with MMatrix values.
{-augmentVector :: (Ring a, Reduce [DecompOf a] [a], Decompose (BaseBGad 2) a) =>
                 MMatrix a -> -- ^ Base vector a0
                 MMatrix a -> -- ^ Base vector a1
                 FullTree n Bool () -> -- ^ Full tree T (bit on each leaf)
                 FullTree n Bool (MMatrix a) -- ^ Full tree T (calculated a_T(x))
augmentVector a0 a1 (Leaf b _)
  | b = Leaf b a1
  | otherwise = Leaf b a0
augmentVector a0 a1 (Internal _ l r) =
  let l' = augmentVector a0 a1 l
      r' = augmentVector a0 a1 r
      c = combineVectors (rootValue l') (rootValue r')
  in (Internal c l' r')-}

-- | Flip the boolean value at a chosen leaf.
flipBit :: (Ring a) =>
           MMatrix a ->
           MMatrix a ->
           Pos ->
           FullTree n Bool (MMatrix a) ->
           FullTree n Bool (MMatrix a)
flipBit a0 a1 O (Leaf b v)
  | b = Leaf (not b) a0
  | otherwise = Leaf (not b) a1
-- pseudocode available for the other case in 5/13/16 log.
