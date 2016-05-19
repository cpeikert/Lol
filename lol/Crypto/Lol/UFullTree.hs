{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, FlexibleContexts, GADTs,
             InstanceSigs, KindSignatures, PartialTypeSignatures, PolyKinds,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Crypto.Lol.UFullTree
( augmentSBS
, augmentVector
, flipBit
, UFullTree(..)
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

import GHC.TypeLits as T

import qualified MathObj.Matrix as M

-- n = numLeaves, l = leafType, v = vectorRq
data UFullTree nl nr l v where
  ULeaf :: l -> v -> UFullTree Int Int l v -- holds
  UInternal :: Int -> Int -> v
                     -> UFullTree Int Int l v -- not type-safe
                     -> UFullTree Int Int l v -- not type-safe
                     -> UFullTree Int Int l v -- assumes you've given the correct # of leaves.

-- | Returns the vector attached to the FullTree.
rootValue :: UFullTree nl nr l v -> v
rootValue (ULeaf _ v) = v
rootValue (UInternal _ _ v _ _) = v

-- | Augments the leaves of the FullTree with Bool values.
augmentSBS :: UFullTree Int Int () () -> -- ^ Full tree T (topology)
              [Bool] -> -- Bit string x (size: nl + nr)
              UFullTree Int Int Bool () -- ^ Full tree T (bit on each leaf)
augmentSBS (ULeaf _ _) bits = ULeaf (head bits) ()
augmentSBS (UInternal ls rs _ left right) bits =
  let (leftBits, rightBits) = splitAt ls bits
  in UInternal ls rs () (augmentSBS left leftBits) (augmentSBS right rightBits)

-- | Augments the nodes of the FullTree with MMatrix values.
augmentVector :: (Ring (DecompOf a), Lift a (DecompOf a), Reduce (DecompOf a) a,
                 Decompose (BaseBGad 2) a, LiftOf a ~ DecompOf a) =>
                 MMatrix a -> -- ^ Base vector a0
                 MMatrix a -> -- ^ Base vector a1
                 UFullTree Int Int Bool () -> -- ^ Full tree T (bit on each leaf)
                 UFullTree Int Int Bool (MMatrix a) -- ^ Full tree T (calculated a_T(x))
augmentVector a0 a1 (ULeaf b _)
  | b = ULeaf b a1
  | otherwise = ULeaf b a0
augmentVector a0 a1 (UInternal nl nr _ l r) =
  let l' = augmentVector a0 a1 l
      r' = augmentVector a0 a1 r
      c = combineVectors (rootValue l') (rootValue r')
  in (UInternal nl nr c l' r')

-- | Flip the boolean value at a chosen leaf.
flipBit :: (Ring a) =>
           MMatrix a -> -- ^ Base vector a0
           MMatrix a -> -- ^ Base vector a1
           Pos -> -- ^ # of bit to flip
           UFullTree Int Int Bool (MMatrix a) -> -- ^ Full Tree T
           UFullTree Int Int Bool (MMatrix a) -- ^ Full Tree T (after bit flip)
flipBit a0 a1 O (ULeaf b v)
  | b = ULeaf (not b) a0
  | otherwise = ULeaf (not b) a1
-- pseudocode available for the other case in 5/13/16 log.
