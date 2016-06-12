{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, FlexibleContexts,
             GADTs, InstanceSigs, KindSignatures, NoImplicitPrelude, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators, UndecidableInstances #-}

-- An implementation of the ring-LWE key-homomorphic PRF from [BP14].

-- TODO: Determine exactly which functions should export.
module Crypto.Lol.Applications.KeyHomomorphicPRF
( combineVectors
, decomposeEntries
, MMatrix
, uAugmentBS
, uAugmentVector
, uComputePRF
, uFlipBit
, UFullTree(..)
, uRootValue
) where

import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude
import Crypto.Lol.PosBin

import Data.List as L

import MathObj.Matrix as M

type MMatrix a = M.T a

-- | Unsafe full tree.
data UFullTree l v where
  ULeaf :: l -> v -> UFullTree l v
  UInternal :: Int -> Int -> v ->
               UFullTree l v ->
               UFullTree l v ->
               UFullTree l v

-- | Returns the vertex type attached to the UFullTree.
uRootValue :: UFullTree l v -> v
uRootValue (ULeaf _ v) = v
uRootValue (UInternal _ _ v _ _) = v

-- | Augments the leaves of the UFullTree with Bool values.
uAugmentBS :: UFullTree () () -> -- ^ Topology of T
              [Bool] -> -- ^ Bitstring x of size |T|
              UFullTree Bool () -- ^ Bit on each leaf of T
uAugmentBS (ULeaf _ _) [bit] = ULeaf bit ()
uAugmentBS (UInternal ls rs _ left right) bits =
  let (leftBits, rightBits) = splitAt ls bits
  in UInternal ls rs () (uAugmentBS left leftBits) (uAugmentBS right rightBits)

-- | Augments the nodes of the UFullTree with MMatrix values.
uAugmentVector :: (Decompose gad a) =>
                  Tagged gad (MMatrix a) -> -- ^ Base vector a0
                  Tagged gad (MMatrix a) -> -- ^ Base vector a1
                  UFullTree Bool () -> -- ^ Bit on each leaf of T
                  UFullTree Bool (Tagged gad (MMatrix a)) -- ^ Matrix at each node of T
uAugmentVector a0 a1 (ULeaf b _) =
  ULeaf b $ if b then a1 else a0
uAugmentVector a0 a1 (UInternal nl nr _ l r) =
  let l' = uAugmentVector a0 a1 l
      r' = uAugmentVector a0 a1 r
      c = combineVectors (uRootValue l') (uRootValue r')
  in (UInternal nl nr c l' r')

-- | Equation (2.10) in [BP14] using an unsafe full tree.
uComputePRF :: (Ring a, Ring b, Rescale a b) =>
               UFullTree l (Tagged gad (MMatrix a)) -> -- ^ Matrix at each node of T
               a -> -- ^ secret s
               MMatrix b -- ^ Output matrix
uComputePRF t s =
  let m = untag $ uRootValue t
  in fmap (rescale . (*s)) m

-- | Flip the boolean value at a chosen leaf.
-- | Updates the affected matrices at each node.
uFlipBit :: (Decompose gad a) =>
            Tagged gad (MMatrix a) -> -- ^ Base vector a0
            Tagged gad (MMatrix a) -> -- ^ Base vector a1
            Int -> -- ^ which bit to flip (1-indexed)
            UFullTree Bool (Tagged gad (MMatrix a)) -> -- ^ Matrix at each node of T
            UFullTree Bool (Tagged gad (MMatrix a)) -- ^ Matrix at each node of T
uFlipBit a0 a1 _ (ULeaf b v) =
  ULeaf (not b) $ if b then a0 else a1
uFlipBit a0 a1 n (UInternal nl nr v l r)
  | (n > nl) =
    let r' = uFlipBit a0 a1 (n - nl) r
    in UInternal nl nr (combineVectors (uRootValue l) (uRootValue r')) l r'
  | otherwise =
    let l' = uFlipBit a0 a1 n l
    in UInternal nl nr (combineVectors (uRootValue l') (uRootValue r)) l' r

-- | Decomposes the entries of a 1xn MMatrix. Returns an nxn MMatrix.
decomposeEntries :: (Decompose gad a) =>
                    Tagged gad (MMatrix a) ->
                    Tagged gad (MMatrix (DecompOf a))
decomposeEntries tm = do
  m <- tm
  let n = M.numColumns m
  [tl] <- sequence $ fmap (sequence . (fmap decompose)) $ M.rows m
  return $ M.fromRows n n $ L.transpose $ fmap (take n) tl

-- | Multiply two vectors as given in the
-- | "otherwise" case of Equation (2.9) in [BP14].
combineVectors :: (Decompose gad a) =>
                  Tagged gad (MMatrix a) ->
                  Tagged gad (MMatrix a) ->
                  Tagged gad (MMatrix a)
combineVectors tl tr = do
  l <- tl
  r <- decomposeEntries tr
  return $ l * (fmap reduce r)
