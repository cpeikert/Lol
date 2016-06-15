{-# LANGUAGE GADTs, NoImplicitPrelude #-}

-- Ring-LWE key-homomorphic PRF from [BP14].

module Crypto.Lol.Applications.KeyHomomorphicPRF
( augmentBS
, augmentVector
, computePRF
, flipBit
, FullTree(..)
) where

import Control.Applicative

import Crypto.Lol

-- | Unsafe full tree.
data FullTree l v where
  Leaf :: l -> v -> FullTree l v
  Internal :: Int -> Int -> v ->
              FullTree l v ->
              FullTree l v ->
              FullTree l v

-- | Returns the vertex type attached to the FullTree.
rootValue :: FullTree l v -> v
rootValue (Leaf _ v) = v
rootValue (Internal _ _ v _ _) = v

-- | Augments the leaves of the FullTree with Bool values.
augmentBS :: FullTree () () -> -- ^ Topology of T
             [Bool] -> -- ^ Bitstring x of size |T|
             FullTree Bool () -- ^ Bit on each leaf of T
augmentBS (Leaf _ _) [bit] = Leaf bit ()
augmentBS (Internal ls rs _ left right) bits =
  let (leftBits, rightBits) = splitAt ls bits
  in Internal ls rs () (augmentBS left leftBits) (augmentBS right rightBits)

-- | Augments the nodes of the FullTree with Matrix values.
-- | Note: The base vectors must have the same number
-- | of entries as the gadget with which these vectors are decomposed.
augmentVector :: Decompose gad a =>
                 Matrix a -> -- ^ Base vector a0
                 Matrix a -> -- ^ Base vector a1
                 FullTree Bool () -> -- ^ Bit on each leaf of T
                 Tagged gad (FullTree Bool (Matrix a)) -- ^ Matrix at nodes of T
augmentVector a0 a1 (Leaf b _) = do
  return $ Leaf b $ if b then a1 else a0
augmentVector a0 a1 (Internal nl nr _ l r) = do
  l' <- augmentVector a0 a1 l
  r' <- augmentVector a0 a1 r
  c <- combineVectors (rootValue l') (rootValue r')
  return $ Internal nl nr c l' r'

-- | Equation (2.10) in [BP14].
computePRF :: (Ring a, Ring b, Rescale a b) =>
              FullTree l (Matrix a) -> -- ^ Matrix at nodes of T
              a -> -- ^ secret s
              Matrix b -- ^ Final result
computePRF t s = rescale . (*s) <$> rootValue t

-- | Flip the boolean value at a chosen leaf.
-- | Updates the affected matrices at each node.
flipBit :: Decompose gad a =>
           Matrix a -> -- ^ Base vector a0
           Matrix a -> -- ^ Base vector a1
           Int -> -- ^ which bit to flip (1-indexed)
           FullTree Bool (Matrix a) -> -- ^ Matrix at nodes of T
           Tagged gad (FullTree Bool (Matrix a)) -- ^ Matrix at nodes of T
flipBit a0 a1 _ (Leaf b _) = do
  return $ Leaf (not b) $ if b then a0 else a1
flipBit a0 a1 n (Internal nl nr _ l r)
  | (n > nl) = do
    r' <- flipBit a0 a1 (n - nl) r
    c <- combineVectors (rootValue l) (rootValue r')
    return $ Internal nl nr c l r'
  | otherwise = do
    l' <- flipBit a0 a1 n l
    c <- combineVectors (rootValue l') (rootValue r)
    return $ Internal nl nr c l' r

-- | Multiply two matrices as given in the
-- | "otherwise" case of Equation (2.9) in [BP14].
combineVectors :: Decompose gad a =>
                  Matrix a ->
                  Matrix a ->
                  Tagged gad (Matrix a)
combineVectors l tr = do
  r <- decomposeMatrix tr
  return $ l * (fmap reduce r)
