{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, FlexibleContexts, GADTs,
             InstanceSigs, KindSignatures, PartialTypeSignatures, PolyKinds,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Crypto.Lol.FullTree
( augmentBitString
, decomposeEntries
, FullTree(..)
, matrixMult
) where

import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude
import Crypto.Lol.PosBinDefs
import Crypto.Lol.Reflects
import Crypto.Lol.SafeBitString
import Crypto.Lol.SafeMatrix

import Crypto.Lol.Types.Numeric as N
import Crypto.Lol.Types.ZqBasic

import Data.Functor.Trans.Tagged

import qualified MathObj.Matrix as M

-- n = numLeaves, l = leafType, v = vectorRq
data FullTree (n :: Pos) l v where
  Leaf :: l -> v -> FullTree O l v
  Internal :: v -> FullTree nl l v
                -> FullTree nr l v
                -> FullTree (AddPos nl nr) l v

-- check that length of the bitstring == n.
-- But how to do that in the type signature?
augmentBitString :: FullTree n () () -> -- ^ Full tree T (topology)
                    SafeBitString n -> -- Bit string x
                    FullTree n Bool () -- ^ Full tree T (bit on each leaf)
augmentBitString (Leaf _ _) (Bit b) = Leaf b ()
--augmentBitString (Internal _ left right) bits =
  --let (leftBits, rightBits) = splitSBS bits
  --in Internal () (augmentBitString left leftBits) (augmentBitString right rightBits)

-- Use SafeRowVector instead of SafeMatrix for the base vectors a0, a1.
{- augmentVector :: SafeMatrix a -> -- ^ Base vector a0
                 SafeMatrix a -> -- ^ Base vector a1
                 FullTree (n :: Pos) Bool () -> -- ^ Full tree T (bit on each leaf)
                 FullTree (n :: Pos) Bool (SafeMatrix a) -- ^ Full tree T (calculated a_T(x))
augmentVector a0 a1 (Leaf b _)
  | b = Leaf b a1
  | otherwise = Leaf b a0
augmentVector a0 a1 (Internal _ l r) =
  let l' = augmentVector a0 a1 l -- use pattern matching for l' and r'?
      r' = augmentVector a0 a1 r -- so that we can obtain the internal vectors?
      c = a0 -- wrong, but I think this is where decompose goes.
  in (Internal c l' r')
-}

-- The input is a row vector of dimension 1xn.
-- The output is a nxn matrix.
decomposeEntries :: forall s u.
                    (Ring u, Reduce [DecompOf u] [u], Decompose (BaseBGad 2) u) =>
                    MMatrix u ->
                    MMatrix u
decomposeEntries m =
  let n = M.numColumns m
  in M.fromColumns n n ((fmap (reduce . take n . untag) taggedList) :: [[u]])
  where taggedList = fmap decompose $ concat $ M.rows m :: [Tagged (BaseBGad 2) [DecompOf u]]

-- Only tested with 1xn * nxn matrix. Additional testing needed.
matrixMult :: (Ring a) => MMatrix a -> MMatrix a -> MMatrix a
matrixMult m1 m2 =
  let elts = [N.sum $ zipWith (N.*) xs ys | xs <- (M.rows m1), ys <- (M.columns m2)]
  in (M.fromList (M.numRows m1) (M.numColumns m2) elts)
