{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, FlexibleContexts, GADTs,
             InstanceSigs, KindSignatures, PartialTypeSignatures, PolyKinds,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module Crypto.Lol.MMatrix (
combineVectors,
decomposeEntries,
matrixMult
) where

import Crypto.Lol.Gadget
import Crypto.Lol.LatticePrelude
import Crypto.Lol.PosBinDefs
import Crypto.Lol.Reflects
import Crypto.Lol.SafeBitString

import Crypto.Lol.Types.Numeric as N

import Data.Functor.Trans.Tagged

import qualified MathObj.Matrix as M

-- | Decomposes the entries of a 1xn MMatrix. Returns an nxn MMatrix.
decomposeEntries :: forall a.
                    (Decompose (BaseBGad 2) a) =>
                    MMatrix a -> MMatrix (DecompOf a)
decomposeEntries m =
  let n = M.numColumns m
  in M.fromColumns n n ((fmap (take n . untag) taggedList))
  where taggedList = fmap decompose $ concat $ M.rows m :: [Tagged (BaseBGad 2) [DecompOf a]]

-- | Performs matrix multiplication for MMatrix types.
matrixMult :: (Ring a) => MMatrix a -> MMatrix a -> MMatrix a
matrixMult m1 m2 =
  let elts = [N.sum $ N.zipWith (N.*) xs ys | xs <- (M.rows m1), ys <- (M.columns m2)]
  in (M.fromList (M.numRows m1) (M.numColumns m2) elts)

-- | Multiply two vectors as given in the
-- | "otherwise" case of Equation (2.9) in [BP14].
combineVectors :: (Ring (DecompOf a), Lift b a, Reduce (DecompOf a) a,
      Decompose (BaseBGad 2) a, DecompOf a ~ LiftOf b) =>
      MMatrix b ->
      MMatrix a ->
      MMatrix a
combineVectors l r = fmap reduce (matrixMult (fmap lift l) (decomposeEntries r))
