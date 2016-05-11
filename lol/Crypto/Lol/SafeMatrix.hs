{-# LANGUAGE ConstraintKinds, DataKinds, ExplicitNamespaces, GADTs,
             InstanceSigs, KindSignatures, PolyKinds, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- For safety, don't export the constructor.
module Crypto.Lol.SafeMatrix
( fromList'
, fromMatrix
, multStd'
, SafeMatrix
) where

--import Algebra.Ring

import Crypto.Lol.PosBinDefs
import Crypto.Lol.SafeBitString

import qualified Data.Matrix as M

-- Read as a matrix with some # of rows and some # of columns.
data SafeMatrix a = SafeMatrix (M.Matrix a) Int Int
  deriving Show

toList' :: SafeMatrix a -> [a]
toList' (SafeMatrix m r c) = M.toList m

-- Convenient constructor for the client.
-- Method is needed to multiply Pos types.
fromList' :: Int -> -- ^ Rows (nl :: Pos)
             Int -> -- ^ Columns (nr :: Pos)
             [a] -> -- ^ List of elements
             Maybe (SafeMatrix a)
fromList' n m elts
  | (n*m == length elts) = Just (SafeMatrix (M.fromList n m elts) n m)
  | otherwise = Nothing

-- An alternative constructor.
-- Unsafe.
fromMatrix :: M.Matrix a -> SafeMatrix a
fromMatrix m = SafeMatrix m (M.nrows m) (M.ncols m)

-- Change to Ring a? How to import Ring? Algebra.Ring?
-- How to use function constraints to ensure c1 == r2?
multStd' :: Num a => SafeMatrix a -> SafeMatrix a -> Maybe (SafeMatrix a)
multStd' (SafeMatrix m1 r1 c1) (SafeMatrix m2 r2 c2)
  | (c1 == r2) = Just (SafeMatrix (M.multStd m1 m2) r1 c2)
  | otherwise = Nothing
