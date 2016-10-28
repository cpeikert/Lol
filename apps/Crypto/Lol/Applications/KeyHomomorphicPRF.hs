{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Key-homomorphic PRF from [BP14].

module Crypto.Lol.Applications.KeyHomomorphicPRF
(FullBinTree(..)
,randomTree, balancedTree, leftSpineTree, rightSpineTree
,PRFFamily, makeFamily, randomFamily
,PRFState, prfState
,latticePRF, latticePRFM
,ringPRF, ringPRFM
) where

import Control.Applicative ((<$>))
import Control.Monad.Random hiding (fromList)
import Control.Monad.State

import Crypto.Lol

import Data.Bits
import Data.Maybe (fromMaybe)

import MathObj.Matrix

-- | Full binary tree.
data FullBinTree = L
                 | I Int FullBinTree FullBinTree

-- | Parameters for PRF
data PRFFamily gad rq rp = Params (Matrix rq) (Matrix rq) FullBinTree -- | a0, a1, tree

-- | Smart constructor
makeFamily :: forall rq rp gad . (Gadget gad rq)
  => Matrix rq -> Matrix rq -> FullBinTree -> PRFFamily gad rq rp
makeFamily a0 a1
  | numRows a0 /= numRows a1 = error $ "a0 has " ++ show (numRows a0) ++
     " rows, but a1 has " ++ show (numRows a1) ++ " rows."
  | numColumns a0 /= (numRows a0)*(length $ untag (gadget :: Tagged gad [rq])) =
     error $ "Expected " ++ show ((numRows a0)*(length $ untag (gadget :: Tagged gad [rq]))) ++
       " columns in a0, but there are " ++ show (numColumns a0) ++ "."
  | numColumns a1 /= (numRows a1)*(length $ untag (gadget :: Tagged gad [rq])) =
     error $ "Expected " ++ show ((numRows a1)*(length $ untag (gadget :: Tagged gad [rq]))) ++
       " columns in a1, but there are " ++ show (numColumns a1) ++ "."
  | otherwise = Params a0 a1

-- not exported,
data DecoratedTree r = DL Int (Matrix r) -- | input bit, output
                     | DI Int Int (Matrix r) (DecoratedTree r) (DecoratedTree r) -- | numleaves, input value, output, left subtree, right subtree

-- | State of the PRF computation. This permits incremental computation.
data PRFState rq rp where
  PRFState :: (Decompose gad rq)
    => Proxy gad -> Matrix rq -> Matrix rq -> DecoratedTree rq -> PRFState rq rp

-- | Given PRF parameters and an optional inital input value (default is 0),
--   produces an initial PRF state.
prfState :: forall gad rq rp . (Decompose gad rq)
  => PRFFamily gad rq rp -> Maybe Int -> PRFState rq rp
prfState p@(Params a0 a1 t) initInput =
  let treelen = case t of
                 L -> 1
                 (I s _ _) -> s
      input = fromMaybe 0 initInput -- default input is 0
      inputGuard = input >= 0 && input < 2^treelen
      pgad = Proxy::Proxy gad
  in if inputGuard
     then PRFState pgad a0 a1 $ snd $ buildDecTree pgad input p
     else
       error $ "prfState: Input tree has " ++ show treelen ++
         " leaves, but input " ++ show input ++ " has " ++
         show (logBase 2 (fromIntegral input) :: Double) ++ " bits."

combineNodes :: (Decompose gad rq) => (Int -> a -> (Matrix rq, b)) -> Proxy gad -> Int -> a -> a -> Int -> (Matrix rq, b, b)
combineNodes go pgad x ltree rtree numRightLeaves =
  let rbits = x .&. ((2^numRightLeaves)-1) -- mask high bits
      lbits = shift x (-numRightLeaves)    -- negate to shift right
      (lval, ltree') = go lbits ltree
      (rval, rtree') = go rbits rtree
      val' = proxy (combineVectors lval rval) pgad
  in (val', ltree', rtree')

-- given validated parameters, constructs a decorated tree with the given input
buildDecTree :: (Decompose gad rq)
  => Proxy gad -> Int -> PRFFamily gad rq rp -> (Matrix rq, DecoratedTree rq)
buildDecTree pgad y (Params a0 a1 t) =
  let getNumLeaves L = 1
      getNumLeaves (I i _ _) = i
      go 0 L = (a0, DL 0 a0)
      go 1 L = (a1, DL 1 a1)
      go i (I numLeaves ltree rtree) =
        let (val, ltree', rtree') = combineNodes go pgad i ltree rtree (getNumLeaves rtree)
        in (val, DI numLeaves i val ltree' rtree')
  in go y t

-- EAC: an optional time-space tradeoff: store the decomposed right tree
-- so that if only the left tree changes, we don't have to re-decompose
-- the right tree.
-- | Evaluates the tree at the new input, reusing as much prior work as possible.
evalTree :: Int -> PRFState rq rp -> (Matrix rq, PRFState rq rp)
evalTree y (PRFState gad a0 a1 t) =
  let getNumLeaves (DL _ _) = 1
      getNumLeaves (DI i _ _ _ _) = i
      go 0 (DL _ _) = (a0, DL 0 a0)
      go 1 (DL _ _) = (a1, DL 1 a1)
      go i n@(DI numLeaves x val ltree rtree) | i == x = (val,n)
                                              | otherwise =
        let (val', ltree', rtree') = combineNodes go gad i ltree rtree (getNumLeaves rtree)
        in (val', DI numLeaves i val' ltree' rtree')
      (res, t') = go y t
  in (res, PRFState gad a0 a1 t')

-- | Equation (2.3) in [BP14]
latticePRF :: (Rescale zq zp)
  => Matrix zq -> Int -> PRFState zq zp -> (Matrix zp, PRFState zq zp)
latticePRF s x state@(PRFState _ a0 _ _)
  | numRows s /= 1 = error "Secret key must have one row."
  | numColumns s /= numRows a0 = error $ "Secret key has " ++
     show (numColumns s) ++ " columns, but a0 has " ++ show (numRows a0) ++ " rows."
  | otherwise = let (res,state') = evalTree x state
                in (rescale <$> s*res, state')

latticePRFM :: (Monad mon, Rescale zq zp)
  => Matrix zq -> Int -> StateT (PRFState zq zp) mon (Matrix zp)
latticePRFM s x = StateT $ return . latticePRF s x

-- | Equation (2.10) in [BP14].
ringPRF :: (Fact m, RescaleCyc (Cyc t) zq zp, Ring rq, rq ~ Cyc t m zq, rp ~ Cyc t m zp)
    => rq -> Int -> PRFState rq rp -> (Matrix rp, PRFState rq rp)
ringPRF s x state =
  let (res,state') = evalTree x state
  in ((rescalePow . (s*)) <$> res, state')

ringPRFM :: (Monad mon, Fact m, RescaleCyc (Cyc t) zq zp, Ring rq, rq ~ Cyc t m zq, rp ~ Cyc t m zp)
  => rq -> Int -> StateT (PRFState rq rp) mon (Matrix rp)
ringPRFM s x = StateT $ return . ringPRF s x

-- | Multiply two matrices as given in the
-- | "otherwise" case of Equation (2.9) in [BP14].
combineVectors :: (Decompose gad r) =>
                  Matrix r ->
                  Matrix r ->
                  Tagged gad (Matrix r)
combineVectors l r = do
  dr <- decomposeMatrix r
  return $ l * fmap reduce dr



-- convenience functions

-- | Given the desired number of leaves, produces a random full binary tree.
randomTree :: (MonadRandom rnd) => Int -> rnd FullBinTree
randomTree 1 = return L
randomTree i = do
  leftSize <- getRandomR (1,i-1)
  left <- randomTree leftSize
  right <- randomTree $ i-leftSize
  return $ I i left right

-- | Given the desired number of leaves, produces a full binary right-spine tree.
leftSpineTree :: Int -> FullBinTree
leftSpineTree 1 = L
leftSpineTree i = I i (leftSpineTree $ i-1) L

-- | Given the desired number of leaves, produces a full binary left-spine tree.
rightSpineTree :: Int -> FullBinTree
rightSpineTree 1 = L
rightSpineTree i = I i L (rightSpineTree $ i-1)

-- | Given the desired number of leaves, produces a full binary tree which is complete,
-- except possibly for the last level, which is left-biased.
balancedTree :: Int -> FullBinTree
balancedTree 1 = L
balancedTree i =
  let lastFullLevelSize = 2^(floor (logBase 2 (fromIntegral i) :: Double) :: Int)
      lsize = min lastFullLevelSize $ i-(lastFullLevelSize `div` 2)
      rsize = i-lsize
  in I i (balancedTree lsize) (balancedTree rsize)

-- | Randomly generate ring-based PRF family.
randomFamily :: forall gad rnd rq rp . (MonadRandom rnd, Random rq, Gadget gad rq)
  => Int -> rnd (PRFFamily gad rq rp)
randomFamily size = do -- in rnd
  t <- randomTree size
  let len = length $ untag (gadget :: Tagged gad [rq])
  a0 <- fromList 1 len <$> take len <$> getRandoms
  a1 <- fromList 1 len <$> take len <$> getRandoms
  return $ makeFamily a0 a1 t
