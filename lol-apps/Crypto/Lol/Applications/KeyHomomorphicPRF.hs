{-|
Module      : Crypto.Lol.Applications.KeyHomomorphicPRF
Description : Key-homomorphic PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Key-homomorphic PRF from <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crypto.Lol.Applications.KeyHomomorphicPRF
(FullBinTree(..), evalTree
  -- * Convenient trees for the PRF parameter
,randomTree, balancedTree, leftSpineTree, rightSpineTree
  -- * Constructors and types for a PRFFamily
,PRFFamily, makeFamily, randomFamily
,grayCode
  -- * Constructor and type for PRFState
,PRFState, prfState
  -- * PRF using standard lattices
,latticePRF, latticePRFM
  -- * Efficient PRF using rings
,ringPRF, ringPRFM
) where

import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Monad.Random hiding (fromList)
import Control.Monad.State

import Crypto.Lol

import Data.Bits
import Data.Maybe (fromMaybe)

import MathObj.Matrix

-- | Full binary tree.
data FullBinTree = L
                 | I Int FullBinTree FullBinTree

instance NFData FullBinTree where
  rnf L = ()
  rnf (I i t1 t2) = rnf i `seq` rnf t1 `seq` rnf t2

-- | Parameters for PRF
data PRFFamily gad rq rp =
  Params
  (Matrix rq) -- a0
  (Matrix rq) -- a1
  FullBinTree -- tree

instance (NFData rq) => NFData (PRFFamily gad rq rp) where
  rnf (Params m1 m2 t) = rnf m1 `seq` rnf m2 `seq` rnf t

-- | Smart constructor for a PRF family. Ensures that the matrices
-- parameterizing the PRF are square and have the same dimension.
makeFamily :: forall rq rp gad . (Gadget gad rq)
  => Matrix rq -> Matrix rq -> FullBinTree -> PRFFamily gad rq rp
makeFamily a0 a1
  | numRows a0 /= numRows a1 = error $ "a0 has " ++ show (numRows a0) ++
     " rows, but a1 has " ++ show (numRows a1) ++ " rows."
  | numColumns a0 /= (numRows a0)*(length $ untag (gadget :: Tagged gad [rq])) =
     error $ "Expected " ++
       show ((numRows a0)*(length $ untag (gadget :: Tagged gad [rq]))) ++
       " columns in a0, but there are " ++ show (numColumns a0) ++ "."
  | numColumns a1 /= (numRows a1)*(length $ untag (gadget :: Tagged gad [rq])) =
     error $ "Expected " ++
       show ((numRows a1)*(length $ untag (gadget :: Tagged gad [rq]))) ++
       " columns in a1, but there are " ++ show (numColumns a1) ++ "."
  | otherwise = Params a0 a1

-- Internal data structure for saving all of the decompositions in the PRF
-- computation. This allows very fast incremental PRF results when the inputs
-- differ by only a few bits.
data DecoratedTree r =
  -- Leaf node: input bit, output
  DL Int (Matrix r)
  -- Internal node: numleaves, input value, output, left subtree,
  -- decomposed result of right subtree, right subtree
  | DI Int Int (Matrix r) (DecoratedTree r) (Matrix r) (DecoratedTree r)

instance (NFData r) => NFData (DecoratedTree r) where
  rnf (DL i m) = rnf i `seq` rnf m
  rnf (DI i1 i2 m1 d1 m2 d2) =
    rnf i1 `seq` rnf i2 `seq` rnf m1 `seq` rnf d1 `seq` rnf m2 `seq` rnf d2

-- | State of the PRF computation.
data PRFState rq rp where
  PRFState :: (Decompose gad rq)
    => Proxy gad -> Matrix rq -> Matrix rq -> DecoratedTree rq -> PRFState rq rp

instance (NFData rq) => NFData (PRFState rq rp) where
  rnf (PRFState Proxy m1 m2 d) = rnf m1 `seq` rnf m2 `seq` rnf d

-- | Given PRF parameters and an optional inital input value (default is 0),
--   produces an initial PRF state which can be quickly updated when the
--   difference between adjacent inputs ha low Hamming weight.
prfState :: forall gad rq rp . (Decompose gad rq, Reduce (DecompOf rq) rq)
  => PRFFamily gad rq rp -> Maybe Int -> PRFState rq rp
prfState p@(Params a0 a1 t) initInput =
  let treelen = case t of
                 L -> 1
                 (I s _ _) -> s
      input = fromMaybe 0 initInput -- default input is 0
      pgad = Proxy::Proxy gad
  -- ensure the input does not have more bits than the tree has leaves
  in if input >= 0 && input < 2^treelen
     then PRFState pgad a0 a1 $ buildDecTree pgad input p
     else
       error $ "prfState: Input tree has " ++ show treelen ++
         " leaves, but input " ++ show input ++ " has " ++
         show (logBase 2 (fromIntegral input) :: Double) ++ " bits."

-- Helper function for prfState. This constructs the initial internal state for
-- a given initial input value and family parameters. The input must have as
-- many bits as the tree (in the PRFFamily) has leaves.
buildDecTree :: (Decompose gad rq, Reduce (DecompOf rq) rq)
  => Proxy gad -> Int -> PRFFamily gad rq rp -> DecoratedTree rq
buildDecTree pgad y (Params a0 a1 t) =
  let -- number of leaves in a subtree
      getNumLeaves L = 1
      getNumLeaves (I x _ _) = x
      -- buildSubtree returns an (ouput, decorated tree) pair where
      -- the output is the (partial) PRF output for the subtree, and the
      -- decorated tree saves incremental results for optimal recomputation.
      buildSubtree 0 L = (a0, DL 0 a0) -- on input 0, the output is just a0
      buildSubtree 1 L = (a1, DL 1 a1) -- on input 1, the output is just a1
      -- on any other input (corresponding to an internal tree node), split
      -- the input into two halves corresponding to the size of the subtrees,
      -- and recurse. Then piece together the results, saving the
      -- decomposition of rval, which can be reused.
      buildSubtree x (I numLeaves ltree rtree) =
        let numRightLeaves = getNumLeaves rtree
            rbits = x .&. ((2^numRightLeaves)-1) -- mask high bits
            lbits = shift x (-numRightLeaves)    -- negate to shift right
            -- recurse on subtrees
            (lval, ltree') = buildSubtree lbits ltree
            (rval, rtree') = buildSubtree rbits rtree
            -- decompose and compute output
            decompr = fmap reduce $ proxy (decomposeMatrix rval) pgad
            val = lval * decompr
        in (val, DI numLeaves x val ltree' decompr rtree')
  -- we only want to build the tree for an intial input, so discard output
  in snd $ buildSubtree y t

-- | Evaluates the tree at the new input, reusing as much prior work as
-- possible. Ouptuts the PRF output and the new PRF state.
evalTree :: Reduce (DecompOf rq) rq => Int -> PRFState rq rp -> (Matrix rq, PRFState rq rp)
evalTree y (PRFState pgad a0 a1 t) =
  let getNumLeaves (DL _ _) = 1
      getNumLeaves (DI i _ _ _ _ _) = i
      -- outputs result, new state, and flag indicating whether the state changed
      evalSubtree 0 (DL _ _) = (a0, DL 0 a0, False)
      evalSubtree 1 (DL _ _) = (a1, DL 1 a1, False)
      evalSubtree i n@(DI numLeaves x val ltree decompr rtree)
        -- if the input is the same as the value for the subtree,
        -- reuse the entire subtree and memoized result
        | i == x = (val, n, False)
        | otherwise =
            let numRightLeaves = getNumLeaves rtree
                -- split the input according to the subtrees
                rbits = x .&. ((2^numRightLeaves)-1) -- mask high bits
                lbits = shift x (-numRightLeaves)    -- negate to shift right
                -- we don't memoize the left subtree, so we don't care if it changed
                (lval, ltree', _) = evalSubtree lbits ltree
                (rval, rtree', changed) = evalSubtree rbits rtree
                -- if the right subtree changed, updated the memoized value
                decompr' = if changed
                           then fmap reduce $ proxy (decomposeMatrix rval) pgad
                           else decompr
                -- and compute the new output
                val' = lval * decompr'
            -- since we're in this branch, something in the tree changed (either
            -- the left or right subtree, or both), so we have a new value.
            in (val', DI numLeaves i val' ltree' decompr' rtree', True)
      (res, t', _) = evalSubtree y t
  in (res, PRFState pgad a0 a1 t')

-- | Equation (2.3) in <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
latticePRF' :: (Rescale zq zp, Reduce (DecompOf zq) zq)
  => Matrix zq -> Int -> PRFState zq zp -> (Matrix zp, PRFState zq zp)
latticePRF' s x state1@(PRFState _ a0 _ _)
  | numRows s /= 1 = error "Secret key must have one row."
  | numColumns s /= numRows a0 = error $ "Secret key has " ++
     show (numColumns s) ++ " columns, but a0 has " ++
     show (numRows a0) ++ " rows."
  | otherwise = let (res,state2) = evalTree x state1
                in (rescale <$> s*res, state2)

-- | Single-ouptut lattice PRF.
latticePRF :: (Rescale zq zp, Reduce (DecompOf zq) zq)
  => Matrix zq -> Int -> PRFState zq zp -> Matrix zp
latticePRF s x = fst. latticePRF' s x

-- | Multi-output lattice PRF with monadic memoized internal state.
latticePRFM :: (MonadState (PRFState zq zp) mon, Rescale zq zp, Reduce (DecompOf zq) zq)
  => Matrix zq -> Int -> mon (Matrix zp)
latticePRFM s x = state $ latticePRF' s x

-- | Equation (2.10) in <http://web.eecs.umich.edu/~cpeikert/pubs/kh-prf.pdf [BP14]>.
ringPRF' :: (Fact m, RescaleCyc c zq zp, Ring rq,
            rq ~ c m zq, rp ~ c m zp, Reduce (DecompOf (c m zq)) (c m zq))
    => rq -> Int -> PRFState rq rp -> (Matrix rp, PRFState rq rp)
ringPRF' s x state1 =
  let (res,state2) = evalTree x state1
  in ((rescaleDec . (s*)) <$> res, state2)

-- | Single-use ring PRF. Discards the state, so future evaluations are slow.
ringPRF :: (Fact m, RescaleCyc c zq zp, Reduce (DecompOf (c m zq)) (c m zq),
            Ring rq, rq ~ c m zq, rp ~ c m zp)
    => rq -> Int -> PRFState rq rp -> Matrix rp
ringPRF s x = fst . ringPRF' s x

-- | Ring PRF with monadic memoized internal state for fast incremental evaluation.
ringPRFM :: (MonadState (PRFState rq rp) mon, Fact m, RescaleCyc c zq zp,
             Reduce (DecompOf (c m zq)) (c m zq), Ring rq,
             rq ~ c m zq, rp ~ c m zp)
  => rq -> Int -> mon (Matrix rp)
ringPRFM s x = state $ ringPRF' s x

-- convenience functions

-- | Given the desired number of leaves, produces a random full binary tree.
randomTree :: (MonadRandom rnd) => Int -> rnd FullBinTree
randomTree 1 = return L -- random tree with one leaf isn't very random
randomTree i = do
  -- each subtree must have at least one node, or the tree won't be full!
  leftSize <- getRandomR (1,i-1)
  left <- randomTree leftSize
  right <- randomTree $ i-leftSize
  return $ I i left right

-- | Given the desired number of leaves, produces a full binary left-spine tree.
leftSpineTree :: Int -> FullBinTree
leftSpineTree 1 = L
leftSpineTree i = I i (leftSpineTree $ i-1) L

-- | Given the desired number of leaves, produces a full binary right-spine tree.
rightSpineTree :: Int -> FullBinTree
rightSpineTree 1 = L
rightSpineTree i = I i L (rightSpineTree $ i-1)

-- | Given the desired number of leaves, produces a complete, full binary tree
balancedTree :: Int -> FullBinTree
balancedTree 1 = L
balancedTree i =
  -- the number of leaves in the last full level of the tree is the most
  -- significant bit of i
  let lastFullLevelSize = 2^(floor (logBase 2 (fromIntegral i) :: Double) :: Int)
      -- Two cases for the right subtree: either it is perfect (so we put
      -- lastFullLevelSize `div` 2 leaves in both subtrees) or it isn't, in
      -- which case we make the left subtree as full as possible (which is
      -- lastFullLevelSize leaves), and give the rest to the right subtree.
      lsize = min lastFullLevelSize $ i-(lastFullLevelSize `div` 2)
      rsize = i-lsize
  in I i (balancedTree lsize) (balancedTree rsize)

-- | Randomly generated ring-based PRF family.
randomFamily :: forall gad rnd rq rp .
  (MonadRandom rnd, Random rq, Gadget gad rq)
  => Int -> rnd (PRFFamily gad rq rp)
randomFamily size = do -- in rnd
  t <- randomTree size
  let len = length $ untag (gadget :: Tagged gad [rq])
  -- randomly generate two square (len x len) matrices
  a0 <- fromList 1 len <$> take len <$> getRandoms
  a1 <- fromList 1 len <$> take len <$> getRandoms
  return $ makeFamily a0 a1 t

-- | Constructs an n-bit Gray code, useful for efficiently evaluating the PRF.
grayCode :: Int -> [Int]
grayCode 1 = [0,1] -- 1 bit gray code has two values
-- n-bit code is the (n-1)-bit code, followed by the reversed (n-1)-bit code,
-- with an extra bit on top
grayCode n =
  let gc' = grayCode (n-1)
      pow2 = 2^(n-1)
      rightHalf = map (+pow2) $ reverse gc'
  in gc' ++ rightHalf
