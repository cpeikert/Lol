{-# LANGUAGE DataKinds, NoImplicitPrelude #-}

import Crypto.Lol
import Crypto.Lol.Applications.KeyHomomorphicPRF

import MathObj.Matrix

main :: IO ()
main = do
  let (actual1, actual2) = untag $ testComputePRF
      expected1 = fromList 1 4 (fmap fromInteger [2,2,4,2])
      expected2 = fromList 1 4 (fmap fromInteger [2,2,2,2])

  print $ "Test1: " ++ (show $ actual1 == expected1)
  print $ "Test2: " ++ (show $ actual2 == expected2)

-- | Returns a tagged tuple of matrices.
-- | The first matrix is the result of computePRF after the initial augmentation.
-- | The second matrix is the result of computePRF after flipping a bit.
testComputePRF :: Tagged (BaseBGad 2)
  (Matrix (ZqBasic 5 Int), (Matrix (ZqBasic 5 Int)))
testComputePRF = do
      -- Define the base vectors and the desired ring.
      -- The compiler will infer the ring after it is defined once.
  let a0 = fromList 1 4 (fmap fromInteger [7,12,3,7] :: [ZqBasic 13 Int])
      a1 = fromList 1 4 (fmap fromInteger [2,11,6,10])
      -- Define the topology of the full tree.
      -- Note that, in this case, |t| = 4.
      t = Internal 3 1 ()
            (Internal 1 2 ()
              (Leaf () ())
              (Internal 1 1 ()
                (Leaf () ())
                (Leaf () ())
              )
            )
            (Leaf () ())
      -- Define a bitstring of length |t|.
      bits = [False, False, True, False]
      -- Define the secret s.
      s = fromInteger 9
  -- Augment the tree with bits and then the base vectors.
  at <- augmentVector a0 a1 $ augmentBS t bits
  -- Flip the bit of a leaf on the tree and recalculate
  -- the vectors of the affected nodes.
  ft <- flipBit a0 a1 2 at
  return $ (computePRF at s, computePRF ft s)
