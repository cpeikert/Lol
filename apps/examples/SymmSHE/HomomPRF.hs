{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types

import Tests

import qualified Test.Framework as TF



data FullBTree = Leaf
               | Node FullBTree FullBTree

countLeaves :: FullBTree -> Int
countLeaves Leaf = 1
countLeaves (Node l r) = (countLeaves l) + (countLeaves r)

-- takes number of leaves = |T|
randomTree :: (MonadRandom rnd) => Int -> rnd FullBTree
randomTree 1 = return Leaf
randomTree i = do
  leftSize <- getRandomR (1,i-1)
  left <- randomTree leftSize
  right <- randomTree $ i-leftSize
  return $ Node left right

-- takes number of leaves
leftSpine :: Int -> FullBTree
leftSpine 1 = Leaf
leftSpine i = Node (leftSpine $ i-1) Leaf

-- takes number of leaves
rightSpine :: Int -> FullBTree
rightSpine 1 = Leaf
rightSpine i = Node Leaf (rightSpine $ i-1)






data PublicParams gad r = Params [r] [r] FullBTree
type Bits = [Bool]

-- takes input size = |T|
randomPRFInst :: forall gad rnd r . (MonadRandom rnd, Random r, Gadget gad r)
  => Int -> rnd (PublicParams gad r)
randomPRFInst tsize = do
  t <- randomTree tsize
  let l = length $ untag (gadget :: Tagged gad [r])
  a0 <- take l <$> getRandoms
  a1 <- take l <$> getRandoms
  return $ Params a0 a1 t

treeA :: forall gad r . (Ring r, Decompose gad r)
  => PublicParams gad r -> Bits ->  [r]
treeA p@(Params a0 a1 t) x = do
  let gadLen = length $ untag (gadget :: Tagged gad [r])
      l0 = length a0
      l1 = length a1
      xLen = length x
      magT = countLeaves t
      check | l0 /= l1 = error "length a0 /= length a1"
            | xLen /= magT = error "length x /= |T|"
            | gadLen /= l0 = error "gadget length is not equal to input vector size"
            | otherwise = ()
  check `seq` case t of
    Leaf -> if (head x) then a1 else a0
    (Node left right) ->
      let (lx, rx) = splitAt (countLeaves left) x
          lparams = Params a0 a1 left `asTypeOf` p
          rparams = Params a0 a1 right `asTypeOf` p
          atl = treeA lparams lx
          atr = treeA rparams rx
          -- EAC: double check next two lines
          gInvAtr = proxy (mapM decompose atr) (Proxy::Proxy gad)
      in map (sum . zipWith (*) atl . map reduce) gInvAtr

prf :: (Decompose gad rq, Fact m, RescaleCyc (Cyc t) zq zp, rq ~ Cyc t m zq)
    => PublicParams gad rq -> rq -> Bits -> [Cyc t m zp]
-- EAC: check that I'm rescaling correctly
prf params s x = (rescalePow . (s *)) <$> treeA params x


prop_keyHomom :: forall t m (zp :: *) zq gad . (Fact m, CElt t zq, _) => Int -> Test '(t,m,zp,zq,gad)
prop_keyHomom size = testIO $ do
  prfInst :: PublicParams gad (Cyc t m zq) <- randomPRFInst size
  s1 <- getRandom
  s2 <- getRandom
  x <- take size <$> getRandoms
  let s3 = s1+s2
      prf1 = prf prfInst s1 x
      prf2 = prf prfInst s2 x
      prf3 = prf prfInst s3 x
      prf3' = prf1+prf2 :: [Cyc t m zp]
  return $ prf3 == prf3'






type Gad = BaseBGad 2
type Zq = ZqBasic 64 Int64
type Zp = ZqBasic 2  Int64
type Rq = Cyc CT F32 Zq
type Rp = Cyc CT F32 Zp

main :: IO ()
main =
  let size = 10
  in TF.defaultMain =<< (sequence
      [hideArgs
        "key homomorphism"
        (prop_keyHomom size)
        (Proxy::Proxy '(CT, F32, Zp, Zq, Gad))] :: IO [TF.Test])
