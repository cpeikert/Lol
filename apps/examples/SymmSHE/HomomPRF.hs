{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Types

data FullBTree = Leaf
               | Node FullBTree FullBTree

countLeaves :: FullBTree -> Int
countLeaves Leaf = 1
countLeaves (Node l r) = (countLeaves l) + (countLeaves r)

treeA :: forall r gad . (Ring r, Decompose gad r) => [r] -> [r] -> FullBTree -> [Bool] ->  Tagged gad [r]
treeA a0 a1 t x = do
  gadLen <- length <$> (gadget `asTypeOf` return a0)
  let l0 = length a0
      l1 = length a1
      xLen = length x
      magT = countLeaves t
  unless (l0 == l1) $ error "length a0 /= length a1"
  unless (xLen == magT) $ error "length x /= |T|"
  unless (gadLen == l0) $ error "gadget length is not equal to input vector size"
  case t of
    Leaf -> return $ if (head x) then a1 else a0
    (Node left right) -> do
      let (lx, rx) = splitAt (countLeaves left) x
      atl <- treeA a0 a1 left lx
      atr <- treeA a0 a1 right rx
      -- EAC: double check next two lines
      gInvAtr <- mapM decompose atr
      return $ map (sum . zipWith (*) atl . map reduce) gInvAtr

type PublicParams = ([Rq],[Rq], FullBTree)

prf :: (Decompose gad Zq) => PublicParams -> Rq -> [Bool] -> Tagged gad [Rp]
-- EAC: check that I'm rescaling correctly
prf (a0,a1,t) s x = pasteT $ (rescalePow . (s *)) <$> (peelT $ treeA a0 a1 t x)

type Gad = BaseBGad 2
type Zq = ZqBasic 64 Int64
type Zp = ZqBasic 2  Int64
type Rq = Cyc CT F32 Zq
type Rp = Cyc CT F32 Zp

-- takes height of tree
leftSpine :: Int -> FullBTree
leftSpine 0 = Leaf
leftSpine i = Node (leftSpine $ i-1) Leaf

-- takes height of tree
rightSpine :: Int -> FullBTree
rightSpine 0 = Leaf
rightSpine i = Node Leaf (rightSpine $ i-1)

main :: IO ()
main = do
  let l = length $ untag (gadget :: Tagged Gad [Rq])
  a0 <- take l <$> getRandoms
  a1 <- take l <$> getRandoms
  let t = leftSpine 10
      magT = countLeaves t
  x <- take magT <$> getRandoms
  let params = (a0,a1,t)
  s1 <- getRandom
  s2 <- getRandom
  let s3 = s1+s2
      prf1 = proxy (prf params s1 x) (Proxy::Proxy Gad)
      prf2 = proxy (prf params s2 x) (Proxy::Proxy Gad)
      prf3 = proxy (prf params s3 x) (Proxy::Proxy Gad)
      prf3' = prf1+prf2
  print $ prf3 == prf3'
  print prf3
  print prf3'
