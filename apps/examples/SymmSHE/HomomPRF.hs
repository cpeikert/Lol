{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Cyclotomic.UCyc
import Crypto.Lol.Types

import Tests

import qualified Test.Framework as TF

type Bits = [Bool]
type TreeFamily gad r = [r] -> [r] -> Bits -> Tagged gad [r]

leafFamily :: TreeFamily gad r
leafFamily a0 a1 [x] = return $ if x then a1 else a0

-- takes the size of the first input tree
joinFamily :: (Reduce (DecompOf r) r, Decompose gad r) => Int -> TreeFamily gad r -> TreeFamily gad r -> TreeFamily gad r
joinFamily lsize left right a0 a1 xs = do
  let (lx,rx) = splitAt lsize xs
  atl <- left a0 a1 lx
  atr <- right a0 a1 rx
  gInvAtr <- mapM decompose atr
  return $ map (sum . zipWith (*) atl . map reduce) gInvAtr

-- takes number of leaves = |T|
randomTree :: (MonadRandom rnd, Decompose gad r) => Int -> rnd (TreeFamily gad r)
randomTree 1 = return leafFamily
randomTree i = do
  leftSize <- getRandomR (1,i-1)
  left <- randomTree leftSize
  right <- randomTree $ i-leftSize
  return $ joinFamily leftSize left right

-- takes number of leaves = |T|
leftSpine :: (Decompose gad r) => Int -> TreeFamily gad r
leftSpine 1 = leafFamily
leftSpine i = joinFamily (i-1) (leftSpine $ i-1) leafFamily

-- takes number of leaves = |T|
rightSpine :: (Decompose gad r) => Int -> TreeFamily gad r
rightSpine 1 = leafFamily
rightSpine i = joinFamily 1 leafFamily (rightSpine $ i-1)

newtype TreeInst gad r = TI (Bits -> [r])

-- takes input size = |T|
randomTreeInst :: forall gad rnd r . (MonadRandom rnd, Random r, Decompose gad r)
  => Int -> rnd (TreeInst gad r)
randomTreeInst size = do -- in rnd
  f <- randomTree size
  let l = length $ untag (gadget :: Tagged gad [r])
  a0 <- take l <$> getRandoms
  a1 <- take l <$> getRandoms
  return $ TI $ \x -> proxy (f a0 a1 x) (Proxy::Proxy gad)

prf :: (Decompose gad rq, Fact m, RescaleCyc (Cyc t) zq zp, rq ~ Cyc t m zq)
    => TreeInst gad rq -> rq -> Bits -> [Cyc t m zp]
prf (TI f) s = map (rescalePow . (s *)) . f

-- +/-1 in every coefficient of the rounding basis
prop_keyHomom :: forall t m (zp :: *) zq gad . (Fact m, CElt t zq, _) => Int -> Test '(t,m,zp,zq,gad)
prop_keyHomom size = testIO $ do
  treeInst :: TreeInst gad (Cyc t m zq) <- randomTreeInst size
  s1 <- getRandom
  s2 <- getRandom
  x <- take size <$> getRandoms
  let s3 = s1+s2
      prf1 = prf treeInst s1 x
      prf2 = prf treeInst s2 x
      prf3 = prf treeInst s3 x
      prf3' = prf1+prf2 :: [Cyc t m zp]
      a = map uncycPow prf3
      b = map uncycPow prf3'
      c = zipWith (-) a b
      c' = map (maximum . fmapPow abs . lift) c
  return $ maximum c' <=1

type Gad = BaseBGad 2
type Zq = ZqBasic 64 Int64
type Zp = ZqBasic 2  Int64
type Rq = Cyc CT F32 Zq
type Rp = Cyc CT F32 Zp

main :: IO ()
main = TF.defaultMain =<< (sequence
  [hideArgs
    "key homomorphism"
    (prop_keyHomom 10)
    (Proxy::Proxy '(CT, F32, Zp, Zq, Gad))] :: IO [TF.Test])
