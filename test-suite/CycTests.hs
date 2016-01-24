{-# LANGUAGE RankNTypes, ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax,
             TypeOperators, FlexibleContexts, ConstraintKinds, TypeFamilies,
             DataKinds #-}
module CycTests (cycTests) where

import TestTypes

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.IrreducibleChar2
import Crypto.Lol.Types.ZPP

import Control.Monad (join, liftM2)

import Data.Array.Repa.Eval (Elt)
import Data.Vector.Unboxed (Vector, Unbox)
import Data.Vector.Storable (Storable)

import Test.Framework (testGroup, Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, property, Arbitrary)

cycTests = [testGroup "coeffsPow" $ groupC $ wrapCmm'rToBool prop_coeffsBasis,
            testGroup "mulGPow" $ groupC' $ wrapCmrToBool prop_mulgPow,
            testGroup "mulGDec" $ groupC' $ wrapCmrToBool prop_mulgDec,
            testGroup "mulGCRT" $ groupC' $ wrapCmrToBool prop_mulgCRT,
            testGroup "crtSet" $ groupC $ wrapProxyCmm'rToBool prop_crtSet_pairs]


prop_mulgPow :: (CElt t r, Fact m) => Cyc t m r -> Bool
prop_mulgPow x =
  let y = advisePow x
  in y == (fromJust' "prop_mulgPow failed divisibility!" $ divG $ mulG y)

prop_mulgDec :: (CElt t r, Fact m) => Cyc t m r -> Bool
prop_mulgDec x = 
  let y = adviseDec x
  in y == (fromJust' "prop_mulgDec failed divisibility!" $ divG $ mulG y)

prop_mulgCRT :: (CElt t r, Fact m) => Cyc t m r -> Bool
prop_mulgCRT x = 
  let y = adviseCRT x
  in y == (fromJust' "prop_mulgCRT failed divisibility!" $ divG $ mulG y)

wrapCmrToBool :: (CElt t r, Fact m, Show (Cyc t m r), Arbitrary (t m r)) 
  => (Cyc t m r -> Bool) -> Proxy (Cyc t) -> Proxy '(m,r) -> Property
wrapCmrToBool f _ _ = property f

groupC' ::
  (forall t m r . 
       (CElt t r, Fact m, Show (Cyc t m r), Arbitrary (t m r))
       => Proxy (Cyc t) 
          -> Proxy '(m,r) 
          -> Property)
  -> [Test]
groupC' f =
  [testGroup "FC CT" $ groupMR (f (Proxy::Proxy (Cyc CT))),
   testGroup "FC RT" $ groupMR (f (Proxy::Proxy (Cyc RT)))]

groupMR :: (forall m r . (Fact m, 
                          CElt CT r, Show (Cyc CT m r), Arbitrary (CT m r),
                          CElt RT r, Show (Cyc RT m r), Arbitrary (RT m r)) 
               => Proxy '(m, r) -> Property) 
            -> [Test]
groupMR f = [testProperty "F7/29" $ f (Proxy::Proxy '(F7, Zq 29)),
             testProperty "F7/32" $ f (Proxy::Proxy '(F7, Zq 32)),
             testProperty "F12/SmoothZQ1" $ f (Proxy::Proxy '(F12, SmoothZQ1)),
             testProperty "F1/17" $ f (Proxy::Proxy '(F1, Zq 17)),
             testProperty "F2/17" $ f (Proxy::Proxy '(F2, Zq 17)),
             testProperty "F4/17" $ f (Proxy::Proxy '(F4, Zq 17)),
             testProperty "F8/17" $ f (Proxy::Proxy '(F8, Zq 17)),
             testProperty "F21/8191" $ f (Proxy::Proxy '(F21, Zq 8191)),
             testProperty "F42/8191" $ f (Proxy::Proxy '(F42, Zq 8191)),
             testProperty "F42/ZQ1" $ f (Proxy::Proxy '(F42, ZQ1)),
             testProperty "F42/1024" $ f (Proxy::Proxy '(F42, Zq 1024)),
             testProperty "F42/ZQ2" $ f (Proxy::Proxy '(F42, ZQ2)),
             testProperty "F89/179" $ f (Proxy::Proxy '(F89, Zq 179))]



type BasisCtx t m m' r = 
  (m `Divides` m', ZPP r, CElt t r, CElt t (ZpOf r))

prop_coeffsBasis :: forall t m m' r . (BasisCtx t m m' r)
  => Proxy m -> Cyc t m' r -> Bool
prop_coeffsBasis _ x = 
  let xs = map embed (coeffsCyc Pow x :: [Cyc t m r])
      bs = proxy powBasis (Proxy::Proxy m)
  in (sum $ zipWith (*) xs bs) == x

-- verifies that CRT set elements satisfy c_i * c_j = delta_ij * c_i
-- necessary (not sufficient?) condition
prop_crtSet_pairs :: forall t m m' r . (BasisCtx t m m' r)
  => Proxy m -> Proxy (Cyc t m' r) -> Bool
prop_crtSet_pairs pm _ = 
  let crtset = proxy crtSet pm :: [Cyc t m' r]
      pairs = join (liftM2 (,)) crtset
  in and $ map (\(a,b) -> if a == b then a*b == a else a*b == zero) pairs

type BasisWrapCtx t m m' r =
  (BasisCtx t m m' r, Show (Cyc t m' r), Arbitrary (t m' r))

wrapCmm'rToBool :: (BasisWrapCtx t m m' r)
  => (Proxy m -> Cyc t m' r -> Bool) 
     -> Proxy (Cyc t) -> Proxy '(m,m',r) -> Property
wrapCmm'rToBool f _ _ = property $ f Proxy

wrapProxyCmm'rToBool :: (BasisWrapCtx t m m' r)
  => (Proxy m -> Proxy (Cyc t m' r) -> Bool) 
     -> Proxy (Cyc t) -> Proxy '(m,m',r) -> Property
wrapProxyCmm'rToBool f _ _ = property $ f Proxy Proxy

groupC ::
  (forall t m m' r . 
       (BasisWrapCtx t m m' r) 
       => Proxy (Cyc t) 
          -> Proxy '(m,m',r) 
          -> Property)
  -> [Test]
-- since we don't have any Tensor-level tests for coeffs/basis functions,
-- we need to test all Tensors here.
groupC f =
  [testGroup "FC CT" $ groupMM'R (f (Proxy::Proxy (Cyc CT))),
   testGroup "FC RT" $ groupMM'R (f (Proxy::Proxy (Cyc RT)))]

type BasisWrapCCtx m m' r =
  (BasisWrapCtx RT m m' r,
   BasisWrapCtx CT m m' r)

groupMM'R :: 
  (forall m m' r . (BasisWrapCCtx m m' r) => Proxy '(m, m', r) -> Property) 
  -> [Test]
groupMM'R f = [testProperty "F1/F7/PP8" $ f (Proxy::Proxy '(F1, F7, ZP8)), 
               testProperty "F1/F7/PP2" $ f (Proxy::Proxy '(F1, F7, ZP2))]



-- for crtSet, take all pairwise products 
-- if elts are equal, id
-- if not, zero

-- also do a cardinality check

-- checks cardinality of the CRT set
{-
prop_crtSet_card pm _
  let inferLen = length $ (proxy crtSetDec pm :: [t m' r])
      expectLen = 
  in  
-}
