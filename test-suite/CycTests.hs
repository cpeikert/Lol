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
            testGroup "crtSet" $ groupC $ wrapProxyCmm'rToBool prop_crtSet_pairs]





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
               testProperty "F1/F7/PP2" $ f (Proxy::Proxy '(F1, F7, ZP2))] -- add some more test cases




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
