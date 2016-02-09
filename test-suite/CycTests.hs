{-# LANGUAGE RankNTypes, ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax,
             TypeOperators, FlexibleContexts, ConstraintKinds, TypeFamilies, PolyKinds,
             DataKinds #-}
module CycTests (cycTests) where

import TestTypes hiding (Zq)

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.IrreducibleChar2
import Crypto.Lol.Types.ZPP

import Control.Applicative
import Control.Monad (join, liftM2)

import Data.Array.Repa.Eval (Elt)
import Data.Vector.Unboxed (Vector, Unbox)
import Data.Vector.Storable (Storable)

import Test.Framework (testGroup, Test, defaultMain, buildTest)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, property, Arbitrary)

import Utils
import Harness.Cyc

cycTests = [buildTest $ testGroupRnd "coeffsPow" $ benchTwoIdx (Proxy::Proxy '[]) $ wrap' prop_coeffsBasis,
            buildTest $ testGroupRnd "mulGPow" $ benchBasic (Proxy::Proxy AllParams) $ wrap' prop_mulgPow,
            buildTest $ testGroupRnd "mulGDec" $ benchBasic (Proxy::Proxy AllParams) $ wrap' prop_mulgDec,
            buildTest $ testGroupRnd "mulGCRT" $ benchBasic (Proxy::Proxy AllParams) $ wrap' prop_mulgCRT,
            buildTest $ testGroupRnd "crtSet"  $ benchBasis (Proxy::Proxy BasisParams) $ wrap' prop_crtSet_pairs
            ]


prop_mulgPow :: (CElt t r, Fact m) => Cyc t m r -> TestBool '(t,m,r)
prop_mulgPow x =
  let y = advisePow x
  in test $ y == (fromJust' "prop_mulgPow failed divisibility!" $ divG $ mulG y)

prop_mulgDec :: (CElt t r, Fact m) => Cyc t m r -> TestBool '(t,m,r)
prop_mulgDec x = 
  let y = adviseDec x
  in test $ y == (fromJust' "prop_mulgDec failed divisibility!" $ divG $ mulG y)

prop_mulgCRT :: (CElt t r, Fact m) => Cyc t m r -> TestBool '(t,m,r)
prop_mulgCRT x = 
  let y = adviseCRT x
  in test $ y == (fromJust' "prop_mulgCRT failed divisibility!" $ divG $ mulG y)

prop_coeffsBasis :: forall t m m' r . (m `Divides` m', CElt t r)
  => Cyc t m' r -> TestBool '(t,m,m',r)
prop_coeffsBasis x = 
  let xs = map embed (coeffsCyc Pow x :: [Cyc t m r])
      bs = proxy powBasis (Proxy::Proxy m)
  in test $ (sum $ zipWith (*) xs bs) == x

-- verifies that CRT set elements satisfy c_i * c_j = delta_ij * c_i
-- necessary (not sufficient?) condition
prop_crtSet_pairs :: forall t m m' r . (m `Divides` m', ZPP r, CElt t r, CElt t (ZpOf r))
  => TestBool '(t,m,m',r)
prop_crtSet_pairs = 
  let crtset = proxy crtSet (Proxy::Proxy m) :: [Cyc t m' r]
      pairs = join (liftM2 (,)) crtset
  in test $ and $ map (\(a,b) -> if a == b then a*b == a else a*b == zero) pairs

type Tensors = '[CT,RT]
type MRCombos = 
  '[ '(F7, Zq 29),
     '(F7, Zq 32),
     '(F12, SmoothZQ1),
     '(F1, Zq 17),
     '(F2, Zq 17),
     '(F4, Zq 17),
     '(F8, Zq 17),
     '(F21, Zq 8191),
     '(F42, Zq 8191),
     '(F42, ZQ1),
     '(F42, Zq 1024),
     '(F42, ZQ2),
     '(F89, Zq 179)
    ]
-- EAC: must be careful where we use Nub: apparently TypeRepStar doesn't work well with the Tensor constructors
type AllParams = ( '(,) <$> Tensors) <*> MRCombos



type MM'RCombos = '[
  '(F1, F7, Zq PP8),
  '(F1, F7, Zq PP2)
  ]
type BasisParams = ( '(,) <$> Tensors) <*> MM'RCombos

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
