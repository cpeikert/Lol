{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude, PolyKinds,
             ScopedTypeVariables, TypeOperators, TypeFamilies #-}
module CycTests (cycTests) where

import Control.Monad (liftM2,join)

import Crypto.Lol
import Crypto.Lol.Types.ZPP

import Utils
import Apply.Cyc
import Tests

import qualified Test.Framework as TF

cycTests :: [TF.Test]
cycTests = [
  testGroupM "mulGPow"   $ applyBasic allParams    $ hideArgs prop_mulgPow,
  testGroupM "mulGDec"   $ applyBasic allParams    $ hideArgs prop_mulgDec,
  testGroupM "mulGCRT"   $ applyBasic allParams    $ hideArgs prop_mulgCRT,
  testGroupM "crtSet"    $ applyBasis basisParams  $ hideArgs prop_crtSet_pairs,
  testGroupM "coeffsPow" $ applyTwoIdx basisParams $ hideArgs prop_coeffsBasis
  ]

prop_mulgPow :: (CElt t r, Fact m, Eq r, IntegralDomain r) => Cyc t m r -> Test '(t,m,r)
prop_mulgPow x =
  let y = advisePow x
  in test $ y == (fromJust' "prop_mulgPow failed divisibility!" $ divG $ mulG y)

prop_mulgDec :: (CElt t r, Fact m, Eq r, IntegralDomain r) => Cyc t m r -> Test '(t,m,r)
prop_mulgDec x =
  let y = adviseDec x
  in test $ y == (fromJust' "prop_mulgDec failed divisibility!" $ divG $ mulG y)

prop_mulgCRT :: (CElt t r, Fact m, Eq r, IntegralDomain r) => Cyc t m r -> Test '(t,m,r)
prop_mulgCRT x =
  let y = adviseCRT x
  in test $ y == (fromJust' "prop_mulgCRT failed divisibility!" $ divG $ mulG y)

prop_coeffsBasis :: forall t m m' r . (m `Divides` m', CElt t r, Eq r)
  => Cyc t m' r -> Test '(t,m,m',r)
prop_coeffsBasis x =
  let xs = map embed (coeffsCyc Pow x :: [Cyc t m r])
      bs = proxy powBasis (Proxy::Proxy m)
  in test $ (sum $ zipWith (*) xs bs) == x

-- verifies that CRT set elements satisfy c_i * c_j = delta_ij * c_i
-- necessary (but not sufficient) condition
prop_crtSet_pairs :: forall t m m' r . (m `Divides` m', ZPP r, Eq r, CElt t r, CElt t (ZpOf r))
  => Test '(t,m,m',r)
prop_crtSet_pairs =
  let crtset = proxy crtSet (Proxy::Proxy m) :: [Cyc t m' r]
      pairs = join (liftM2 (,)) crtset
  in test $ and $ map (\(a,b) -> if a == b then a*b == a else a*b == zero) pairs

type Tensors = '[CT,RT]
type MRCombos =
  '[ '(F7, Zq 29),
     '(F7, Zq 32),
     '(F12, Zq 2148249601),
     '(F1, Zq 17),
     '(F2, Zq 17),
     '(F4, Zq 17),
     '(F8, Zq 17),
     '(F21, Zq 8191),
     '(F42, Zq 8191),
     '(F42, Zq 18869761),
     '(F42, Zq 1024),
     '(F42, Zq (18869761 ** 19393921)),
     '(F89, Zq 179)
    ]

type MM'RCombos = '[
  '(F1, F7, Zq PP8),
  '(F1, F7, Zq PP2)
  ]

type AllParams = ( '(,) <$> Tensors) <*> MRCombos
allParams :: Proxy AllParams
allParams = Proxy

type BasisParams = ( '(,) <$> Tensors) <*> MM'RCombos
basisParams :: Proxy BasisParams
basisParams = Proxy
