{-|
Module      : Crypto.Lol.Tests.CycTests
Description : Tests for the 'Cyc' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for the 'Cyc' interface.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.CycTests (cycTests1, cycTests2) where

import Control.Applicative
import Control.Monad       (join, liftM2)

import Crypto.Lol

import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.Tests    (Gen, Test, testGroup, testWithGen,
                                  testWithoutGen)

-- | Tests for single-index 'Cyc' operations. There must be a CRT basis for \(O_m\) over @r@.
cycTests1 :: forall t m r . (Fact m, _) => Gen (Cyc t m r) -> Test
cycTests1 cycGen =
  let ptmr = Proxy :: Proxy '(t,m,r) in
  testGroup (showType ptmr) $ ($ cycGen) <$> [
   testWithGen "mulGPow" prop_mulgPow,
   testWithGen "mulGDec" prop_mulgDec,
   testWithGen "mulGCRT" prop_mulgCRT]

-- | Tests for inter-ring 'Cyc' operations. There must be a CRT basis for \(O_{m'}\) over @r@.
cycTests2 :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Gen (Cyc t m' r) -> Test
cycTests2 _ cycGen =
  let ptmmr = Proxy::Proxy '(t,m,m',r)
  in testGroup (showType ptmmr) [
      testWithoutGen "crtSet" (prop_crtSet_pairs ptmmr),
      testWithGen "coeffsPow" (prop_coeffsPow ptmmr) cycGen]

prop_mulgPow :: _ => Cyc t m r -> Bool
prop_mulgPow x =
  let y = advisePow x
  in y == fromJust' "prop_mulgPow failed divisibility!" (divG $ mulG y)

prop_mulgDec :: _ => Cyc t m r -> Bool
prop_mulgDec x =
  let y = adviseDec x in
  y == fromJust' "prop_mulgDec failed divisibility!" (divG $ mulG y)

prop_mulgCRT :: _ => Cyc t m r -> Bool
prop_mulgCRT x =
  let y = adviseCRT x in
  y == fromJust' "prop_mulgCRT failed divisibility!" (divG $ mulG y)

prop_coeffsPow :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Cyc t m' r -> Bool
prop_coeffsPow _ x =
  let xs = map embed (coeffsPow x :: [Cyc t m r])
      bs = proxy powBasis (Proxy::Proxy m)
  in sum (zipWith (*) xs bs) == x

-- verifies that CRT set elements satisfy c_i * c_j = delta_ij * c_i
-- necessary (but not sufficient) condition
prop_crtSet_pairs :: forall (t :: Factored -> * -> *) m (m' :: Factored) (r :: *) . _
                  => Proxy '(t,m,m',r) -> Bool
prop_crtSet_pairs _ =
  let crtset = proxy crtSet (Proxy::Proxy m) :: [Cyc t m' r]
      pairs = join (liftM2 (,)) crtset
  in all (\(a,b) -> if a == b then a*b == a else a*b == zero) pairs
