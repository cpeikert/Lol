{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.CycTests (cycTests1, cycTests2) where

import Control.Applicative
import Control.Monad (liftM2,join)

import Crypto.Lol
import Crypto.Lol.Types.ZPP

import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Tests

import qualified Test.Framework as TF

cycTests1 :: forall t m r . _ => Proxy '(m,r) -> Proxy t -> TF.Test
cycTests1 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,r)
  in testGroupM (showType ptmr) $ ($ ptmr) <$> [
      hideArgs "mulGPow" prop_mulgPow,
      hideArgs "mulGDec" prop_mulgDec,
      hideArgs "mulGCRT" prop_mulgCRT
      ]

cycTests2 :: forall t m m' r . _ => Proxy '(m,m',r) -> Proxy t -> TF.Test
cycTests2 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,m',r)
  in testGroupM (showType ptmr) $ ($ ptmr) <$> [
      hideArgs "crtSet" prop_crtSet_pairs,
      hideArgs "coeffsPow" prop_coeffsBasis
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
