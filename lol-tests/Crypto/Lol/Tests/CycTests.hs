{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

-- | Tests for the 'Cyc' interface.

module Crypto.Lol.Tests.CycTests where

import Control.Applicative
import Control.Monad (liftM2,join,replicateM)

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor (TElt)
import Crypto.Lol.Types.ZPP

import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Tests

import Control.Monad.Random
import qualified Test.Framework as TF

-- | Tests for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
cycTests1 :: forall t m r . _ => Proxy '(m,r) -> Proxy t -> TF.Test
cycTests1 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,r)
  in testGroupM (showType ptmr) $ ($ ptmr) <$> [
      genTestArgs "mulGPow" prop_mulgPow,
      genTestArgs "mulGDec" prop_mulgDec,
      genTestArgs "mulGCRT" prop_mulgCRT
      ]

-- | Tests for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
cycTests2 :: forall t m m' r . _ => Proxy '(m,m',r) -> Proxy t -> TF.Test
cycTests2 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,m',r)
  in testGroupM (showType ptmr) $ ($ ptmr) <$> [
      genTestArgs "crtSet" prop_crtSet_pairs,
      genTestArgs "coeffsPow" prop_coeffsPow,
      genTestArgs "coeffsCRTSet" prop_coeffsCRTSet,
      genTestArgs "crtSetDual" prop_CRTSetDual
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

-- verifies that powBasis^T * coeffsPow(x) = x
prop_coeffsPow :: forall t m m' r . (m `Divides` m', CElt t r, Eq r)
  => Cyc t m' r -> Test '(t,m,m',r)
prop_coeffsPow x =
  let xs = map embed (coeffsPow x :: [Cyc t m r])
      bs = proxy powBasis (Proxy::Proxy m)
  in test $ (sum $ zipWith (*) xs bs) == x

-- verifies that crtSet^T * coeffsCRTSet(x) = x for any x that is
-- a linear combination of the crtSet
prop_coeffsCRTSet :: forall t m m' r . (m `Divides` m', CElt t r, ZPP r, Eq r, IntegralDomain r, Random r, TElt t (ZpOf r))
  => Test '(t,m,m',r)
prop_coeffsCRTSet = testIO $ do
  let cset = proxy crtSet (Proxy::Proxy m)
      len = length cset
  coeffs :: [Cyc t m r] <- replicateM len getRandom
  let coeffs' = map embed coeffs :: [Cyc t m' r]
      x = sum $ zipWith (*) cset coeffs'
  return $ coeffs == coeffsCRTSet x

prop_CRTSetDual :: forall t m m' r . (m `Divides` m', ZPP r, CElt t r, TElt t (ZpOf r),
               IntegralDomain r, Eq r)
  => Test '(t,m,m',r)
prop_CRTSetDual =
  let cset = proxy crtSet (Proxy::Proxy m) :: [Cyc t m' r]
      csetd = proxy crtSetDual (Proxy::Proxy m)
      idxs = [0..(length cset-1)]
      delta i j =
        let x = twace $ (cset !! i) * (csetd !! j) :: Cyc t m r
        in if i == j
           then x == one
           else x == zero
      pairs = [(i,j) | i <- idxs, j <- idxs, i <= j]
  in test $ and $ map (uncurry delta) pairs

-- verifies that CRT set elements satisfy c_i * c_j = delta_ij * c_i
-- necessary (but not sufficient) condition
prop_crtSet_pairs :: forall t m m' r . (m `Divides` m', ZPP r, Eq r, CElt t r, CElt t (ZpOf r))
  => Test '(t,m,m',r)
prop_crtSet_pairs =
  let crtset = proxy crtSet (Proxy::Proxy m) :: [Cyc t m' r]
      pairs = join (liftM2 (,)) crtset
  in test $ and $ map (\(a,b) -> if a == b then a*b == a else a*b == zero) pairs
