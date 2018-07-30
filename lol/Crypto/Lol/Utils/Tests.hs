{-|
Module      : Crypto.Lol.Utils.Tests
Description : Infrastructure for testing Lol.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Infrastructure for testing Lol.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Utils.Tests
(nestGroup
,testIOWithGen
,testIOWithoutGen
,testWithGen
,testWithoutGen
,ApproxEqual(..)
-- Re-exports
,QCG.choose
,QCG.chooseAny
,TF.testGroup
,QC.Gen
,TF.Test) where

import Crypto.Lol.Prelude              (Complex, Int64, imag, real)
import Crypto.Lol.Types.Unsafe.ZqBasic

import qualified Test.Framework                       as TF
import qualified Test.Framework.Providers.QuickCheck2 as QC2
import qualified Test.QuickCheck                      as QC
import qualified Test.QuickCheck.Gen                  as QCG
import qualified Test.QuickCheck.Monadic              as QCM

-- The allowable distance from 1 the ratio of two Doubles is allowed to be in order for the numbers
-- to be considered "approximately equal". That is, x =~= y iff -1-ε < x/y < 1+ε
-- In the case that x == 0, we just measure that abs y < ε, and vice-versa for y == 0
approxEqualEpsilon :: Double
approxEqualEpsilon = 1e-6

class ApproxEqual a where
  (=~=) :: a -> a -> Bool

instance ApproxEqual Int64 where
  x =~= y = x == y

instance Eq r => ApproxEqual (ZqBasic q r) where
  x =~= y = x == y

instance ApproxEqual Double where
  x =~= y
    | x == 0             = abs y < approxEqualEpsilon
    | y == 0             = abs x < approxEqualEpsilon
    | isNaN x || isNaN y = False
    -- Calculate the distance of x/y and y/x from 1 and take the minimum
    | otherwise          = let minDist = minimum $ map (\r -> abs $ abs r - 1) [x/y, y/x] in
                               minDist < approxEqualEpsilon

instance ApproxEqual (Complex Double) where
  x =~= y = (real x =~= real y) && (imag x =~= imag y)

-- | Make a 'Test' given a name, a testing function, and a parameter generator
testWithGen :: (Show a, QC.Testable prop) => String -> (a -> prop) -> QC.Gen a -> TF.Test
testWithGen name f gen = QC2.testProperty name $ QC.forAll gen f

-- | Make a 'Test' given a name, a monadic (IO only) testing function, and a parameter generator
testIOWithGen :: (Show a, QC.Testable prop) => String -> (a -> IO prop) -> QC.Gen a -> TF.Test
testIOWithGen name f gen = QC2.testProperty name $ QCM.monadicIO $ QCM.forAllM gen (QCM.run . f)

-- | Make a 'Test' given a name and a 'QC.Testable' value
testWithoutGen :: (QC.Testable prop) => String -> prop -> TF.Test
testWithoutGen name p = QC2.testProperty name $ QC.property p

-- | Make a 'Test' given a name and a monadic (IO only) 'QC.Testable' value
testIOWithoutGen :: (QC.Testable prop) => String -> IO prop -> TF.Test
testIOWithoutGen name p = QC2.testProperty name $ QCM.monadicIO $ QCM.run p

-- | Apply parameters to a list of 'Test'.
nestGroup :: String -> [QC.Gen a -> TF.Test] -> QC.Gen a -> TF.Test
nestGroup name ts gen = TF.testGroup name $ map ($ gen) ts
