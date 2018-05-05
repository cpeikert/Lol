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
(test
,testIO
,testWithGen
,nestGroup
,testGroupM
,genTestArgs
,Test(..)) where

import Crypto.Lol.Utils.GenArgs

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Test.QuickCheck as QC

-- TODO: Kill all the old test framework stuff (or is it still used for benchmarks?)

-- | Test a 'Bool' value.
test :: Bool -> Test params
test = Test

-- | Test a monadic 'Bool' value.
testIO :: IO Bool -> Test params
testIO = TestIO

-- Make a 'TF.Test' given a name, a testing function, and a parameter generator
testWithGen :: (Show a, QC.Testable prop) => String -> (a -> prop) -> QC.Gen a -> TF.Test
testWithGen name f gen = testProperty name $ QC.forAll gen f

-- | Apply parameters to a list of 'TF.Test'.
nestGroup :: String -> [QC.Gen a -> TF.Test] -> QC.Gen a -> TF.Test
nestGroup name ts gen = TF.testGroup name $ map ($ gen) ts

-- | Wrapper around QuickCheck's 'TF.testGroup'.
testGroupM :: String -> [IO TF.Test] -> TF.Test
testGroupM str = TF.buildTest . (TF.testGroup str <$>) . sequence

-- | Converts a function mapping zero or more arguments to a 'TF.Test' @a@
-- by generating random inputs to the function.
genTestArgs :: (GenArgs bnch, ResultOf bnch ~ Test a)
  => String -> bnch -> proxy a -> TF.Test
genTestArgs s f _ = testProperty s $ QC.ioProperty $ do
  res <- genArgs f
  case res of
    Test b -> return b
    TestIO b -> b

-- | Wrapper for simple 'Testable' property, with phantom parameters.
data Test params where
  Test :: Bool -> Test params
  TestIO :: IO Bool -> Test params

instance (ResultOf (Test params) ~ Test params)
  => GenArgs (Test params) where
  genArgs = return
