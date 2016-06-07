{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses,
             PolyKinds, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Tests
(test
,testIO
,TF.testGroup
,testGroupM
,hideArgs
,Test(..)) where

import GenArgs
import Utils

import Control.Monad.Random

import Data.Proxy

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2

test :: Bool -> Test params
test = Test

testIO :: (forall m . MonadRandom m => m Bool) -> Test params
testIO = TestM

testGroupM :: String -> [IO TF.Test] -> TF.Test
testGroupM str = TF.buildTest . (TF.testGroup str <$>) . sequence

-- normalizes any function resulting in a Benchmark to
-- one that takes a proxy for its arguments
hideArgs :: (GenArgs rnd bnch, MonadRandom rnd, ShowType a,
             ResultOf bnch ~ Test a)
  => bnch -> Proxy a -> rnd TF.Test
hideArgs f p = do
  res <- genArgs f
  case res of
    Test b -> return $ testProperty (showType p) b
    TestM b -> testProperty (showType p) <$> b

data Test params where
  Test :: Bool -> Test params
  TestM :: (forall m . MonadRandom m => m Bool) -> Test params

instance (MonadRandom rnd) => GenArgs rnd (Test params) where
  type ResultOf (Test params) = Test params
  genArgs = return
