{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Crypto.Lol.Tests
(test
,testIO
,TF.testGroup
,nestGroup
,testGroupM
,hideArgs
,Test(..)) where

import Crypto.Lol.Utils.GenArgs
import Control.Monad.Random

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2
import Test.Framework (testGroup)

test :: Bool -> Test params
test = Test

nestGroup :: String -> [proxy (a :: k) -> IO TF.Test] -> proxy a -> IO TF.Test
nestGroup s ts p = testGroup s <$> mapM ($ p) ts

testIO :: (forall m . MonadRandom m => m Bool) -> Test params
testIO = TestM

-- testGroup :: TestName -> [Test] -> Test
-- from Test.Framework in test-framework

testGroupM :: String -> [IO TF.Test] -> TF.Test
testGroupM str = TF.buildTest . (TF.testGroup str <$>) . sequence

-- normalizes any function resulting in a Benchmark to
-- one that takes a proxy for its arguments
hideArgs :: (GenArgs rnd bnch, MonadRandom rnd, ResultOf bnch ~ Test a)
  => String -> bnch -> proxy a -> rnd TF.Test
hideArgs s f _ = do
  res <- genArgs f
  case res of
    Test b -> return $ testProperty s b
    TestM b -> testProperty s <$> b

data Test params where
  Test :: Bool -> Test params
  TestM :: (forall m . MonadRandom m => m Bool) -> Test params

instance (MonadRandom rnd) => GenArgs rnd (Test params) where
  genArgs = return
