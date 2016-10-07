{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Infrastructure for testing Lol.

module Crypto.Lol.Tests
(test
,testIO
,TF.testGroup
,nestGroup
,testGroupM
,genTestArgs
,Test(..)) where

import Crypto.Lol.Utils.GenArgs
import Control.Monad.Random

import qualified Test.Framework as TF
import Test.Framework.Providers.QuickCheck2

-- | Test a 'Bool' value.
test :: Bool -> Test params
test = Test

-- | Test a monadic 'Bool' value.
testIO :: (forall m . MonadRandom m => m Bool) -> Test params
testIO = TestM

-- | Apply parameters to a list of 'TF.Test'
nestGroup :: String -> [proxy (a :: k) -> IO TF.Test] -> proxy a -> IO TF.Test
nestGroup s ts p = TF.testGroup s <$> mapM ($ p) ts

-- | Wrapper around QuickCheck's 'TF.testGroup'
testGroupM :: String -> [IO TF.Test] -> TF.Test
testGroupM str = TF.buildTest . (TF.testGroup str <$>) . sequence

-- | Converts a function mapping zero or more arguments to a 'Test' @a@
-- by generating random inputs to the function
genTestArgs :: (GenArgs rnd bnch, MonadRandom rnd, ResultOf bnch ~ Test a)
  => String -> bnch -> proxy a -> rnd TF.Test
genTestArgs s f _ = do
  res <- genArgs f
  case res of
    Test b -> return $ testProperty s b
    TestM b -> testProperty s <$> b

-- | Wrapper for simple 'Testable' property, with phantom parameters.
data Test params where
  Test :: Bool -> Test params
  TestM :: (forall m . MonadRandom m => m Bool) -> Test params

instance (MonadRandom rnd) => GenArgs rnd (Test params) where
  genArgs = return
