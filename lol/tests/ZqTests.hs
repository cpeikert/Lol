{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs, MultiParamTypeClasses, NoImplicitPrelude, 
             RebindableSyntax, ScopedTypeVariables, PolyKinds, RankNTypes, TypeFamilies, TypeOperators #-}

module ZqTests (zqTests) where

import Tests
import Utils
import Gen
import Apply

import Crypto.Lol
import Crypto.Lol.CRTrans

import Control.Monad.Random

data BasicCtxD
type BasicCtx r =  (Field r, Eq r, Random r, ShowType r, CRTEmbed r)
instance (params `Satisfy` BasicCtxD, BasicCtx r) 
  => ( r ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx r) => Proxy r -> ArgsCtx BasicCtxD
  run _ f = (f $ BC (Proxy::Proxy r)) : (run (Proxy::Proxy params) f)

applyBasic :: (params `Satisfy` BasicCtxD, MonadRandom rnd) =>
  Proxy params 
  -> (forall r . (BasicCtx r, Generatable rnd r) 
       => Proxy r -> rnd res)
  -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p

prop_add :: forall r . (Ring r, Eq r) => Proxy r -> Int -> Int -> Test r
prop_add _ x y = test $ (fromIntegral $ x + y) == ((fromIntegral x) + (fromIntegral y :: r))

prop_mul :: forall r . (Ring r, Eq r) => Proxy r -> Int -> Int -> Test r
prop_mul _ x y = test $ (fromIntegral $ x * y) == ((fromIntegral x) * (fromIntegral y :: r))

prop_recip :: forall r . (Field r, Eq r) => r -> Test r
prop_recip x = test $ (x == 0) || (one == (x * recip x))

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: forall r . (CRTEmbed r, Ring r, Eq r)
  => r -> r -> Test r
prop_mul_ext x y = test $
  let z = x * y
      z' = fromExt $ (toExt x) * (toExt y)
  in z == z'

type ZqTypes = [
  Zq 3,
  Zq (3 ** 5),
  Zq (3 ** 5 ** 7)]

zqTests = [
  testGroupM "(+)" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_add,
  testGroupM "(*)" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_mul,
  testGroupM "^-1" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_recip,
  testGroupM "extension ring (*)" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_mul_ext
  ]