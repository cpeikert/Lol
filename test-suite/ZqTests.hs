{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, DataKinds, TypeOperators, PolyKinds, FlexibleContexts, RankNTypes, KindSignatures, MultiParamTypeClasses #-}

module ZqTests (zqTests) where

import Crypto.Lol.Types.ZqBasic
import Crypto.Lol.LatticePrelude hiding (Nat)
import Crypto.Lol.Reflects

import Control.Monad

import GHC.TypeLits

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck


prop_add :: forall (q :: Nat) . (Reflects q Int, KnownNat q) => Proxy q -> Int -> Int -> Bool
prop_add _ x y = (fromIntegral $ x + y) == ((fromIntegral x) + (fromIntegral y :: ZqBasic q Int))

prop_mul :: forall (q :: Nat) . (Reflects q Int, KnownNat q) => Proxy q -> Int -> Int -> Bool
prop_mul _ x y = (fromIntegral $ x * y) == ((fromIntegral x) * (fromIntegral y :: ZqBasic q Int))

prop_recip :: forall (q :: Nat) . (Reflects q Int, KnownNat q) => Proxy q -> Int -> Bool
prop_recip _ x = let qval = proxy value (Proxy::Proxy q)
                     y = fromIntegral x :: ZqBasic q Int
                 in if (x `mod` qval) == 0
                    then True
                    else (fromIntegral (1::Int)) == (y * (recip y))

type ZqModuli = '[7, 13, 17, 11, 13, 29]

class CallZqProp xs where
  callProp :: Proxy xs -> Gen Int -> (forall (q :: Nat) . (Reflects q Int, KnownNat q) => Proxy q -> Int -> Int -> Bool) -> [Test]

  callProp2 :: Proxy xs 
                -> Gen Int 
                -> (forall (q :: Nat) . (Reflects q Int, KnownNat q) => Proxy q -> Int -> Bool)
                -> [Test]

instance CallZqProp '[] where
  callProp _ _ _ = []
  callProp2 _ _ _ = []

instance (CallZqProp qs, KnownNat q) => CallZqProp (q ': qs) where
  callProp _ gen f = (testProperty ("q = " ++ (show $ (proxy value (Proxy::Proxy q) :: Int))) $ property $ liftM2 (f (Proxy::Proxy q)) gen gen) : (callProp (Proxy::Proxy qs) gen f)
  callProp2 _ gen f = (testProperty ("q = " ++ (show $ (proxy value (Proxy::Proxy q) :: Int))) $ property $ liftM (f (Proxy::Proxy q)) gen) : (callProp2 (Proxy::Proxy qs) gen f)

zqModuli :: Proxy ZqModuli
zqModuli = Proxy

zqTests :: [Test]
zqTests = 
  [testGroup "ZqBasic +" $ callProp zqModuli (choose (-100,100)) prop_add,
   testGroup "ZqBasic *" $ callProp zqModuli (choose (-100,100)) prop_mul,
   testGroup "ZqBasic recip" $ callProp2 zqModuli (choose (-100,100)) prop_recip]
