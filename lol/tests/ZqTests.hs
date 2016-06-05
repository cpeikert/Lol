{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, MultiParamTypeClasses, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables, PolyKinds, RankNTypes, TypeFamilies, TypeOperators, UndecidableInstances #-}

module ZqTests (zqTests) where

import Tests
import Utils
import Gen
import Apply

import Crypto.Lol
import Crypto.Lol.CRTrans

import Control.Applicative
import Control.Monad.Random

data BasicCtxD
type BasicCtx r =
  (Field r, Eq r, Random r, Random (ModRep r), ToInteger (ModRep r),
   PID (ModRep r), ShowType r, CRTEmbed r, Mod r)
instance (params `Satisfy` BasicCtxD, BasicCtx r)
  => ( r ': params) `Satisfy` BasicCtxD where
  data ArgsCtx BasicCtxD where
    BC :: (BasicCtx r) => Proxy r -> ArgsCtx BasicCtxD
  run _ f = (f $ BC (Proxy::Proxy r)) : (run (Proxy::Proxy params) f)

applyBasic :: (params `Satisfy` BasicCtxD, MonadRandom rnd) =>
  Proxy params
  -> (forall r . (BasicCtx r, Generatable rnd r, Generatable rnd (Invertible r))
       => Proxy r -> rnd res)
  -> [rnd res]
applyBasic params g = run params $ \(BC p) -> g p

prop_add :: forall r . (Ring r, Eq r) => LiftedMod r -> LiftedMod r -> Test r
prop_add (LMod x) (LMod y) = test $ (fromIntegral $ x + y) == ((fromIntegral x) + (fromIntegral y :: r))

prop_mul :: forall r . (Ring r, Eq r) => LiftedInvertible r -> LiftedInvertible r -> Test r
prop_mul (LInv x) (LInv y) = test $ (fromIntegral $ x * y) == ((fromIntegral x) * (fromIntegral y :: r))

prop_recip :: forall r . (Field r, Eq r) => Invertible r -> Test r
prop_recip (Invertible x) = test $ one == (x * recip x)

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: forall r . (CRTEmbed r, Ring r, Eq r)
  => Invertible r -> Invertible r -> Test r
prop_mul_ext (Invertible x) (Invertible y) = test $
  let z = x * y
      z' = fromExt $ (toExt x) * (toExt y)
  in z == z'

type ZqTypes = [
  Zq 3,
  Zq 7,
  Zq (3 ** 5),
  Zq (3 ** 5 ** 7)]

zqTypes :: Proxy ZqTypes
zqTypes = Proxy

zqTests = [
  testGroupM "(+)" $ applyBasic zqTypes $ hideArgs prop_add,
  testGroupM "(*)" $ applyBasic zqTypes $ hideArgs prop_mul,
  testGroupM "^-1" $ applyBasic zqTypes $ hideArgs prop_recip,
  testGroupM "extension ring (*)" $ applyBasic (Proxy::Proxy '[ Zq 3, Zq (3 ** 5) ]) $ hideArgs prop_mul_ext
  ]



data LiftedMod r where
  LMod :: (ToInteger (ModRep r)) => ModRep r -> LiftedMod r

data LiftedInvertible r where
  LInv :: (ToInteger (ModRep r)) => ModRep r -> LiftedInvertible r

newtype Invertible r = Invertible r

instance (MonadRandom rnd, Mod r, Random (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedMod r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
    in LMod <$> getRandomR (0,q-1)

instance (MonadRandom rnd, Mod r, Random (ModRep r), PID (ModRep r), ToInteger (ModRep r))
  => Generatable rnd (LiftedInvertible r) where
  genArg =
    let q = proxy modulus (Proxy::Proxy r)
        go = do
          x <- getRandomR (1,q-1)
          if gcd x q == 1
          then return $ LInv x
          else go
    in go

instance (MonadRandom rnd, Generatable rnd (LiftedInvertible r), Ring r)
  => Generatable rnd (Invertible r) where
  genArg = do
    (LInv x) :: LiftedInvertible r <- genArg
    return $ Invertible $ fromIntegral x
{-
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module ZqTests (zqTests) where

import Crypto.Lol.Prelude       hiding (Nat)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

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
-}