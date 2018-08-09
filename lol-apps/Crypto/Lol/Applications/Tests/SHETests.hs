{-|
Module      : Crypto.Lol.Applications.Tests.SHETests
Description : Tests for SymmSHE.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for SymmSHE.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Applications.Tests.SHETests
( decTest
, ksTests
, modSwPTTest
, sheTests
, tunnelTests
, twemTests
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.Tests                (Gen, Test, chooseAny, showType,
                                        testGroup, testIOWithGen,
                                        testWithGen)

-- EAC: is there a simple way to parameterize the variance?
-- generates a secret key with scaled variance 1.0
instance (GenSKCtx c m' z Double) => Random (SK (c m' z)) where
  -- Need this for 'chooseAny' in our tests
  random = runRand $ genSK (1 :: Double)
  randomR = error "randomR not defined for SK"

consGens2 :: Gen a -> Gen b -> Gen (a, b)
consGens2 g h = liftA2 (,) g h

consGens3 :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
consGens3 g h i = liftA3 (,,) g h i

sheTests :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (zp :: *) (zq :: *) . _
         => Proxy '(m,m',zp,zq) -> Proxy t -> Test
sheTests _ _ =
  let ptmmrr = Proxy::Proxy '(t,m,m',zp,zq)
      gpt = chooseAny :: Gen (PT (Cyc t m zp))
      gsk = chooseAny :: Gen (SK (Cyc t m' (LiftOf zp)))
      gc = chooseAny :: Gen (Cyc t m zp)
  in testGroup (showType ptmmrr)
  [ testIOWithGen "Dec . Enc"  (prop_encDec ptmmrr)  (consGens2 gpt gsk)
  , testIOWithGen "AddPub"     (prop_addPub ptmmrr)  (consGens3 gc  gpt gsk)
  , testIOWithGen "MulPub"     (prop_mulPub ptmmrr)  (consGens3 gc  gpt gsk)
  , testIOWithGen "CTAdd"      (prop_ctadd ptmmrr)   (consGens3 gpt gpt gsk)
  , testIOWithGen "CTAdd2"     (prop_ctadd2 ptmmrr)  (consGens3 gpt gpt gsk)
  , testIOWithGen "CTMul"      (prop_ctmul ptmmrr)   (consGens3 gpt gpt gsk)
  , testWithGen   "CT zero"    (prop_ctzero ptmmrr)  gsk
  , testWithGen   "CT one"     (prop_ctone ptmmrr)   gsk
  ]

-- zq must be liftable
decTest :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (zp :: *) (zq :: *) . _
        => Proxy '(m,m',zp,zq) -> Proxy t -> Test
decTest _ _ =
  let ptmmrr = Proxy::Proxy '(t,m,m',zp,zq)
      gsk = chooseAny :: Gen (SK (Cyc t m' (LiftOf zp)))
      gpt = chooseAny :: Gen (Cyc t m zp)
  in testGroup (showType ptmmrr)
               [testIOWithGen "Dec . Enc" (prop_encDec ptmmrr) (consGens2 gpt gsk)]

-- (PT (Cyc t m zp), SK (Cyc t m' z))
modSwPTTest :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (zp :: *) zp' zq . _
  => Proxy '(m,m',zp,zp',zq) -> Proxy t -> Test
modSwPTTest _ _ =
  let ptmmrrr = Proxy::Proxy '(t,m,m',zp,zp',zq)
      gpt = chooseAny :: Gen (PT (Cyc t m zp))
      gsk = chooseAny :: Gen (SK (Cyc t m' (LiftOf zp)))
  in testGroup (showType ptmmrrr)
               [testIOWithGen "ModSwitch PT" (prop_modSwPT ptmmrrr) (consGens2 gpt gsk)]

ksTests :: forall (t :: Factored -> * -> *) (m :: Factored) (m' :: Factored) (zp :: *) zq gad . _
  => Proxy '(m,m',zp,zq) -> Proxy gad -> Proxy t -> Test
ksTests _ _ _ =
  let ptmmrrg = Proxy::Proxy '(t,m,m',zp,zq,gad)
      gpt = chooseAny :: Gen (PT (Cyc t m zp))
      gsk = chooseAny :: Gen (SK (Cyc t m' (LiftOf zp)))
  in testGroup (showType ptmmrrg) [
    testIOWithGen "KSLin" (prop_ksLin ptmmrrg) (consGens3 gpt gsk gsk),
    testIOWithGen "KSQuad" (prop_ksQuad ptmmrrg) (consGens3 gpt gpt gsk)]

twemTests :: forall (t :: Factored -> * -> *) (r :: Factored) (r' :: Factored)
                    (s :: Factored) s' (zp :: *) zq . _
  => Proxy '(r,r',s,s',zp,zq) -> Proxy t -> Test
twemTests _ _ =
  let p = Proxy::Proxy '(t,r,r',s,s',zp,zq) -- Gave up on the Proxy naming convention here
      gpt = chooseAny :: Gen (PT (Cyc t r zp))
      gsk = chooseAny :: Gen (SK (Cyc t r' (LiftOf zp)))
      gpt' = chooseAny :: Gen (PT (Cyc t s zp))
  in testGroup (showType p) [
      testIOWithGen "Embed" (prop_ctembed p) (consGens2 gpt gsk),
      testIOWithGen "Twace" (prop_cttwace p) (consGens2 gpt' gsk)]

tunnelTests :: forall (t :: Factored -> * -> *) (r :: Factored) (r' :: Factored)
                      s (s' :: Factored) (zp :: *) zq gad . (_)
  => Proxy '(r,r',s,s',zp,zq) -> Proxy gad -> Proxy t -> Test
tunnelTests _ _ _ =
  let p = Proxy::Proxy '(t,r,r',s,s',zp,zq,gad)
      gpt = chooseAny :: Gen (PT (Cyc t r zp))
      gsk = chooseAny :: Gen (SK (Cyc t r' (LiftOf zp)))
      gsk' = chooseAny :: Gen (SK (Cyc t s' (LiftOf zp)))
  in testGroup (showType p)
               [testIOWithGen "Tunnel" (prop_tunnel p) (consGens3 gpt gsk gsk')]

prop_encDec :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq) -> (PT (Cyc t m zp), SK (Cyc t m' z)) -> IO Bool
prop_encDec _ (x, sk) = do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decrypt sk $ y
  return $ x == x'

prop_addPub :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> (Cyc t m zp, PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_addPub _ (a, pt, sk) = do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = addPublic a ct
      pt' = decrypt sk ct'
  return $ pt' == (a+pt)

prop_mulPub :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> (Cyc t m zp, PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_mulPub _ (a, pt, sk) = do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = mulPublic a ct
      pt' = decrypt sk ct'
  return $ pt' == (a*pt)

prop_ctadd :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> (PT (Cyc t m zp), PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_ctadd _ (pt1, pt2, sk) = do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  let ct' = ct1 + ct2
      pt' = decrypt sk ct'
  return $ pt1+pt2 == pt'

-- tests adding with different scale values
prop_ctadd2 :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> (PT (Cyc t m zp), PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_ctadd2 _ (pt1, pt2, sk) = do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  -- no-op to induce unequal scale values
  let ct' = ct1 + (modSwitchPT ct2)
      pt' = decrypt sk ct'
  return $ pt1+pt2 == pt'

prop_ctmul :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq)
     -> (PT (Cyc t m zp), PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_ctmul _ (pt1, pt2, sk) = do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  let ct' = ct1 * ct2
      pt' = decrypt sk ct'
  return $ pt1*pt2 == pt'

prop_ctzero :: forall t m m' z zp (zq :: *) . (z ~ LiftOf zp, Fact m, _)
  => Proxy '(t,m,m',zp,zq)
     -> SK (Cyc t m' z)
     -> Bool
prop_ctzero _ sk = zero == decrypt sk (zero :: CT m zp (Cyc t m' zq))

prop_ctone :: forall t m m' z zp (zq :: *) . (z ~ LiftOf zp, Fact m, _)
  => Proxy '(t,m,m',zp,zq)
     -> SK (Cyc t m' z)
     -> Bool
prop_ctone _ sk = one == decrypt sk (one :: CT m zp (Cyc t m' zq))

prop_modSwPT :: forall t m m' z zp (zp' :: *) (zq :: *) . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zp',zq)
     -> (PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_modSwPT _ (pt, sk) = do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let p = modulus @zp
      p' = modulus @zp'
      z = (fromIntegral $ p `div` p')*y
      x = decrypt sk z
      y' = modSwitchPT z :: CT m zp' (Cyc t m' zq)
      x'' = decrypt sk y'
  return $ x'' == rescaleCyc Dec x

prop_ksLin :: forall t m m' z zp (zq :: *) (gad :: *) . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq,gad)
     -> (PT (Cyc t m zp), SK (Cyc t m' z), SK (Cyc t m' z))
     -> IO Bool
prop_ksLin _ (pt, skin, skout) = do
  ct <- encrypt skin pt
  hint :: KSHint gad (Cyc t m' zq) <- ksLinearHint skout skin
  let ct' = keySwitchLinear hint ct :: CT m zp (Cyc t m' zq)
      pt' = decrypt skout ct'
  return $ pt == pt'

prop_ksQuad :: forall t m m' z zp zq (gad :: *) . (z ~ LiftOf zp, _)
  => Proxy '(t,m,m',zp,zq,gad)
     -> (PT (Cyc t m zp), PT (Cyc t m zp), SK (Cyc t m' z))
     -> IO Bool
prop_ksQuad _ (pt1, pt2, sk) = do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 <- encrypt sk pt2
  hint :: KSHint gad (Cyc t m' zq) <- ksQuadCircHint sk
  let ct' = keySwitchQuadCirc hint $ ct1*ct2
      ptProd = pt1*pt2
      pt' = decrypt sk ct'
  return $ ptProd == pt'

prop_ctembed :: forall t r r' s s' z zp (zq :: *) . (z ~ LiftOf zp, Fact s', Fact s, _)
  => Proxy '(t,r,r',s,s',zp,zq)
     -> (PT (Cyc t r zp), SK (Cyc t r' z))
     -> IO Bool
prop_ctembed _ (pt, sk) = do
  ct :: CT r zp (Cyc t r' zq) <- encrypt sk pt
  let ct' = embedCT ct :: CT s zp (Cyc t s' zq)
      pt' = decrypt (embedSK sk) ct'
  return $ embed pt == pt'

-- CT must be encrypted with key from small ring
prop_cttwace :: forall t r r' s s' z zp (zq :: *) . (z ~ LiftOf zp, Fact r, _)
  => Proxy '(t,r,r',s,s',zp,zq)
     -> (PT (Cyc t s zp), SK (Cyc t r' z))
     -> IO Bool
prop_cttwace _ (pt, sk) = do
  ct :: CT s zp (Cyc t s' zq) <- encrypt (embedSK sk) pt
  let ct' = twaceCT ct :: CT r zp (Cyc t r' zq)
      pt' = decrypt sk ct'
  return $ twace pt == pt'

prop_tunnel :: forall c t e r s e' r' s' z zp zq gad .
  (c ~ Cyc t,
   TunnelHintCtx c e r s e' r' s' z zp zq gad,
   TunnelCtx c r s e' r' s' zp zq gad,
   EncryptCtx c r r' z zp zq,
   DecryptCtx c s s' z zp zq,
   Cyclotomic (Cyc t s zp), Random (Cyc t s zp), Ring (Cyc t s zp), Eq (Cyc t s zp),
   Random zp, Eq zp,
   e ~ FGCD r s, Fact e)
  => Proxy '(t,r,r',s,s',zp,zq,gad)
  -> (PT (Cyc t r zp), SK (Cyc t r' z), SK (Cyc t s' z))
  -> IO Bool
prop_tunnel _ (x, skin, skout) = do
  let totr = totientFact @r
      tote = totientFact @e
      basisSize = totr `div` tote
  -- choose a random linear function of the appropriate size
  bs :: [Cyc t s zp] <- replicateM basisSize getRandom
  let f = linearDec bs \\ (gcdDivides @r @s) :: Linear c e r s zp
      expected = evalLin f x \\ (gcdDivides @r @s)
  y :: CT r zp (Cyc t r' zq) <- encrypt skin x
  hints :: TunnelHint gad c e r s e' r' s' zp zq <- tunnelHint f skout skin
  let y' = tunnel hints y :: CT s zp (Cyc t s' zq)
      actual = decrypt skout y' :: Cyc t s zp
  return $ expected == actual
