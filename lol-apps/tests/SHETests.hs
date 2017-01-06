{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module SHETests (sheTests, decTest, modSwPTTest, ksTests, twemTests, tunnelTests) where

import Crypto.Lol.Tests

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

import qualified Test.Framework as TF

sheTests :: forall t m m' zp zq rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy t -> [rnd TF.Test]
sheTests _ _ =
  let ptmr = Proxy::Proxy '(t,m,m',zp,zq)
  in ($ ptmr) <$> [
   genTestArgs "DecU . Enc" prop_encDecU,
   genTestArgs "AddPub"     prop_addPub,
   genTestArgs "MulScal"    prop_mulScal,
   genTestArgs "MulPub"     prop_mulPub,
   genTestArgs "ScalarPub"  prop_addScalar,
   genTestArgs "CTAdd"      prop_ctadd,
   genTestArgs "CTAdd2"     prop_ctadd2,
   genTestArgs "CTMul"      prop_ctmul,
   genTestArgs "CT zero"    prop_ctzero,
   genTestArgs "CT one"     prop_ctone]

-- zq must be liftable
decTest :: forall t m m' zp zq rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq) -> Proxy t -> [rnd TF.Test]
decTest _ _ =
  [genTestArgs "Dec . Enc" prop_encDec (Proxy::Proxy '(t,m,m',zp,zq))]

modSwPTTest :: forall t m m' zp zp' zq rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zp',zq) -> Proxy t -> [rnd TF.Test]
modSwPTTest _ _ =
  [genTestArgs "ModSwitch PT" prop_modSwPT (Proxy::Proxy '(t,m,m',zp,zp',zq))]

ksTests :: forall t m m' zp zq zq' gad rnd . (MonadRandom rnd, _)
  => Proxy '(m,m',zp,zq,zq') -> Proxy gad -> Proxy t -> [rnd TF.Test]
ksTests _ _ _ =
  let ptmr = Proxy::Proxy '(t,m,m',zp,zq,zq',gad)
  in ($ ptmr) <$> [
    genTestArgs "KSLin" prop_ksLin,
    genTestArgs "KSQuad" prop_ksQuad]

twemTests :: forall t r r' s s' zp zq rnd . (MonadRandom rnd, _)
  => Proxy '(r,r',s,s',zp,zq) -> Proxy t -> [rnd TF.Test]
twemTests _ _ =
  let ptmr = Proxy::Proxy '(t,r,r',s,s',zp,zq)
  in [genTestArgs "Embed" prop_ctembed ptmr,
      genTestArgs "Twace" prop_cttwace ptmr]

tunnelTests :: forall t r r' s s' zp zq gad rnd . (MonadRandom rnd, _)
  => Proxy '(r,r',s,s',zp,zq) -> Proxy gad -> Proxy t -> [rnd TF.Test]
tunnelTests _ _ _ =
  [genTestArgs "Tunnel" prop_ringTunnel (Proxy::Proxy '(t,r,r',s,s',zp,zq,gad))]





prop_encDecU :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
prop_encDecU x sk = testIO $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decryptUnrestricted sk $ y
  return $ x == x'

prop_addPub :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Cyc t m zp
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_addPub a pt sk = testIO $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = addPublic a ct
      pt' = decryptUnrestricted sk ct'
  return $ pt' == (a+pt)

prop_mulScal :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => zp
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_mulScal a pt sk = testIO $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = mulScalar a ct
      pt' = decryptUnrestricted sk ct'
  return $ pt' == ((scalarCyc a) * pt)

prop_mulPub :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => Cyc t m zp
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_mulPub a pt sk = testIO $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = mulPublic a ct
      pt' = decryptUnrestricted sk ct'
  return $ pt' == (a*pt)

prop_addScalar :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => zp -> PT (Cyc t m zp) -> SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
prop_addScalar c pt sk = testIO $ do
  ct :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let ct' = addScalar c ct
      pt' = decryptUnrestricted sk ct'
  return $ pt' == ((scalarCyc c)+pt)

prop_ctadd :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_ctadd pt1 pt2 sk = testIO $ do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  let ct' = ct1 + ct2
      pt' = decryptUnrestricted sk ct'
  return $ pt1+pt2 == pt'

-- tests adding with different scale values
prop_ctadd2 :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_ctadd2 pt1 pt2 sk = testIO $ do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  -- no-op to induce unequal scale values
  let ct' = ct1 + (modSwitchPT ct2)
      pt' = decryptUnrestricted sk ct'
  return $ pt1+pt2 == pt'

prop_ctmul :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp)
     -> PT (Cyc t m zp)
     -> SK (Cyc t m' z)
     -> Test '(t,m,m',zp,zq)
prop_ctmul pt1 pt2 sk = testIO $ do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 :: CT m zp (Cyc t m' zq) <- encrypt sk pt2
  let ct' = ct1 * ct2
      pt' = decryptUnrestricted sk ct'
  return $ pt1*pt2 == pt'

prop_ctzero :: forall t m m' z zp (zq :: *) . (z ~ LiftOf zp, Fact m, _)
  => SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
prop_ctzero sk =
  let z = decryptUnrestricted sk (zero :: CT m zp (Cyc t m' zq))
  in test $ zero == z

prop_ctone :: forall t m m' z zp (zq :: *) . (z ~ LiftOf zp, Fact m, _)
  => SK (Cyc t m' z) -> Test '(t,m,m',zp,zq)
prop_ctone sk =
  let z = decryptUnrestricted sk (one :: CT m zp (Cyc t m' zq)) :: Cyc t m zp
  in test $ one == z

prop_encDec :: forall t m m' z zp zq . (z ~ LiftOf zp, _)
  => SK (Cyc t m' z) -> Cyc t m zp -> Test '(t,m,m',zp,zq)
prop_encDec sk x = testIO $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk x
  let x' = decrypt sk $ y
  return $ x == x'

prop_modSwPT :: forall t m m' z zp (zp' :: *) (zq :: *) . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> Test '(t,m,m',zp,zp',zq)
prop_modSwPT pt sk = testIO $ do
  y :: CT m zp (Cyc t m' zq) <- encrypt sk pt
  let p = proxy modulus (Proxy::Proxy zp)
      p' = proxy modulus (Proxy::Proxy zp')
      z = (fromIntegral $ p `div` p')*y
      x = decryptUnrestricted sk z
      y' = modSwitchPT z :: CT m zp' (Cyc t m' zq)
      x'' = decryptUnrestricted sk y'
  return $ x'' == rescaleCyc Dec x

prop_ksLin :: forall t m m' z zp (zq :: *) (zq' :: *) (gad :: *) . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> SK (Cyc t m' z) -> SK (Cyc t m' z) -> Test '(t,m,m',zp,zq,zq',gad)
prop_ksLin pt skin skout = testIO $ do
  ct <- encrypt skin pt
  kslHint :: KSLinearHint gad (Cyc t m' zq') <- ksLinearHint skout skin
  let ct' = keySwitchLinear kslHint ct :: CT m zp (Cyc t m' zq)
      pt' = decryptUnrestricted skout ct'
  return $ pt == pt'

prop_ksQuad :: forall t m m' z zp zq (zq' :: *) (gad :: *) . (z ~ LiftOf zp, _)
  => PT (Cyc t m zp) -> PT (Cyc t m zp) -> SK (Cyc t m' z) -> Test '(t,m,m',zp,zq,zq',gad)
prop_ksQuad pt1 pt2 sk = testIO $ do
  ct1 :: CT m zp (Cyc t m' zq) <- encrypt sk pt1
  ct2 <- encrypt sk pt2
  ksqHint :: KSQuadCircHint gad (Cyc t m' zq') <- ksQuadCircHint sk
  let ct' = keySwitchQuadCirc ksqHint $ ct1*ct2
      ptProd = pt1*pt2
      pt' = decryptUnrestricted sk ct'
  return $ ptProd == pt'

prop_ctembed :: forall t r r' s s' z zp (zq :: *) . (z ~ LiftOf zp, Fact s', Fact s, _)
  => PT (Cyc t r zp) -> SK (Cyc t r' z) -> Test '(t,r,r',s,s',zp,zq)
prop_ctembed pt sk =testIO $ do
  ct :: CT r zp (Cyc t r' zq) <- encrypt sk pt
  let ct' = embedCT ct :: CT s zp (Cyc t s' zq)
      pt' = decryptUnrestricted (embedSK sk) ct'
  return $ embed pt == pt'

-- CT must be encrypted with key from small ring
prop_cttwace :: forall t r r' s s' z zp (zq :: *) . (z ~ LiftOf zp, Fact r, _)
  => PT (Cyc t s zp) -> SK (Cyc t r' z) -> Test '(t,r,r',s,s',zp,zq)
prop_cttwace pt sk = testIO $ do
  ct :: CT s zp (Cyc t s' zq) <- encrypt (embedSK sk) pt
  let ct' = twaceCT ct :: CT r zp (Cyc t r' zq)
      pt' = decryptUnrestricted sk ct'
  return $ twace pt == pt'

prop_ringTunnel :: forall t e r s e' r' s' z zp zq gad .
  (GenTunnelInfoCtx t e r s e' r' s' z zp zq gad,
   TunnelCtx t r s e' r' s' zp zq gad,
   EncryptCtx t r r' z zp zq,
   DecryptUCtx t s s' z zp zq,
   Random zp, Eq zp,
   e ~ FGCD r s, Fact e)
  => PT (Cyc t r zp) -> SK (Cyc t r' z) -> SK (Cyc t s' z) -> Test '(t,r,r',s,s',zp,zq,gad)
prop_ringTunnel x skin skout = testIO $ do
  let totr = proxy totientFact (Proxy::Proxy r)
      tote = proxy totientFact (Proxy::Proxy e)
      basisSize = totr `div` tote
  -- choose a random linear function of the appropriate size
  bs :: [Cyc t s zp] <- replicateM basisSize getRandom
  let f = linearDec bs \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)) :: Linear t zp e r s
      expected = evalLin f x \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s))
  y :: CT r zp (Cyc t r' zq) <- encrypt skin x
  hints :: TunnelInfo gad t e r s e' r' s' zp zq <- tunnelInfo f skout skin
  let y' = tunnelCT hints y :: CT s zp (Cyc t s' zq)
      actual = decryptUnrestricted skout y' :: Cyc t s zp
  return $ expected == actual
