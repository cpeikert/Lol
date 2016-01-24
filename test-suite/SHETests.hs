{-# LANGUAGE ScopedTypeVariables, NoImplicitPrelude, RebindableSyntax, 
             DataKinds, TypeOperators, NoMonomorphismRestriction, NoMonoLocalBinds,
             ConstraintKinds, TypeFamilies, FlexibleContexts, PartialTypeSignatures, 
             RankNTypes, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, 
             RebindableSyntax, GADTs, PolyKinds, KindSignatures #-}

module SHETests (sheTests) where

import TestTypes

import Control.Applicative
import Control.Monad
import Control.Monad.Random

import Crypto.Lol.LatticePrelude hiding (lift)
import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Applications.SymmSHE
import Crypto.Lol.CRTrans
import Crypto.Lol.Gadget
import Crypto.Lol.Cyclotomic.Linear

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor
import qualified Crypto.Lol.Cyclotomic.Tensor.CTensor as CT
import Crypto.Lol.Cyclotomic.Tensor.CTensor hiding (CT)
import Crypto.Lol.Types.ZqBasic

import Data.Array.Repa.Eval (Elt)
import Data.Vector.Unboxed (Unbox)
import Data.Vector.Storable (Storable)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (generate,output)
import Test.QuickCheck.Monadic (monadicIO, assert)

v = 1 :: Double

sheTests = 
  [testGroup "Tunnel" $ tunnelTests,
   testGroup "Dec . Enc (Unrestricted)" $ groupCEnc $ wrapEnc prop_encDec,
   testGroup "AddPub" $ groupCEnc $ wrapEnc prop_addPub,
   testGroup "MulPub" $ groupCEnc $ wrapEnc prop_mulPub,
   testGroup "ScalarPub" $ groupCEnc $ wrapScalar prop_addScalar,
   testGroup "CTAdd" $ groupCEnc $ wrapMath prop_ctadd,
   testGroup "CTMul" $ groupCEnc $ wrapMath prop_ctmul,
   testGroup "CT zero" $ groupCEnc $ wrapConst prop_ctzero,
   testGroup "CT one" $ groupCEnc $ wrapConst prop_ctone,
   testGroup "ModSwPT" modSwPTTests,
   testGroup "KSLin" $ groupCKS $ wrapKSLin prop_ksLin,
   testGroup "KSQuad" $ groupCKS $ wrapKSQuad prop_ksQuad,
   testGroup "Embed" $ groupCTwEm $ wrapEm prop_ctembed,
   testGroup "Twace" $ groupCTwEm $ wrapTw prop_cttwace
  ]

type EncDecCtx c m m' zp zq =
  (GenSKCtx c m (LiftOf zp) Double,
   EncryptCtx c m m' (LiftOf zp) zp zq,
   -- constraints from decryptUnrestricted
   ToSDCtx c m' zp zq, Lift' zq, Reduce (LiftOf zq) zp)

prop_encDec :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq) 
  => Proxy '(m', zq) -> Cyc c m zp -> Property
prop_encDec _ x = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x
  let x' = decryptUnrestricted sk $ y
  assert $ x == x'

prop_addPub :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq)
  => Proxy '(m', zq) -> Cyc c m zp -> Property
prop_addPub _ x = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x
  let y' = addPublic x y
      x' = decryptUnrestricted sk y'
  assert $ x' == (x+x)

prop_mulPub :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq)
  => Proxy '(m', zq) -> Cyc c m zp -> Property
prop_mulPub _ x = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x
  let y' = mulPublic x y
      x' = decryptUnrestricted sk y'
  assert $ x' == (x*x)

prop_addScalar :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq)
  => Proxy '(m', zq) -> zp -> Cyc c m zp -> Property
prop_addScalar _ s x = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x
  let y' = addScalar s y
      x' = decryptUnrestricted sk y'
  assert $ x' == ((scalarCyc s)+x)

prop_ctadd :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq) 
  => Proxy '(m', zq) -> Cyc c m zp -> Cyc c m zp -> Property
prop_ctadd _ x1 x2 = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y1 :: CT m zp (Cyc c m' zq) <- encrypt sk x1
  y2 :: CT m zp (Cyc c m' zq) <- encrypt sk x2
  let y' = y1+y2
      x' = decryptUnrestricted sk y'
  assert $ x1+x2 == x'

prop_ctmul :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq)
  => Proxy '(m', zq) -> Cyc c m zp -> Cyc c m zp -> Property
prop_ctmul _ x1 x2 = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y1 :: CT m zp (Cyc c m' zq) <- encrypt sk x1
  y2 :: CT m zp (Cyc c m' zq) <- encrypt sk x2
  let y' = y1*y2
      x' = decryptUnrestricted sk y'
  assert $ x1*x2 == x'

prop_ctzero :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq) 
  => Proxy '(m', zq) -> Proxy (Cyc c m zp) -> Property
prop_ctzero _ _ = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  let z = decryptUnrestricted sk (zero :: CT m zp (Cyc c m' zq))
  assert $ zero == z

prop_ctone :: forall m zp c m' zq . 
  (EncDecCtx c m m' zp zq)
  => Proxy '(m', zq) -> Proxy (Cyc c m zp) -> Property
prop_ctone _ _ = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  let z = decryptUnrestricted sk (one :: CT m zp (Cyc c m' zq))
  assert $ one == z

type EncDecWrapCtx c m m' zp zq =
  (EncDecCtx c m m' zp zq, Show (Cyc c m zp), Arbitrary (c m zp), Show zp, Arbitrary zp)

wrapEnc :: (EncDecWrapCtx c m m' zp zq)
  => (Proxy '(m', zq) -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy '(m, m', zp, zq) -> Property
wrapEnc f _ _ = property $ f Proxy

wrapScalar :: (EncDecWrapCtx c m m' zp zq)
  => (Proxy '(m', zq) -> zp -> Cyc c m zp -> Property)
     -> Proxy (Cyc c) -> Proxy '(m, m', zp, zq) -> Property
wrapScalar f _ _ = property $ f Proxy

wrapMath :: (EncDecWrapCtx c m m' zp zq)
  => (Proxy '(m', zq) -> Cyc c m zp -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy '(m, m', zp, zq) -> Property
wrapMath f _ _ = property $ f Proxy

wrapConst :: (EncDecWrapCtx c m m' zp zq)
  => (Proxy '(m', zq) -> Proxy (Cyc c m zp) -> Property) 
     -> Proxy (Cyc c) -> Proxy '(m, m', zp, zq) -> Property
wrapConst f _ _ = property $ f Proxy Proxy

groupCEnc :: 
  (forall c m m' zp zq . (EncDecWrapCtx c m m' zp zq)
     => Proxy (Cyc c)
     -> Proxy '(m, m', zp, zq)
     -> Property) 
  -> [Test]
groupCEnc f =
  [testGroup "CT" $ groupTypesEnc (f (Proxy::Proxy (Cyc CT.CT))),
   testGroup "RT" $ groupTypesEnc (f (Proxy::Proxy (Cyc RT)))]

type EncDecWrapCCtx m m' zp zq =
  (EncDecWrapCtx RT m m' zp zq,
   EncDecWrapCtx CT.CT m m' zp zq)

groupTypesEnc :: 
  (forall m m' zp zq . (EncDecWrapCCtx m m' zp zq)
    => Proxy '(m, m', zp, zq)
    -> Property)
  -> [Test]
groupTypesEnc f = [testProperty "F7/F7 /ZP2/ZQ2" $ f (Proxy::Proxy '(F7, F7,  ZP2, ZQ2)),
                   testProperty "F7/F21/ZP2/ZQ2" $ f (Proxy::Proxy '(F7, F21, ZP2, ZQ2)),
                   testProperty "F2/F8 /ZP2/536871001" $ f (Proxy::Proxy '(F2,F8,ZP2,Zq 536871001)),
                   testProperty "F1/F8 /ZP2/536871001" $ f (Proxy::Proxy '(F1,F8,ZP2,Zq 536871001)),
                   testProperty "F4/F12/ZP2/SmoothZQ1" $ f (Proxy::Proxy '(F4,F12,ZP2,SmoothZQ1)),
                   testProperty "F4/F8/ZP3/SmoothQ1" $ f (Proxy::Proxy '(F4,F8,ZP3, Zq SmoothQ1)),
                   testProperty "F7/F7 /ZP4/ZQ2" $ f (Proxy::Proxy '(F7, F7,  ZP4, ZQ2)),
                   testProperty "F7/F21/ZP4/ZQ2" $ f (Proxy::Proxy '(F7, F21, ZP4, ZQ2)),
                   testProperty "F1/F4/ZP4/ZQ1" $ f (Proxy::Proxy '(F1,F4,ZP4,ZQ1)),
                   testProperty "F4/F4/ZP4/ZQ1" $ f (Proxy::Proxy '(F4,F4,ZP4,ZQ1)),
                   testProperty "F14/F14/ZP4/ZQ1" $ f (Proxy::Proxy '(F14,F14,ZP4,ZQ1)),
                   testProperty "F28/F28/ZP4/ZQ1" $ f (Proxy::Proxy '(F28,F28,ZP4,ZQ1)),
                   testProperty "F28/F28/ZP4/80221" $ f (Proxy::Proxy '(F28,F28,ZP4,Zq 80221)),
                   testProperty "F1/F8 /ZP4/536871001" $ f (Proxy::Proxy '(F1,F8,ZP4,Zq 536871001)),
                   testProperty "F2/F8 /ZP4/536871001" $ f (Proxy::Proxy '(F2,F8,ZP4,Zq 536871001)),
                   testProperty "F4/F12/ZP8/SmoothZQ1" $ f (Proxy::Proxy '(F4,F12,ZP8,SmoothZQ1))
                  ]

-- one-off tests, no wrapper

prop_modSwPT :: forall m zp c m' zq z v zp' .
  (EncryptCtx c m m' z zp zq,
   GenSKCtx c m z v,
   DecryptCtx c m m' z zp' zq,
   ModSwitchPTCtx c m' zp zp' zq,
   RescaleCyc (Cyc c) zp zp', Mod zp',
   CElt c Int64, CElt c zq,
   z ~ LiftOf zp', v ~ Double, ModRep zp' ~ ModRep zp) 
  => Proxy '(m', zq, zp') -> Cyc c m zp -> Property
prop_modSwPT _ x = monadicIO $ do
  let p = proxy modulus (Proxy::Proxy zp)
      p' = proxy modulus (Proxy::Proxy zp')
      x' = (fromIntegral $ p `div` p') * x
  sk :: SK (Cyc c m' z) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x'
  let y' = modSwitchPT y :: CT m zp' (Cyc c m' zq)
      x'' = decrypt sk y'
  assert $ x'' == rescaleCyc Dec x'

modSwPTTests = 
  [testProperty "RT/F7/F21/ZQ1/ZP4/ZP8" (prop_modSwPT (Proxy::Proxy '(F21, ZQ1, ZP4)) :: Cyc RT F7 ZP8 -> Property),
   testProperty "RT/F7/F42/ZQ1/ZP2/ZP4" (prop_modSwPT (Proxy::Proxy '(F42, ZQ1, ZP2)) :: Cyc RT F7 ZP4 -> Property),
   testProperty "CT/F7/F21/ZQ1/ZP4/ZP8" (prop_modSwPT (Proxy::Proxy '(F21, ZQ1, ZP4)) :: Cyc CT.CT F7 ZP8 -> Property),
   testProperty "CT/F7/F42/ZQ1/ZP2/ZP4" (prop_modSwPT (Proxy::Proxy '(F42, ZQ1, ZP2)) :: Cyc CT.CT F7 ZP4 -> Property)]


tunnelTests = 
  [testProperty "RT/F7/F21/ZQ1/ZP4/ZP8" 
    (prop_ringTunnel (Proxy::Proxy '(F40,ZQ1,F20,F60,TrivGad,ZQ2)) :: Cyc RT F8 ZP4 -> Property)]

prop_ringTunnel :: forall c e r s e' r' s' z zp zq zq' gad . 
  (TunnelCtx c e r s e' r' s' z zp zq' gad,
   RescaleCyc (Cyc c) zq zq', RescaleCyc (Cyc c) zq' zq,
   EncryptCtx c r r' z zp zq,
   GenSKCtx c r' z Double,
   GenSKCtx c s' z Double,
   DecryptCtx c s s' z zp zq,
   Random zp,
   e ~ FGCD r s, Fact e) 
  => Proxy '(r', zq, s, s', gad, zq') -> Cyc c r zp -> Property
prop_ringTunnel _ x = monadicIO $ do
  let totr = proxy totientFact (Proxy::Proxy r)
      tote = proxy totientFact (Proxy::Proxy e)
      basisSize = totr `div` tote
  -- choose a random linear function of the appropriate size
  bs :: [Cyc c s zp] <- replicateM basisSize getRandom
  let f = (linearDec bs) \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s)) :: Linear c zp e r s 
      expected = evalLin f x \\ (gcdDivides (Proxy::Proxy r) (Proxy::Proxy s))
  skin :: SK (Cyc c r' (LiftOf zp)) <- genSK v
  skout :: SK (Cyc c s' (LiftOf zp)) <- genSK v
  y :: CT r zp (Cyc c r' zq) <- encrypt skin x
  tunn <- proxyT (tunnelCT f skout skin) (Proxy::Proxy gad)
  let y' = tunn $ rescaleLinearCT y :: CT s zp (Cyc c s' zq')
      y'' = rescaleLinearCT y' :: CT s zp (Cyc c s' zq)
      actual = decrypt skout y'' :: Cyc c s zp
  assert $ expected == actual












type KsCtx m zp z c m' zq gad zq' deczq = 
  (GenSKCtx c m' z Double,
   z ~ LiftOf zp, 
   EncryptCtx c m m' z zp zq,
   KeySwitchCtx gad c m' zp zq zq', 
   KSHintCtx gad c m' z zq',
   RescaleCyc (Cyc c) zq deczq,
   DecryptCtx c m m' z zp deczq)

prop_ksLin :: forall m zp z c m' zq gad zq' deczq . (KsCtx m zp z c m' zq gad zq' deczq) 
  => Proxy '(m', zq, gad, zq', deczq) -> Cyc c m zp -> Property
prop_ksLin (_ :: Proxy '(m', zq, gad, zq', deczq)) x = monadicIO $ do
  sk1 :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  sk2 :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk1 x
  ks <- proxyT (keySwitchLinear sk2 sk1) (Proxy::Proxy (gad,zq'))
  let y' :: CT m zp (Cyc c m' zq) = ks y
      x' = decrypt sk2 (rescaleLinearCT y' :: CT m zp (Cyc c m' deczq))
  assert $ x == x'

prop_ksQuad :: forall m zp z c m' zq gad zq' deczq . (KsCtx m zp z c m' zq gad zq' deczq) 
  => Proxy '(m', zq, gad, zq', deczq) -> Cyc c m zp -> Cyc c m zp -> Property
prop_ksQuad (_ :: Proxy '(m', zq, gad, zq', deczq)) x1 x2 = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y1 :: CT m zp (Cyc c m' zq) <- encrypt sk x1
  y2 :: CT m zp (Cyc c m' zq) <- encrypt sk x2
  ks <- proxyT (keySwitchQuadCirc sk) (Proxy::Proxy (gad,zq'))
  let y' = ks (y1*y2)
      x' = decrypt sk (rescaleLinearCT y' :: CT m zp (Cyc c m' deczq))
  assert $ x1*x2 == x'

type KsWrapCtx m zp z c m' zq gad zq' deczq = 
  (KsCtx m zp z c m' zq gad zq' deczq, Show (Cyc c m zp), Arbitrary (c m zp))

wrapKSLin :: forall m zp z c m' zq gad zq' deczq . (KsWrapCtx m zp z c m' zq gad zq' deczq)
  => (Proxy '(m', zq, gad, zq', deczq) -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy gad -> Proxy '(m, m', zp, zq, zq', deczq) -> Property
wrapKSLin f _ _ _ = property $ f Proxy

wrapKSQuad :: forall m zp z c m' zq gad zq' deczq . (KsWrapCtx m zp z c m' zq gad zq' deczq)
  => (Proxy '(m', zq, gad, zq',deczq) -> Cyc c m zp -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy gad -> Proxy '(m, m', zp, zq, zq', deczq) -> Property
wrapKSQuad f _ _ _ = property $ f Proxy

groupCKS :: 
  (forall c m m' zp z zq zq' gad deczq . (KsWrapCtx m zp z c m' zq gad zq' deczq)
     => Proxy (Cyc c)
     -> Proxy gad
     -> Proxy '(m, m', zp, zq, zq', deczq)
     -> Property)
  -> [Test]
groupCKS f =
  [testGroup "CT" $ groupGadKS $ f (Proxy::Proxy (Cyc CT.CT)),
   testGroup "RT" $ groupGadKS $ f (Proxy::Proxy (Cyc RT))]

type KsWrapCCtx m zp z m' zq gad zq' deczq = 
  (KsWrapCtx m zp z RT m' zq gad zq' deczq,
   KsWrapCtx m zp z CT.CT m' zq gad zq' deczq)

groupGadKS :: 
  (forall m m' zp z zq zq' gad deczq . (KsWrapCCtx m zp z m' zq gad zq' deczq)
     => Proxy gad
     -> Proxy '(m, m', zp, zq, zq', deczq)
     -> Property) 
  -> [Test]
groupGadKS f =
  [testGroup "TrivGad" $ groupTypesKS (f (Proxy::Proxy TrivGad))]
   --testGroup "Base16" $ groupTypesKS (f (Proxy::Proxy (BaseBGad N16)))]

type KsWrapCGadCtx m zp z m' zq zq' deczq = 
  (KsWrapCCtx m zp z m' zq TrivGad zq' deczq)

groupTypesKS :: 
  (forall m m' zp z zq zq' deczq . (KsWrapCGadCtx m zp z m' zq zq' deczq)
    => Proxy '(m, m', zp, zq, zq', deczq) 
    -> Property) 
  -> [Test]
groupTypesKS f = 
  [testProperty "F1/F7/ZP2/ZQ1/ZQ2" $ f (Proxy::Proxy '(F1, F7, ZP2, ZQ1, ZQ2, ZQ1)),
   testProperty "F2/F4/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F2, F4, ZP8, SmoothZQ1, SmoothZQ2, SmoothZQ1)),
   testProperty "F4/F12/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F4, F12, ZP2, SmoothZQ1, SmoothZQ2, SmoothZQ1)),
   testProperty "F8/F64/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F8, F64, ZP2, SmoothZQ1, SmoothZQ2, SmoothZQ1)),
   testProperty "F3/F27/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F3, F27, ZP2, SmoothZQ1, SmoothZQ2, SmoothZQ1)),
   testProperty "F2/F4/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F2, F4, ZP8, SmoothZQ2, SmoothZQ3, SmoothZQ1)),
   testProperty "F4/F12/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F4, F12, ZP2, SmoothZQ2, SmoothZQ3, SmoothZQ1)),
   testProperty "F8/F64/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F8, F64, ZP2, SmoothZQ2, SmoothZQ3, SmoothZQ1)),
   testProperty "F3/F27/ZP2/SmoothZQ1/SmoothZQ2" $ f (Proxy::Proxy '(F3, F27, ZP2, SmoothZQ2, SmoothZQ3, SmoothZQ1))]


















type TwEmCtx c m m' t t' zp zq =
  (EncryptCtx c m m' (LiftOf zp) zp zq,
   GenSKCtx c m (LiftOf zp) Double, 
   DecryptCtx c m m' (LiftOf zp) zp zq, 
   t `Divides` t', m `Divides` t, m' `Divides` t', m ~ FGCD m' t)

prop_ctembed :: forall c m m' t t' zp zq . 
  (TwEmCtx c m m' t t' zp zq)
  => Proxy '(m', zq, t, t') -> Cyc c m zp -> Property
prop_ctembed _ x = monadicIO $ do
  sk :: SK (Cyc c m' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt sk x
  let y' = embedCT y :: CT t zp (Cyc c t' zq)
      x' = decrypt (embedSK sk) y'
  assert $ (embed x :: Cyc c t zp) == x'

prop_cttwace :: forall c m m' t t' zp zq . 
  (TwEmCtx c t t' m m' zp zq)
  => Proxy '(m', zq, t, t') -> Cyc c m zp -> Property
prop_cttwace _ x = monadicIO $ do
  sk :: SK (Cyc c t' (LiftOf zp)) <- genSK v
  y :: CT m zp (Cyc c m' zq) <- encrypt (embedSK sk) x
  let y' = twaceCT y :: CT t zp (Cyc c t' zq)
      x' = decrypt sk y'
  assert $ (twace x :: Cyc c t zp) == x'

type TwEmWrapCtx c m m' t t' zp zq = 
  (TwEmCtx c m m' t t' zp zq, Show (Cyc c m zp), Show (Cyc c t zp), Arbitrary (c m zp), Arbitrary (c t zp))

wrapEm :: (TwEmWrapCtx c m m' t t' zp zq)
  => (Proxy '(m', zq, t, t') -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy '(m, m', t, t', zp, zq) -> Property
wrapEm f _ _ = property $ f Proxy

wrapTw :: (TwEmWrapCtx c t t' m m' zp zq)
  => (Proxy '(m', zq, t, t') -> Cyc c m zp -> Property) 
     -> Proxy (Cyc c) -> Proxy '(t, t', m, m', zp, zq) -> Property
wrapTw f _ _ = property $ f Proxy

groupCTwEm :: 
  (forall c m m' t t' zp zq . (TwEmWrapCtx c m m' t t' zp zq)
     => Proxy (Cyc c)
     -> Proxy '(m, m', t, t', zp, zq)
     -> Property) 
  -> [Test]
groupCTwEm f =
  [testGroup "CT" $ groupTypesTwEm (f (Proxy::Proxy (Cyc CT.CT))),
   testGroup "RT" $ groupTypesTwEm (f (Proxy::Proxy (Cyc RT)))]

type TwEmWrapCCtx m m' t t' zp zq =
  (TwEmWrapCtx RT m m' t t' zp zq,
   TwEmWrapCtx CT.CT m m' t t' zp zq)

groupTypesTwEm :: 
  (forall m m' t t' zp zq . (TwEmWrapCCtx m m' t t' zp zq)
    => Proxy '(m, m', t, t', zp, zq) 
    -> Property) 
  -> [Test]
groupTypesTwEm f = 
  [testProperty "F1/F7/F3/F21/ZP2/ZQ1" $ f (Proxy::Proxy '(F1, F7, F3, F21, ZP2, ZQ1))]
