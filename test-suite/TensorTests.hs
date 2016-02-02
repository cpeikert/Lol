{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, ScopedTypeVariables, 
             DataKinds, TypeOperators, KindSignatures, RankNTypes, GADTs,
             MultiParamTypeClasses, ConstraintKinds, FlexibleInstances, RebindableSyntax,
             FlexibleContexts, UndecidableInstances, TypeFamilies #-}

module TensorTests (tensorTests) where


import TestTypes

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude as LP hiding (round)
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor

import Control.Applicative
import Control.Monad.Random

import Data.Array.Repa.Eval (Elt)
import Data.Constraint
import Data.Maybe
import Data.Vector.Unboxed as U
import Data.Vector.Storable (Storable)

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding (generate,output)

tensorTests = 
  [testGroup "fmap comparison" $ groupTMR $ wrapTmrToBool prop_fmap,
   testGroup "fmap comparison 2" $ groupTMR $ wrapTmrToBool prop_fmap2,
   testGroup "Extension Mult" $ groupExtTests $ wrap2TmrToBool prop_mul_ext,
   testGroup "GSqNormDec" $ groupNorm $ wrapNorm prop_gsqnorm,

   -- inverse property
   tremTests, 
   gInvGTests,
   testGroup "CRTInv.CRT == id" $ groupTMR $ wrapTmrToBool prop_crt_inv,
   testGroup "LInv.L == id" $ groupTMR $ wrapTmrToBool prop_l_inv,

   -- commutative property
   gCommuteTests,
   embedCommuteTests,
   twaceCommuteTests,
   testGroup "Scalar" $ groupTMR $ wrapRToBool prop_scalar_crt,
   twaceInvarTests
   ]

type TMRCtx t m r = (Tensor t, Fact m, m `Divides` m, CRTrans r, TElt t r, CRTEmbed r, 
                     TElt t (CRTExt r), Eq r, ZeroTestable r, IntegralDomain r)

prop_fmap :: (TMRCtx t m r) => t m r -> Bool
prop_fmap x = fmapT id x == x \\ witness entailEqT x \\ witness entailIndexT x

prop_fmap2 :: (TMRCtx t m r) => t m r -> Bool
prop_fmap2 x = (fmapT id x) == (fmap id x) \\ witness entailEqT x \\ witness entailIndexT x

-- tests that multiplication in the extension ring matches CRT multiplication
prop_mul_ext :: forall t m r . (TMRCtx t m r)
  => t m r -> t m r -> Bool
prop_mul_ext x y = 
  let m = proxy valueFact (Proxy::Proxy m)
  in case (crtInfo m :: Maybe (CRTInfo r)) of
       Nothing -> error "mul have a CRT to call prop_mul_ext"
       Just _ -> (let z = x * y
                      z' = fmapT fromExt $ (fmapT toExt x) * (fmapT toExt y)
                  in z == z') \\ witness entailEqT x 
                              \\ witness entailRingT x
                              \\ witness entailRingT (fmap toExt x)
                              \\ witness entailIndexT x

gInvGTests = testGroup "GInv.G == id" [
  testGroup "Pow basis" $ groupTMR $ wrapTmrToBool prop_ginv_pow,
  testGroup "Dec basis" $ groupTMR $ wrapTmrToBool prop_ginv_dec,
  testGroup "CRT basis" $ groupTMR $ wrapTmrToBool prop_ginv_crt]

-- divG . mulG == id in Pow basis
prop_ginv_pow :: (TMRCtx t m r) => t m r -> Bool
prop_ginv_pow x = (fromMaybe (error "could not divide by G in prop_ginv_pow") $ 
  divGPow $ mulGPow x) == x \\ witness entailEqT x

-- divG . mulG == id in Dec basis
prop_ginv_dec :: (TMRCtx t m r) => t m r -> Bool
prop_ginv_dec x = (fromMaybe (error "could not divide by G in prop_ginv_dec") $ 
  divGDec $ mulGDec x) == x \\ witness entailEqT x

-- divG . mulG == id in CRT basis
prop_ginv_crt :: (TMRCtx t m r) => t m r -> Bool
prop_ginv_crt x = fromMaybe (error "no CRT in prop_ginv_crt") $ do
  divGCRT' <- divGCRT
  mulGCRT' <- mulGCRT
  return $ (divGCRT' $ mulGCRT' x) == x \\ witness entailEqT x

-- crtInv . crt == id
prop_crt_inv :: (TMRCtx t m r) => t m r -> Bool
prop_crt_inv x = fromMaybe (error "no CRT in prop_crt_inv") $ do
  crt' <- crt
  crtInv' <- crtInv
  return $ (crtInv' $ crt' x) == x \\ witness entailEqT x

-- lInv . l == id
prop_l_inv :: (TMRCtx t m r) => t m r -> Bool
prop_l_inv x = (lInv $ l x) == x \\ witness entailEqT x

-- scalarCRT = crt . scalarPow
prop_scalar_crt :: forall t m r . (TMRCtx t m r)
                   => Proxy (t m r) -> r -> Bool
prop_scalar_crt _ r = fromMaybe (error "no CRT in prop_scalar_crt") $ do
  scalarCRT' <- scalarCRT
  crt' <- crt
  return $ (scalarCRT' r :: t m r) == (crt' $ scalarPow r)
  \\ proxy entailEqT (Proxy::Proxy (t m r))

gCommuteTests = testGroup "G commutes with L" [
  testGroup "Dec basis" $ groupTMR $ wrapTmrToBool prop_g_dec,
  testGroup "CRT basis" $ groupTMR $ wrapTmrToBool prop_g_crt]

-- mulGDec == lInv. mulGPow . l
prop_g_dec :: (TMRCtx t m r) => t m r -> Bool
prop_g_dec x = (mulGDec x) == (lInv $ mulGPow $ l x) \\ witness entailEqT x

prop_g_crt :: (TMRCtx t m r) => t m r -> Bool
prop_g_crt x = fromMaybe (error "no CRT in prop_g_crt") $ do
  mulGCRT' <- mulGCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (mulGCRT' x) == (crt' $ mulGPow $ crtInv' x) \\ witness entailEqT x

type TMRWrapCtx t m r = (TMRCtx t m r, Show (t m r), Arbitrary (t m r), Show r, Arbitrary r)

wrap2TmrToBool :: (TMRWrapCtx t m r) => (t m r -> t m r -> Bool) 
     -> Proxy t -> Proxy '(m,r) -> Property
wrap2TmrToBool f _ _ = property f

wrapTmrToBool :: (TMRWrapCtx t m r) => (t m r -> Bool) 
     -> Proxy t -> Proxy '(m,r) -> Property
wrapTmrToBool f _ _ = property f

wrapRToBool :: (TMRWrapCtx t m r) => (Proxy (t m r) -> r -> Bool)
     -> Proxy t -> Proxy '(m,r) -> Property
wrapRToBool f _ _ = property $ f Proxy 

groupTMR :: (forall t m r . (TMRWrapCtx t m r)
             => Proxy t
             -> Proxy '(m,r) 
             -> Property) -> [Test]
groupTMR f =
  [testGroup "CT" $ groupMR (f (Proxy::Proxy CT))]
   --testGroup "RT" $ groupMR (f (Proxy::Proxy RT))]

groupExtTests :: (forall t m r . (TMRWrapCtx t m r)
             => Proxy t
             -> Proxy '(m,r) 
             -> Property) -> [Test]
groupExtTests f =
  [testGroup "CT" $ groupMRExt (f (Proxy::Proxy CT)),
   testGroup "RT" $ groupMRExt (f (Proxy::Proxy RT))]

type MRWrapCtx m r = (TMRWrapCtx CT m r, TMRWrapCtx RT m r)

groupMR :: (forall m r . (MRWrapCtx m r) => Proxy '(m, r) -> Property) 
            -> [Test]
groupMR f = [testProperty "F7/29" $ f (Proxy::Proxy '(F7, Zq 29)),
             testProperty "F12/SmoothZQ1" $ f (Proxy::Proxy '(F12, SmoothZQ1)),
             testProperty "F1/17" $ f (Proxy::Proxy '(F1, Zq 17)),
             testProperty "F2/17" $ f (Proxy::Proxy '(F2, Zq 17)),
             testProperty "F4/17" $ f (Proxy::Proxy '(F4, Zq 17)),
             testProperty "F8/17" $ f (Proxy::Proxy '(F8, Zq 17)),
             testProperty "F21/8191" $ f (Proxy::Proxy '(F21, Zq 8191)),
             testProperty "F42/8191" $ f (Proxy::Proxy '(F42, Zq 8191)),
             --testProperty "F42/Bad" $ f (Proxy::Proxy '(F42, (Zq 150, Zq 343))),
             testProperty "F42/ZQ1" $ f (Proxy::Proxy '(F42, ZQ1)),
             testProperty "F2/ZQ2" $ f (Proxy::Proxy '(F2, ZQ2)),
             testProperty "F3/ZQ2" $ f (Proxy::Proxy '(F3, ZQ2)),
             testProperty "F7/ZQ2" $ f (Proxy::Proxy '(F7, ZQ2)),
             testProperty "F6/ZQ2" $ f (Proxy::Proxy '(F6, ZQ2)),
             testProperty "F42/SmoothZQ3" $ f (Proxy::Proxy '(F42, SmoothZQ3)),
             testProperty "F42/ZQ2" $ f (Proxy::Proxy '(F42, ZQ2)),
             testProperty "F89/179" $ f (Proxy::Proxy '(F89, Zq 179))
             ]

-- we can't include a large modulus here because there is not enough
-- precision in Doubles to handle the error
groupMRExt :: (forall m r . (MRWrapCtx m r) => Proxy '(m, r) -> Property) 
            -> [Test]
groupMRExt f = [testProperty "F7/29" $ f (Proxy::Proxy '(F7, Zq 29)),
             testProperty "F1/17" $ f (Proxy::Proxy '(F1, Zq 17)),
             testProperty "F2/17" $ f (Proxy::Proxy '(F2, Zq 17)),
             testProperty "F4/17" $ f (Proxy::Proxy '(F4, Zq 17)),
             testProperty "F8/17" $ f (Proxy::Proxy '(F8, Zq 17)),
             testProperty "F21/8191" $ f (Proxy::Proxy '(F21, Zq 8191)),
             testProperty "F42/8191" $ f (Proxy::Proxy '(F42, Zq 8191)),
             testProperty "F42/ZQ1" $ f (Proxy::Proxy '(F42, ZQ1)),
             testProperty "F42/ZQ2" $ f (Proxy::Proxy '(F42, ZQ2)),
             testProperty "F89/179" $ f (Proxy::Proxy '(F89, Zq 179))]


type NormCtx t m r = (TElt t r, TElt t (LiftOf r), 
  Fact m, Lift' r, CRTrans r, Eq (LiftOf r),
  ZeroTestable r, Ring (LiftOf r), Ring r, IntegralDomain r)

type NormWrapCtx m r = (NormCtx CT m r, NormCtx RT m r)

-- tests that gSqNormDec of two "random-looking" vectors agrees for RT and CT
prop_gsqnorm :: forall m r . 
  (NormWrapCtx m r) 
  => Proxy m -> r -> Bool
prop_gsqnorm _ x = 
  let crtCT = fromJust crt
      crtRT = fromJust crt
      -- not mathematically meaningful, we just need some "random" coefficients
      ct = fmapT lift (mulGDec $ lInv $ crtCT $ scalarPow x :: CT m r)
      rt = fmapT lift (mulGDec $ lInv $ crtRT $ scalarPow x :: RT m r)
  in gSqNormDec ct == gSqNormDec rt

wrapNorm :: forall m r . (NormWrapCtx m r, Show r, Arbitrary r) => (Proxy m -> r -> Bool) -> Proxy '(m,r) -> Property
wrapNorm f _ = property $ f Proxy

-- these tests all use "good" moduli that lift to Int64
groupNorm :: (forall m r . (NormWrapCtx m r, Show r, Arbitrary r) => Proxy '(m, r) -> Property) 
            -> [Test]
groupNorm f = [testProperty "F7/29" $ f (Proxy::Proxy '(F7, Zq 29)),
               testProperty "F12/SmoothZQ1" $ f (Proxy::Proxy '(F12, SmoothZQ1)),
               testProperty "F1/17" $ f (Proxy::Proxy '(F1, Zq 17)),
               testProperty "F2/17" $ f (Proxy::Proxy '(F2, Zq 17)),
               testProperty "F4/17" $ f (Proxy::Proxy '(F4, Zq 17)),
               testProperty "F8/17" $ f (Proxy::Proxy '(F8, Zq 17)),
               testProperty "F21/8191" $ f (Proxy::Proxy '(F21, Zq 8191)),
               testProperty "F42/8191" $ f (Proxy::Proxy '(F42, Zq 8191)),
               testProperty "F42/ZQ1" $ f (Proxy::Proxy '(F42, ZQ1)),
               testProperty "F89/179" $ f (Proxy::Proxy '(F89, Zq 179))]


type TMM'RCtx t m m' r = (Tensor t, m `Divides` m', TElt t r, Ring r, CRTrans r, Eq r, ZeroTestable r, IntegralDomain r)

-- groups related tests
tremTests = testGroup "Tw.Em == id" [
  testGroup "Pow basis" $ groupTMM'R $ wrapTmm'rToBool prop_trem_pow,
  testGroup "Dec basis" $ groupTMM'R $ wrapTmm'rToBool prop_trem_dec,
  testGroup "CRT basis" $ groupTMM'R $ wrapTmm'rToBool prop_trem_crt]

-- tests that twace . embed == id in the Pow basis
prop_trem_pow :: forall t m m' r . (TMM'RCtx t m m' r)
  => Proxy m' -> t m r -> Bool
prop_trem_pow _ x = (twacePowDec $ (embedPow x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the Dec basis
prop_trem_dec :: forall t m m' r . (TMM'RCtx t m m' r)
  => Proxy m' -> t m r -> Bool
prop_trem_dec _ x = (twacePowDec $ (embedDec x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the CRT basis
prop_trem_crt :: forall t m m' r . (TMM'RCtx t m m' r)
  => Proxy m' -> t m r -> Bool
prop_trem_crt _ x = fromMaybe (error "no CRT in prop_trem_crt") $
  (x==) <$> (twaceCRT <*> (embedCRT <*> pure x :: Maybe (t m' r))) \\ witness entailEqT x

embedCommuteTests = testGroup "Em commutes with L" [
  testGroup "Dec basis" $ groupTMM'R $ wrapTmm'rToBool prop_embed_dec,
  testGroup "CRT basis" $ groupTMM'R $ wrapTmm'rToBool prop_embed_crt]

-- embedDec == lInv . embedPow . l
prop_embed_dec :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> t m r -> Bool
prop_embed_dec _ x = (embedDec x :: t m' r) == (lInv $ embedPow $ l x) 
  \\ proxy entailEqT (Proxy::Proxy (t m' r))

-- embedCRT = crt . embedPow . crtInv
prop_embed_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> t m r -> Bool
prop_embed_crt _ x = fromMaybe (error "no CRT in prop_embed_crt") $ do
  crt' <- crt
  crtInv' <- crtInv
  embedCRT' <- embedCRT
  return $ (embedCRT' x :: t m' r) == (crt' $ embedPow $ crtInv' x) 
    \\ proxy entailEqT (Proxy::Proxy (t m' r))

twaceCommuteTests = testGroup "Tw commutes with L" [
  testGroup "Dec basis" $ groupTMM'R $ wrapTm'mrToBool prop_twace_dec,
  testGroup "CRT basis" $ groupTMM'R $ wrapTm'mrToBool prop_twace_crt]

-- twacePowDec = lInv . twacePowDec . l
prop_twace_dec :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m -> t m' r -> Bool
prop_twace_dec _ x = (twacePowDec x :: t m r) == (lInv $ twacePowDec $ l x)
  \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twaceCRT = crt . twacePowDec . crtInv
prop_twace_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m -> t m' r -> Bool
prop_twace_crt _ x = fromMaybe (error "no CRT in prop_trace_crt") $ do
  twaceCRT' <- twaceCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (twaceCRT' x :: t m r) == (crt' $ twacePowDec $ crtInv' x)
    \\ proxy entailEqT (Proxy::Proxy (t m r))

twaceInvarTests = testGroup "Twace invariants" [
  testGroup "Tw and Em ID for equal indices" $ groupTMR $ wrapTmrToBool $ prop_twEmID,
  testGroup "Invar1 Pow basis" $ groupTMM'R $ wrapProxyTmm'rToBool prop_twace_invar1_pow,
  testGroup "Invar1 Dec basis" $ groupTMM'R $ wrapProxyTmm'rToBool prop_twace_invar1_dec,
  testGroup "Invar1 CRT basis" $ groupTMM'R $ wrapProxyTmm'rToBool prop_twace_invar1_crt,
  testGroup "Invar2 Pow/Dec basis" $ groupTMM'R $ wrapProxyTmm'rToBool prop_twace_invar2_powdec,
  testGroup "Invar2 CRT basis" $ groupTMM'R $ wrapProxyTmm'rToBool prop_twace_invar2_crt
  ]

prop_twEmID :: forall t m r . (Tensor t, TElt t r, CRTrans r, Fact m, m `Divides` m, Eq r, ZeroTestable r, IntegralDomain r) => t m r -> Bool
prop_twEmID x = ((twacePowDec x) == x) &&
                  (((fromMaybe (error "twemid_crt") twaceCRT) x) == x) &&
                  ((embedPow x) == x) &&
                  ((embedDec x) == x) &&
                  (((fromMaybe (error "twemid_crt") embedCRT) x) == x) \\ witness entailEqT x

-- twace mhat'/g' = mhat*totm'/totm/g (Pow basis)
prop_twace_invar1_pow :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> Proxy (t m r) -> Bool
prop_twace_invar1_pow _ _ = fromMaybe (error "could not divide by G in prop_twace_invar1_pow") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGPow $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGPow $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (Dec basis)
prop_twace_invar1_dec :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> Proxy (t m r) -> Bool
prop_twace_invar1_dec _ _ = fromMaybe (error "could not divide by G in prop_twace_invar1_dec") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGDec $ lInv $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGDec $ lInv $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (CRT basis)
prop_twace_invar1_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> Proxy (t m r) -> Bool
prop_twace_invar1_crt _ _ = fromMaybe (error "no CRT in prop_twace_invar1_crt") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  divGCRT1 <- divGCRT
  divGCRT2 <- divGCRT
  twaceCRT' <- twaceCRT
  let output :: t m r = divGCRT1 $ scalarCRT1 $ fromIntegral $ mhat * totm' `div` totm
      input :: t m' r = divGCRT2 $ scalarCRT2 $ fromIntegral mhat'
  return $ (twaceCRT' input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_powdec :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> Proxy (t m r) -> Bool
prop_twace_invar2_powdec _ _ = 
  let output = scalarPow $ one :: t m r
      input = scalarPow $ one :: t m' r
  in (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Proxy m' -> Proxy (t m r) -> Bool
prop_twace_invar2_crt _ _ = fromMaybe (error "no CRT in prop_twace_invar2_crt") $ do
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  let input = scalarCRT1 one :: t m' r
      output = scalarCRT2 one :: t m r
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

type TMM'RWrapCtx t m m' r = (TMM'RCtx t m m' r, Show (t m' r), Show (t m r), Arbitrary (t m' r), Arbitrary (t m r))

wrapProxyTmm'rToBool :: (TMM'RWrapCtx t m m' r)
                        => (Proxy m' -> Proxy (t m r) -> Bool) 
                        -> Proxy t -> Proxy '(m,m',r) -> Property
wrapProxyTmm'rToBool f _ _ = property $ f Proxy Proxy

wrapTmm'rToBool :: (TMM'RWrapCtx t m m' r) => (Proxy m' -> t m r -> Bool) 
                   -> Proxy t -> Proxy '(m,m',r) -> Property
wrapTmm'rToBool f _ _ = property $ f Proxy

wrapTm'mrToBool :: (TMM'RWrapCtx t m m' r) => (Proxy m -> t m' r -> Bool) 
                   -> Proxy t -> Proxy '(m,m',r) -> Property
wrapTm'mrToBool f _ _ = property $ f Proxy

groupTMM'R :: (forall t m m' r . TMM'RWrapCtx t m m' r
               => Proxy t
               -> Proxy '(m,m',r) 
               -> Property) -> [Test]
groupTMM'R f =
  [testGroup "CT" $ groupMM'R (f (Proxy::Proxy CT)),
   testGroup "RT" $ groupMM'R (f (Proxy::Proxy RT))]

type MM'RWrapCtx m m' r = (TMM'RWrapCtx CT m m' r, TMM'RWrapCtx RT m m' r)

groupMM'R :: (forall m m' r . (MM'RWrapCtx m m' r)
              => Proxy '(m, m', r) -> Property) -> [Test]
groupMM'R f = [testProperty "F1/F7/29" $ f (Proxy::Proxy '(F1, F7, Zq 29)),
               testProperty "F4/F12/536871001" $ f (Proxy::Proxy '(F4, F12, Zq 536871001)),
               testProperty "F4/F12/SmoothZQ1" $ f (Proxy::Proxy '(F4, F12, SmoothZQ1)),
               testProperty "F2/F8/17" $ f (Proxy::Proxy '(F2, F8, Zq 17)),
               testProperty "F8/F8/17" $ f (Proxy::Proxy '(F8, F8, Zq 17)),
               testProperty "F12/F12/SmoothZQ1" $ f (Proxy::Proxy '(F2, F8, SmoothZQ1)),
               testProperty "F4/F8/17" $ f (Proxy::Proxy '(F4, F8, Zq 17)),
               testProperty "F3/F21/8191" $ f (Proxy::Proxy '(F3, F21, Zq 8191)),
               testProperty "F7/F21/8191" $ f (Proxy::Proxy '(F7, F21, Zq 8191)),
               testProperty "F3/F42/8191" $ f (Proxy::Proxy '(F3, F42, Zq 8191)),
               testProperty "F3/F21/ZQ1" $ f (Proxy::Proxy '(F3, F21, ZQ1)),
               testProperty "F7/F21/ZQ2" $ f (Proxy::Proxy '(F7, F21, ZQ2)),
               testProperty "F3/F42/ZQ3" $ f (Proxy::Proxy '(F3, F42, ZQ3))]
