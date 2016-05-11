{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RebindableSyntax     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TensorTests ( tensorTests )
  where

import Harness.Cyc
import Tests
import Utils

import TestTypes

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor

import Control.Applicative
import Data.Maybe

import Data.Singletons
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar ()

import qualified Test.Framework as TF


tensorTests :: [TF.Test]
tensorTests =
  [testGroupM "fmapT comparison" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_fmapT,
   testGroupM "fmap comparison"  $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_fmap,
   testGroup  "GInv.G == id"       gInvGTests,
   testGroupM "CRTInv.CRT == id" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_crt_inv,
   testGroupM "LInv.L == id"     $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_l_inv,
   testGroupM "Scalar"           $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_scalar_crt,
   testGroup  "G commutes with L"  gCommuteTests,
   testGroupM "GSqNormDec"       $ applyLift (Proxy::Proxy NormParams) $ hideArgs prop_gsqnorm,
   testGroup  "Tw.Em == id"        tremTests,
   testGroup  "Em commutes with L" embedCommuteTests,
   testGroup "Tw commutes with L"  twaceCommuteTests,
   testGroup  "Twace invariants"   twaceInvarTests
   ]

prop_fmapT :: (Tensor t, TElt t r, Fact m, Eq r) => t m r -> Test '(t,m,r)
prop_fmapT x = test $ fmapT id x == x \\ witness entailEqT x \\ witness entailIndexT x

prop_fmap :: (Tensor t, TElt t r, Fact m, Eq r) => t m r -> Test '(t,m,r)
prop_fmap x = test $ (fmap id x) == x \\ witness entailEqT x \\ witness entailIndexT x

-- divG . mulG == id in Pow basis
prop_ginv_pow
    :: (Tensor t, TElt t r, Fact m, Eq r, Ring (TRep t r), ZeroTestable (TRep t r), IntegralDomain (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_ginv_pow x = test $ (fromMaybe (error "could not divide by G in prop_ginv_pow") $
  divGPow $ mulGPow x) == x \\ witness entailEqT x

-- divG . mulG == id in Dec basis
prop_ginv_dec
    :: (Tensor t, TElt t r, Fact m, Eq r, Ring (TRep t r), ZeroTestable (TRep t r), IntegralDomain (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_ginv_dec x = test $ (fromMaybe (error "could not divide by G in prop_ginv_dec") $
  divGDec $ mulGDec x) == x \\ witness entailEqT x

-- divG . mulG == id in CRT basis
prop_ginv_crt
    :: (Tensor t, TElt t r, Fact m, Eq r, CRTrans Maybe (TRep t Int) (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_ginv_crt x = test $ fromMaybe (error "no CRT in prop_ginv_crt") $ do
  divGCRT' <- divGCRT
  mulGCRT' <- mulGCRT
  return $ (divGCRT' $ mulGCRT' x) == x \\ witness entailEqT x

gInvGTests :: [TF.Test]
gInvGTests =
  [
    testGroupM "Pow basis" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_ginv_pow
  , testGroupM "Dec basis" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_ginv_dec
  , testGroupM "CRT basis" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_ginv_crt
  ]

-- mulGDec == lInv. mulGPow . l
prop_g_dec
    :: (Tensor t, Fact m, TElt t r, Eq r, Ring (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_g_dec x = test $ (mulGDec x) == (lInv $ mulGPow $ l x) \\ witness entailEqT x

prop_g_crt
    :: (Tensor t, TElt t r, Fact m, Eq r, CRTrans Maybe (TRep t Int) (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_g_crt x = test $ fromMaybe (error "no CRT in prop_g_crt") $ do
  mulGCRT' <- mulGCRT
  crt'     <- crt
  crtInv'  <- crtInv
  return $ (mulGCRT' x) == (crt' $ mulGPow $ crtInv' x) \\ witness entailEqT x

gCommuteTests :: [TF.Test]
gCommuteTests =
  [ testGroupM "Dec basis" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_g_dec
  , testGroupM "CRT basis" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_g_crt
  ]

-- crtInv . crt == id
prop_crt_inv
    :: (Tensor t, TElt t r, Fact m, Eq r, CRTrans Maybe (TRep t Int) (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_crt_inv x = test $ fromMaybe (error "no CRT in prop_crt_inv") $ do
  crt'    <- crt
  crtInv' <- crtInv
  return $ (crtInv' $ crt' x) == x \\ witness entailEqT x

-- lInv . l == id
prop_l_inv :: (Tensor t, TElt t r, Eq r, Fact m, Ring (TRep t r)) => t m r -> Test '(t,m,r)
prop_l_inv x = test $ (lInv $ l x) == x \\ witness entailEqT x

-- scalarCRT = crt . scalarPow
prop_scalar_crt
    :: forall t m r . (Tensor t, TElt t r, Fact m, Eq r, CRTrans Maybe (TRep t Int) (TRep t r))
    => r
    -> Test '(t,m,r)
prop_scalar_crt r = test $ fromMaybe (error "no CRT in prop_scalar_crt") $ do
  let r' = proxy (constant r) (Proxy :: Proxy t)
  scalarCRT' <- scalarCRT
  crt'       <- crt
  return $ (scalarCRT' r' :: t m r) == (crt' $ scalarPow r')
  \\ proxy entailEqT (Proxy::Proxy (t m r))

type NormCtx t m r = (TElt t r, TElt t (LiftOf r),
  Fact m, Lift' r, CRTrans Maybe Int r, Eq (LiftOf r),
  ZeroTestable r, Ring (LiftOf r), Ring r, IntegralDomain r)

type NormWrapCtx m r = (NormCtx CT m r, NormCtx RT m r)

-- tests that gSqNormDec of two "random-looking" vectors agrees for RT and CT
-- t is a dummy param
prop_gsqnorm
    :: forall t m r . (NormWrapCtx m r, NormCtx t m r)
    => r
    -> Test '(t,m,r)
prop_gsqnorm x = test $
  let crtCT = fromJust crt
      crtRT = fromJust crt
      -- not mathematically meaningful, we just need some "random" coefficients
      ct = fmapT lift (mulGDec $ lInv $ crtCT $ scalarPow x :: CT m r)
      rt = fmapT lift (mulGDec $ lInv $ crtRT $ scalarPow x :: RT m r)
  in gSqNormDec ct == gSqNormDec rt


type TMM'RCtx t m m' r =
  ( Tensor t, m `Divides` m', TElt t r
  , Eq r, Ring r
  , CRTrans Maybe (TRep t Int) (TRep t r)
  , Ring (TRep t r) , ZeroTestable (TRep t r), IntegralDomain (TRep t r)
  )

-- groups related tests
tremTests :: [TF.Test]
tremTests =
  [ testGroupM "Pow basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_trem_pow
  , testGroupM "Dec basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_trem_dec
  , testGroupM "CRT basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_trem_crt
  ]

-- tests that twace . embed == id in the Pow basis
prop_trem_pow
    :: forall t m m' r . (TMM'RCtx t m m' r)
    => t m r
    -> Test '(t,m,m',r)
prop_trem_pow x = test $ (twacePowDec $ (embedPow x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the Dec basis
prop_trem_dec
    :: forall t m m' r . (TMM'RCtx t m m' r)
    => t m r
    -> Test '(t,m,m',r)
prop_trem_dec x = test $ (twacePowDec $ (embedDec x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the CRT basis
prop_trem_crt
    :: forall t m m' r . (TMM'RCtx t m m' r)
    => t m r
    -> Test '(t,m,m',r)
prop_trem_crt x = test $ fromMaybe (error "no CRT in prop_trem_crt") $
  (x==) <$> (twaceCRT <*> (embedCRT <*> pure x :: Maybe (t m' r))) \\ witness entailEqT x



embedCommuteTests :: [TF.Test]
embedCommuteTests =
  [ testGroupM "Dec basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_embed_dec
  , testGroupM "CRT basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_embed_crt
  ]

-- embedDec == lInv . embedPow . l
prop_embed_dec :: forall t m m' r . (TMM'RCtx t m m' r) => t m r -> Test '(t,m,m',r)
prop_embed_dec x = test $ (embedDec x :: t m' r) == (lInv $ embedPow $ l x)
  \\ proxy entailEqT (Proxy::Proxy (t m' r))

-- embedCRT = crt . embedPow . crtInv
prop_embed_crt :: forall t m m' r . (TMM'RCtx t m m' r) => t m r -> Test '(t,m,m',r)
prop_embed_crt x = test $ fromMaybe (error "no CRT in prop_embed_crt") $ do
  crt'      <- crt
  crtInv'   <- crtInv
  embedCRT' <- embedCRT
  return $ (embedCRT' x :: t m' r) == (crt' $ embedPow $ crtInv' x)
    \\ proxy entailEqT (Proxy::Proxy (t m' r))

twaceCommuteTests :: [TF.Test]
twaceCommuteTests =
  [ testGroupM "Dec basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_dec
  , testGroupM "CRT basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_crt
  ]

-- twacePowDec = lInv . twacePowDec . l
prop_twace_dec :: forall t m m' r . (TMM'RCtx t m m' r) => t m' r -> Test '(t,m,m',r)
prop_twace_dec x = test $ (twacePowDec x :: t m r) == (lInv $ twacePowDec $ l x)
  \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twaceCRT = crt . twacePowDec . crtInv
prop_twace_crt :: forall t m m' r . (TMM'RCtx t m m' r) => t m' r -> Test '(t,m,m',r)
prop_twace_crt x = test $ fromMaybe (error "no CRT in prop_trace_crt") $ do
  twaceCRT' <- twaceCRT
  crt'      <- crt
  crtInv'   <- crtInv
  return $ (twaceCRT' x :: t m r) == (crt' $ twacePowDec $ crtInv' x)
    \\ proxy entailEqT (Proxy::Proxy (t m r))

twaceInvarTests :: [TF.Test]
twaceInvarTests =
  [ testGroupM "Tw and Em ID for equal indices" $ applyBasic (Proxy::Proxy TMRParams) $ hideArgs prop_twEmID
  , testGroupM "Invar1 Pow basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_invar1_pow
  , testGroupM "Invar1 Dec basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_invar1_dec
  , testGroupM "Invar1 CRT basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_invar1_crt
  , testGroupM "Invar2 Pow/Dec basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_invar2_powdec
  , testGroupM "Invar2 CRT basis" $ applyTwoIdx (Proxy::Proxy TrEmParams) $ hideArgs prop_twace_invar2_crt
  ]

prop_twEmID
    :: ( Tensor t, TElt t r, Fact m, m `Divides` m, Eq r
       , CRTrans Maybe (TRep t Int) (TRep t r), ZeroTestable (TRep t r), IntegralDomain (TRep t r))
    => t m r
    -> Test '(t,m,r)
prop_twEmID x = test $
  ((twacePowDec x) == x) &&
  (((fromMaybe (error "twemid_crt") twaceCRT) x) == x) &&
  ((embedPow x) == x) &&
  ((embedDec x) == x) &&
  (((fromMaybe (error "twemid_crt") embedCRT) x) == x) \\ witness entailEqT x

-- twace mhat'/g' = mhat*totm'/totm/g (Pow basis)
prop_twace_invar1_pow :: forall t m m' r . (TMM'RCtx t m m' r) => Test '(t,m,m',r)
prop_twace_invar1_pow = test $ fromMaybe (error "could not divide by G in prop_twace_invar1_pow") $ do
  let mhat  = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm  = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m  r <- divGPow $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input  :: t m' r <- divGPow $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (Dec basis)
prop_twace_invar1_dec :: forall t m m' r . (TMM'RCtx t m m' r) => Test '(t,m,m',r)
prop_twace_invar1_dec = test $ fromMaybe (error "could not divide by G in prop_twace_invar1_dec") $ do
  let mhat  = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm  = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m  r <- divGDec $ lInv $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input  :: t m' r <- divGDec $ lInv $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (CRT basis)
prop_twace_invar1_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Test '(t,m,m',r)
prop_twace_invar1_crt = test $ fromMaybe (error "no CRT in prop_twace_invar1_crt") $ do
  let mhat  = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm  = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  divGCRT1   <- divGCRT
  divGCRT2   <- divGCRT
  twaceCRT'  <- twaceCRT
  let output :: t m  r = divGCRT1 $ scalarCRT1 $ fromIntegral $ mhat * totm' `div` totm
      input  :: t m' r = divGCRT2 $ scalarCRT2 $ fromIntegral mhat'
  return $ (twaceCRT' input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_powdec :: forall t m m' r . (TMM'RCtx t m m' r) => Test '(t,m,m',r)
prop_twace_invar2_powdec = test $
  let output = scalarPow $ one :: t m r
      input  = scalarPow $ one :: t m' r
  in (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_crt :: forall t m m' r . (TMM'RCtx t m m' r) => Test '(t,m,m',r)
prop_twace_invar2_crt = test $ fromMaybe (error "no CRT in prop_twace_invar2_crt") $ do
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  let input  = scalarCRT1 one :: t m' r
      output = scalarCRT2 one :: t m  r
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))





type Tensors  = '[CT,RT]
type MRCombos = '[
  '(F7, Zq 29),
  '(F12, SmoothZQ1),
  '(F1, Zq 17),
  '(F2, Zq 17),
  '(F4, Zq 17),
  '(F8, Zq 17),
  '(F21, Zq 8191),
  '(F42, Zq 8191),
  '(F42, ZQ1),
  '(F2, ZQ2),
  '(F3, ZQ2),
  '(F7, ZQ2),
  '(F6, ZQ2),
  '(F42, SmoothZQ3),
  '(F42, ZQ2),
  '(F89, Zq 179)
  ]

-- we can't include a large modulus here because there is not enough
-- precision in Doubles to handle the error
type MRExtCombos = '[
  '(F7, Zq 29),
  '(F1, Zq 17),
  '(F2, Zq 17),
  '(F4, Zq 17),
  '(F8, Zq 17),
  '(F21, Zq 8191),
  '(F42, Zq 8191),
  '(F42, ZQ1),
  '(F42, ZQ2),
  '(F89, Zq 179)
  ]

type MM'RCombos = '[
  '(F1, F7, Zq 29),
  '(F4, F12, Zq 536871001),
  '(F4, F12, SmoothZQ1),
  '(F2, F8, Zq 17),
  '(F8, F8, Zq 17),
  '(F2, F8, SmoothZQ1),
  '(F4, F8, Zq 17),
  '(F3, F21, Zq 8191),
  '(F7, F21, Zq 8191),
  '(F3, F42, Zq 8191),
  '(F3, F21, ZQ1),
  '(F7, F21, ZQ2),
  '(F3, F42, ZQ3)
  ]

type TMRParams  = ( '(,) <$> Tensors) <*> MRCombos
type ExtParams  = ( '(,) <$> Tensors) <*> MRExtCombos
type TrEmParams = ( '(,) <$> Tensors) <*> MM'RCombos
type NormParams = ( '(,) <$> '[RT]) <*> (Filter Liftable MRCombos)

data Liftable :: TyFun (Factored, *) Bool -> *
type instance Apply Liftable '(m,zq) = Int64 :== (LiftOf zq)

