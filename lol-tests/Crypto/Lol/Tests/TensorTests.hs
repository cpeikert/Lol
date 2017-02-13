{-|
Module      : Crypto.Lol.Tests.TensorTests
Description : Tests for the 'Tensor' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for the 'Tensor' interface.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.TensorTests where

import Crypto.Lol.Tests
import Crypto.Lol.Utils.ShowType

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor

import Control.Applicative

import Data.Maybe

import qualified Test.Framework as TF

--testGroupM "GSqNormDec"       $ applyLift (Proxy::Proxy NormParams) $ genTestArgs prop_gsqnorm,

-- | Tests for single-index operations. There must be a CRT basis for \(O_m\) over @r@.
tensorTests1 :: forall t m r . _ => Proxy '(m,r) -> Proxy t -> TF.Test
tensorTests1 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,r)
  in testGroup (showType ptmr) $ ($ ptmr) <$> [
      genTestArgs   "fmapT comparison" prop_fmapT,
      genTestArgs   "fmap comparison"  prop_fmap,
      nestGroup  "GInv.G == id" [
        genTestArgs "Pow basis"        prop_ginv_pow,
        genTestArgs "Dec basis"        prop_ginv_dec,
        genTestArgs "CRT basis"        prop_ginv_crt],
      genTestArgs   "CRTInv.CRT == id" prop_crt_inv,
      genTestArgs   "LInv.L == id"     prop_l_inv,
      genTestArgs   "Scalar"           prop_scalar_crt,
      nestGroup  "G commutes with L" [
        genTestArgs "Dec basis"        prop_g_dec,
        genTestArgs "CRT basis"        prop_g_crt],
      genTestArgs "Tw and Em ID for equal indices" prop_twEmID
      ]

-- | Tests for inter-ring operations. There must be a CRT basis for \(O_{m'}\) over @r@.
tensorTests2 :: forall t m m' r . _ => Proxy '(m,m',r) -> Proxy t -> TF.Test
tensorTests2 _ _ =
  let ptmr = Proxy :: Proxy '(t,m,m',r)
  in testGroup (showType ptmr) $ ($ ptmr) <$> [
      nestGroup  "Tw.Em == id" [
        genTestArgs "Pow basis"                      prop_trem_pow,
        genTestArgs "Dec basis"                      prop_trem_dec,
        genTestArgs "CRT basis"                      prop_trem_crt],
      nestGroup  "Em commutes with L" [
        genTestArgs "Dec basis"                      prop_embed_dec,
        genTestArgs "CRT basis"                      prop_embed_crt],
      nestGroup  "Tw commutes with L" [
        genTestArgs "Dec basis"                      prop_twace_dec,
        genTestArgs "CRT basis"                      prop_twace_crt],
      nestGroup  "Twace invariants" [
        genTestArgs "Invar1 Pow basis"               prop_twace_invar1_pow,
        genTestArgs "Invar1 Dec basis"               prop_twace_invar1_dec,
        genTestArgs "Invar1 CRT basis"               prop_twace_invar1_crt,
        genTestArgs "Invar2 Pow/Dec basis"           prop_twace_invar2_powdec,
        genTestArgs "Invar2 CRT basis"               prop_twace_invar2_crt]
      ]

prop_fmapT :: _ => t m r -> Test '(t,m,r)
prop_fmapT x = test $ fmapT id x == x \\ witness entailEqT x \\ witness entailIndexT x

prop_fmap :: _ => t m r -> Test '(t,m,r)
prop_fmap x = test $ (fmap id x) == x \\ witness entailEqT x \\ witness entailIndexT x

-- divG . mulG == id in Pow basis
prop_ginv_pow :: _ => t m r -> Test '(t,m,r)
prop_ginv_pow x = test $ (fromMaybe (error "could not divide by G in prop_ginv_pow") $
  divGPow $ mulGPow x) == x \\ witness entailEqT x

-- divG . mulG == id in Dec basis
prop_ginv_dec :: _ => t m r -> Test '(t,m,r)
prop_ginv_dec x = test $ (fromMaybe (error "could not divide by G in prop_ginv_dec") $
  divGDec $ mulGDec x) == x \\ witness entailEqT x

-- divG . mulG == id in CRT basis
prop_ginv_crt :: _ => t m r -> Test '(t,m,r)
prop_ginv_crt x = test $ fromMaybe (error "no CRT in prop_ginv_crt") $ do
  divGCRT' <- divGCRT
  mulGCRT' <- mulGCRT
  return $ (divGCRT' $ mulGCRT' x) == x \\ witness entailEqT x

-- mulGDec == lInv. mulGPow . l
prop_g_dec :: _ => t m r -> Test '(t,m,r)
prop_g_dec x = test $ (mulGDec x) == (lInv $ mulGPow $ l x) \\ witness entailEqT x

prop_g_crt :: _ => t m r -> Test '(t,m,r)
prop_g_crt x = test $ fromMaybe (error "no CRT in prop_g_crt") $ do
  mulGCRT' <- mulGCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (mulGCRT' x) == (crt' $ mulGPow $ crtInv' x) \\ witness entailEqT x

-- crtInv . crt == id
prop_crt_inv :: _ => t m r -> Test '(t,m,r)
prop_crt_inv x = test $ fromMaybe (error "no CRT in prop_crt_inv") $ do
  crt' <- crt
  crtInv' <- crtInv
  return $ (crtInv' $ crt' x) == x \\ witness entailEqT x

-- lInv . l == id
prop_l_inv :: _ => t m r -> Test '(t,m,r)
prop_l_inv x = test $ (lInv $ l x) == x \\ witness entailEqT x

-- scalarCRT = crt . scalarPow
prop_scalar_crt :: forall t m r . (Tensor t, Fact m, _) => r -> Test '(t,m,r)
prop_scalar_crt r = test $ fromMaybe (error "no CRT in prop_scalar_crt") $ do
  scalarCRT' <- scalarCRT
  crt' <- crt
  return $ (scalarCRT' r :: t m r) == (crt' $ scalarPow r)
  \\ proxy entailEqT (Proxy::Proxy (t m r))

-- tests that twace . embed == id in the Pow basis
prop_trem_pow :: forall t m m' r . (Fact m, Fact m', _) => t m r -> Test '(t,m,m',r)
prop_trem_pow x = test $ (twacePowDec $ (embedPow x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the Dec basis
prop_trem_dec :: forall t m m' r . (Fact m, Fact m', _) => t m r -> Test '(t,m,m',r)
prop_trem_dec x = test $ (twacePowDec $ (embedDec x :: t m' r)) == x \\ witness entailEqT x

-- tests that twace . embed == id in the CRT basis
prop_trem_crt :: forall t m m' r . (Fact m, Fact m', _) => t m r -> Test '(t,m,m',r)
prop_trem_crt x = test $ fromMaybe (error "no CRT in prop_trem_crt") $
  (x==) <$> (twaceCRT <*> (embedCRT <*> pure x :: Maybe (t m' r))) \\ witness entailEqT x

-- embedDec == lInv . embedPow . l
prop_embed_dec :: forall t m m' r . (Fact m, Fact m', _) => t m r -> Test '(t,m,m',r)
prop_embed_dec x = test $ (embedDec x :: t m' r) == (lInv $ embedPow $ l x)
  \\ proxy entailEqT (Proxy::Proxy (t m' r))

-- embedCRT = crt . embedPow . crtInv
prop_embed_crt :: forall t m m' r . (Fact m, Fact m', _) => t m r -> Test '(t,m,m',r)
prop_embed_crt x = test $ fromMaybe (error "no CRT in prop_embed_crt") $ do
  crt' <- crt
  crtInv' <- crtInv
  embedCRT' <- embedCRT
  return $ (embedCRT' x :: t m' r) == (crt' $ embedPow $ crtInv' x)
    \\ proxy entailEqT (Proxy::Proxy (t m' r))

-- twacePowDec = lInv . twacePowDec . l
prop_twace_dec :: forall t m m' r . (Fact m, Fact m', _) => t m' r -> Test '(t,m,m',r)
prop_twace_dec x = test $ (twacePowDec x :: t m r) == (lInv $ twacePowDec $ l x)
  \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twaceCRT = crt . twacePowDec . crtInv
prop_twace_crt :: forall t m m' r . (Fact m, Fact m', _) => t m' r -> Test '(t,m,m',r)
prop_twace_crt x = test $ fromMaybe (error "no CRT in prop_trace_crt") $ do
  twaceCRT' <- twaceCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (twaceCRT' x :: t m r) == (crt' $ twacePowDec $ crtInv' x)
    \\ proxy entailEqT (Proxy::Proxy (t m r))

prop_twEmID :: forall t m r . _ => t m r -> Test '(t,m,r)
prop_twEmID x = test $
  ((twacePowDec x) == x) &&
  (((fromMaybe (error "twemid_crt") twaceCRT) x) == x) &&
  ((embedPow x) == x) &&
  ((embedDec x) == x) &&
  (((fromMaybe (error "twemid_crt") embedCRT) x) == x) \\ witness entailEqT x

-- twace mhat'/g' = mhat*totm'/totm/g (Pow basis)
prop_twace_invar1_pow :: forall t m m' r . _ => Test '(t,m,m',r)
prop_twace_invar1_pow = test $ fromMaybe (error "could not divide by G in prop_twace_invar1_pow") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGPow $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGPow $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (Dec basis)
prop_twace_invar1_dec :: forall t m m' r . _ => Test '(t,m,m',r)
prop_twace_invar1_dec = test $ fromMaybe (error "could not divide by G in prop_twace_invar1_dec") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGDec $ lInv $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGDec $ lInv $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace mhat'/g' = mhat*totm'/totm/g (CRT basis)
prop_twace_invar1_crt :: forall t m m' r . _ => Test '(t,m,m',r)
prop_twace_invar1_crt = test $ fromMaybe (error "no CRT in prop_twace_invar1_crt") $ do
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
prop_twace_invar2_powdec :: forall t m m' r . (Tensor t, Fact m, Fact m', Ring r, _) => Test '(t,m,m',r)
prop_twace_invar2_powdec = test $
  let output = scalarPow $ one :: t m r
      input = scalarPow $ one :: t m' r
  in (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_crt :: forall t m m' r . (Tensor t, Fact m, Fact m', Ring r, _) => Test '(t,m,m',r)
prop_twace_invar2_crt = test $ fromMaybe (error "no CRT in prop_twace_invar2_crt") $ do
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  let input = scalarCRT1 one :: t m' r
      output = scalarCRT2 one :: t m r
  return $ (twacePowDec input) == output \\ proxy entailEqT (Proxy::Proxy (t m r))

{-
-- tests that gSqNormDec of two "random-looking" vectors agrees for RT and CT
-- t is a dummy param
prop_gsqnorm :: forall (t :: Factored -> * -> *) m r . _ => r -> Test '(t,m,r)
prop_gsqnorm x = test $
  let crtCT = fromJust crt
      crtRT = fromJust crt
      -- not mathematically meaningful, we just need some "random" coefficients
      ct = fmapT lift (mulGDec $ lInv $ crtCT $ scalarPow x :: CT m r)
      rt = fmapT lift (mulGDec $ lInv $ crtRT $ scalarPow x :: RT m r)
  in gSqNormDec ct == gSqNormDec rt

-- we can't include a large modulus here because there is not enough
-- precision in Doubles to handle the error
{-type MRExtCombos = '[
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
  ]-}

type MM'RCombos = '[

--type ExtParams = ( '(,) <$> Tensors) <*> MRExtCombos

type NormParams = ( '(,) <$> '[RT]) <*> (Filter Liftable MRCombos)

-}