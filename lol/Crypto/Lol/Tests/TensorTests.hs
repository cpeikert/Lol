{-|
Module      : Crypto.Lol.Tests.TensorTests
Description : Tests for the 'Tensor' interface.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Tests for the 'Tensor' interface.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Crypto.Lol.Tests.TensorTests (tensorCrtTests1, tensorCrtTests2, tensorTests1, tensorTests2) where

import Crypto.Lol
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Utils.ShowType
import Crypto.Lol.Utils.Tests (nestGroup, testGroup, testWithGen, testWithoutGen,
                               (=~=), ApproxEqual, Gen, Test)

import Control.Applicative
import Data.Maybe

-- TODO: We don't test:
--         * TensorPowDec::coeffs,
--         * TensorPowDec::powBasisPow,
--         * TensorGSqNorm::gSqNormDec

-- Has to take two generators because prop_scalar_crt only takes ring elements as input
tensorTests1 :: forall t m r . _ => Gen (t m r) -> Test
tensorTests1 tensorGen =
  let ptmr  = Proxy::Proxy '(t,m,r)
      tests = ($ tensorGen) <$> [
        testWithGen   "fmap comparison"                           prop_fmap,
        nestGroup     "GInv.G == id" [
          testWithGen "Pow basis"                                 prop_ginv_pow,
          testWithGen "Dec basis"                                 prop_ginv_dec],
        testWithGen   "DecToPowToDec == id"                       prop_decToPowToDec,
        testWithGen   "G commutes with L on Dec basis"            prop_g_dec,
        testWithGen   "Tw and Em ID on Pow/Dec for equal indices" prop_twEmID] in
  testGroup (showType ptmr) tests

tensorCrtTests1 :: forall t m r . _ => Gen r -> Gen (t m r) -> Test
tensorCrtTests1 ringGen tensorGen =
  let ptmr = Proxy::Proxy '(t,m,r)
      tests = [
          testWithGen "GInv.G == id on CRT basis"             prop_ginv_crt tensorGen,
          testWithGen "CRTInv.CRT == id"                      prop_crt_inv tensorGen,
          testWithGen "G commutes with L on CRT basis"        prop_g_crt tensorGen,
          testWithGen "Tw and Em ID on CRT for equal indices" prop_twEmIDCRT tensorGen,
          testWithGen "Scalar"                               (prop_scalar_crt ptmr) ringGen] in
  testGroup (showType ptmr) tests

tensorTests2 :: forall t m m' r . _
             => Proxy '(t,m,m',r) -> Gen (t m r) -> Test
tensorTests2 _ tensorGen =
  let ptmmr  = Proxy::Proxy '(t,m,m',r)
      randTests  = ($ tensorGen) <$> [
        nestGroup  "Tw.Em == id" [
          testWithGen "Pow basis"                       (prop_trem_pow ptmmr)
        , testWithGen "Dec basis"                       (prop_trem_dec ptmmr)
          ],
        testWithGen   "Em commutes with L in Dec basis" (prop_embed_dec ptmmr),
        testWithGen   "Tw commutes with L in Dec basis" (prop_twace_dec ptmmr)]
      deterministicTests = [
        testGroup  "Twace invariants" [
          testWithoutGen "Invar1 Pow basis"             (prop_twace_invar1_pow ptmmr),
          testWithoutGen "Invar1 Dec basis"             (prop_twace_invar1_dec ptmmr),
          testWithoutGen "Invar2 Pow/Dec basis"         (prop_twace_invar2_powdec ptmmr)]] in
      testGroup (showType ptmmr) (randTests ++ deterministicTests)

tensorCrtTests2 :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Gen (t m r) -> Test
tensorCrtTests2 _ tensorGen =
  let ptmmr  = Proxy::Proxy '(t,m,m',r)
      randTests  = ($ tensorGen) <$> [
        testWithGen "Tw.Em == id in CRT basis"        (prop_trem_crt ptmmr),
        testWithGen "Em commutes with L in CRT basis" (prop_embed_crt ptmmr),
        testWithGen "Tw commutes with L in CRT basis" (prop_twace_crt ptmmr)]
      deterministicTests = [
        testGroup "Twace invariants" [
          testWithoutGen "Invar1 CRT basis"           (prop_twace_invar1_crt ptmmr),
          testWithoutGen "Invar2 CRT basis"           (prop_twace_invar2_crt ptmmr)]] in
      testGroup (showType ptmmr) (randTests ++ deterministicTests)

prop_fmap :: _ => t m r -> Bool
prop_fmap x = (fmap id x) =~= x

-- divG . mulG == id in Pow basis
prop_ginv_pow :: _ => t m r -> Bool
prop_ginv_pow x = (fromMaybe (error "could not divide by G in prop_ginv_pow") $
  divGPow $ mulGPow x) =~= x

-- divG . mulG == id in Dec basis
prop_ginv_dec :: _ => t m r -> Bool
prop_ginv_dec x = (fromMaybe (error "could not divide by G in prop_ginv_dec") $
  divGDec $ mulGDec x) =~= x

-- divG . mulG == id in CRT basis
prop_ginv_crt :: _ => t m r -> Bool
prop_ginv_crt x = fromMaybe (error "no CRT in prop_ginv_crt") $ do
  divGCRT' <- divGCRT
  mulGCRT' <- mulGCRT
  return $ (divGCRT' $ mulGCRT' x) =~= x

-- mulGDec == powToDec . mulGPow . decToPow
prop_g_dec :: _ => t m r -> Bool
prop_g_dec x = (mulGDec x) =~= (powToDec $ mulGPow $ decToPow x)

prop_g_crt :: _ => t m r -> Bool
prop_g_crt x = fromMaybe (error "no CRT in prop_g_crt") $ do
  mulGCRT' <- mulGCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (mulGCRT' x) =~= (crt' $ mulGPow $ crtInv' x)

-- crtInv . crt == id
prop_crt_inv :: _ => t m r -> Bool
prop_crt_inv x = fromMaybe (error "no CRT in prop_crt_inv") $ do
  crt' <- crt
  crtInv' <- crtInv
  return $ (crtInv' $ crt' x) =~= x

-- powToDec . decToPowToDec == id
prop_decToPowToDec :: _ => t m r -> Bool
prop_decToPowToDec x = (powToDec $ decToPow x) =~= x

-- scalarCRT = crt . scalarPow
-- This only requires Proxy '(t,m) to be fully determined, but this works too
prop_scalar_crt :: forall t m r . (TensorPowDec t r, Fact m, _) => Proxy '(t,m,r) -> r -> Bool
prop_scalar_crt _ x = fromMaybe (error "no CRT in prop_scalar_crt") $ do
  scalarCRT' <- scalarCRT
  crt' <- crt
  return $ (scalarCRT' x :: t m r) =~= (crt' $ scalarPow x)

-- tests that twace . embed == id in the Pow basis
prop_trem_pow :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> t m r -> Bool
prop_trem_pow _ x = (twacePowDec $ (embedPow x :: t m' r)) =~= x

-- tests that twace . embed == id in the Dec basis
prop_trem_dec :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> t m r -> Bool
prop_trem_dec _ x = (twacePowDec $ (embedDec x :: t m' r)) =~= x

-- tests that twace . embed == id in the CRT basis
prop_trem_crt :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> t m r -> Bool
prop_trem_crt _ x = fromMaybe (error "no CRT in prop_trem_crt") $
  (x=~=) <$> (twaceCRT <*> (embedCRT <*> pure x :: Maybe (t m' r)))

-- embedDec == powToDec . embedPow . decToPow
prop_embed_dec :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> t m r -> Bool
prop_embed_dec _ x = (embedDec x :: t m' r) =~=
                     (powToDec $ embedPow $ decToPow x)

-- embedCRT = crt . embedPow . crtInv
prop_embed_crt :: forall t m m' r . (m `Divides` m', _) => Proxy '(t,m,m',r) -> t m r -> Bool
prop_embed_crt _ x = fromMaybe (error "no CRT in prop_embed_crt") $ do
  crt' <- crt
  crtInv' <- crtInv
  embedCRT' <- embedCRT
  return $ (embedCRT' x :: t m' r) =~= (crt' $ embedPow $ crtInv' x)

-- twacePowDec = powToDec . twacePowDec . decToPow
prop_twace_dec :: forall t m m' r . _ => Proxy '(t,m,m',r) -> t m r -> Bool
prop_twace_dec _ x = (twacePowDec x :: t m r) =~=
                     (powToDec $ twacePowDec $ decToPow x)

-- twaceCRT = crt . twacePowDec . crtInv
prop_twace_crt :: forall t m m' r . _ => Proxy '(t,m,m',r) -> t m r -> Bool
prop_twace_crt _ x = fromMaybe (error "no CRT in prop_trace_crt") $ do
  twaceCRT' <- twaceCRT
  crt' <- crt
  crtInv' <- crtInv
  return $ (twaceCRT' x :: t m r) =~= (crt' $ twacePowDec $ crtInv' x)

prop_twEmIDCRT :: forall t m r . _ => t m r -> Bool
prop_twEmIDCRT x = (((fromMaybe (error "twemid_crt") twaceCRT) x) =~= x) &&
                   (((fromMaybe (error "twemid_crt") embedCRT) x) =~= x)

prop_twEmID :: forall t m r . _ => t m r -> Bool
prop_twEmID x = ((twacePowDec x) =~= x) &&
                ((embedPow x) =~= x)

-- twace mhat'/g' = mhat*totm'/totm/g (Pow basis)
prop_twace_invar1_pow :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Bool
prop_twace_invar1_pow _ = fromMaybe (error "could not divide by G in prop_twace_invar1_pow") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGPow $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGPow $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) =~= output

-- twace mhat'/g' = mhat*totm'/totm/g (Dec basis)
prop_twace_invar1_dec :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Bool
prop_twace_invar1_dec _ = fromMaybe (error "could not divide by G in prop_twace_invar1_dec") $ do
  let mhat = proxy valueHatFact (Proxy::Proxy m)
      mhat' = proxy valueHatFact (Proxy::Proxy m')
      totm = proxy totientFact (Proxy::Proxy m)
      totm' = proxy totientFact (Proxy::Proxy m')
  output :: t m r <- divGDec $ powToDec $ scalarPow $ fromIntegral $ mhat * totm' `div` totm
  input :: t m' r <- divGDec $ powToDec $ scalarPow $ fromIntegral mhat'
  return $ (twacePowDec input) =~= output

-- twace mhat'/g' = mhat*totm'/totm/g (CRT basis)
prop_twace_invar1_crt :: forall t m m' r . _ => Proxy '(t,m,m',r) -> Bool
prop_twace_invar1_crt _ = fromMaybe (error "no CRT in prop_twace_invar1_crt") $ do
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
  return $ (twaceCRT' input) =~= output

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_powdec :: forall t m m' r . (TensorPowDec t r, m `Divides` m', _)
                         => Proxy '(t,m,m',r) -> Bool
prop_twace_invar2_powdec _ =
  let output = scalarPow $ one :: t m r
      input = scalarPow $ one :: t m' r
  in (twacePowDec input) =~= output

-- twace preserves scalars in Pow/Dec basis
prop_twace_invar2_crt :: forall t m m' r . (TensorPowDec t r, m `Divides` m', _)
                      => Proxy '(t,m,m',r) -> Bool
prop_twace_invar2_crt _ = fromMaybe (error "no CRT in prop_twace_invar2_crt") $ do
  scalarCRT1 <- scalarCRT
  scalarCRT2 <- scalarCRT
  let input = scalarCRT1 one :: t m' r
      output = scalarCRT2 one :: t m r
  return $ (twacePowDec input) =~= output

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
