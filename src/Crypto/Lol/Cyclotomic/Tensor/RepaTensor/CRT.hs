{-# LANGUAGE ConstraintKinds, FlexibleContexts, GADTs, NoImplicitPrelude,
             ScopedTypeVariables #-}

-- | Functions to support the chinese remainder transform on Repa arrays

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.CRT
( scalarCRT'
, fCRT, fCRTInv
, mulGCRT', divGCRT'
, gCRT, gInvCRT
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon as RT
import Crypto.Lol.LatticePrelude                        as LP

import Control.Applicative
import Data.Coerce
import Data.Singletons.Prelude

-- | Embeds a scalar into the CRT basis (when it exists).
scalarCRT' :: forall m r . (Fact m, CRTrans r, Unbox r)
              => Maybe (r -> Arr m r)
{-# INLINABLE scalarCRT' #-}
scalarCRT'
  = let pps = proxy ppsFact (Proxy::Proxy m)
        sz = Z :. totientPPs pps
    in pure $ Arr . force . fromFunction sz . const

-- | Multiply by @g_m@ in the CRT basis (when it exists).
mulGCRT' :: forall m r . (Fact m, CRTrans r, Unbox r, Elt r)
            => Maybe (Arr m r -> Arr  m r)
{-# INLINABLE mulGCRT' #-}
mulGCRT' = (coerce (\x -> force . RT.zipWith (*) x) `asTypeOf` asTypeOf) <$> gCRT

-- | Divide by @g@ in the CRT basis (when it exists).
divGCRT' :: (Fact m, CRTrans r, IntegralDomain r, ZeroTestable r,
             Unbox r, Elt r) => Maybe (Arr m r -> Arr m r)
{-# INLINABLE divGCRT' #-}
divGCRT' =  (coerce (\x -> force . RT.zipWith (*) x) `asTypeOf` asTypeOf) <$> gInvCRT

-- | The representation of @g@ in the CRT basis (when it exists).
gCRT :: (Fact m, CRTrans r, Unbox r, Elt r) => Maybe (Arr m r)
{-# INLINABLE gCRT #-}
gCRT = fCRT <*> pure (fGPow $ scalarPow' LP.one)

-- | The representation of @g^{ -1 }@ in the CRT basis (when it exists).
gInvCRT:: (Fact m, CRTrans r, IntegralDomain r,
           ZeroTestable r, Unbox r, Elt r)
          => Maybe (Arr m r)
{-# INLINABLE gInvCRT #-}
gInvCRT = fCRT <*> fGInvPow (scalarPow' LP.one)

fCRT, fCRTInv ::
  forall m r . (Fact m, CRTrans r, Unbox r, Elt r)
  => Maybe (Arr m r -> Arr m r)

{-# INLINABLE fCRT #-}
{-# INLINABLE fCRTInv #-}

-- | The chinese remainder transform.
-- Exists if and only if crt exists for all prime powers.
fCRT = evalM $ fTensor ppCRT

-- divide by mhat after doing crtInv'
-- | The inverse chinese remainder transform.
-- Exists if and only if crt exists for all prime powers.
fCRTInv = do -- in Maybe
  (_, mhatInv) :: (CRTInfo r) <- proxyT crtInfoFact (Proxy :: Proxy m)
  let totm = proxy totientFact (Proxy :: Proxy m)
      divMhat = trans totm $ RT.map (*mhatInv)
  evalM $ (divMhat .*) <$> fTensor ppCRTInv'

ppDFT, ppDFTInv', ppCRT, ppCRTInv' ::
  forall pp r . (PPow pp, CRTrans r, Unbox r, Elt r)
  => TaggedT pp Maybe (Trans r)

{-# INLINABLE ppDFT #-}
{-# INLINABLE ppDFTInv' #-}
{-# INLINABLE ppCRT #-}
{-# INLINABLE ppCRTInv' #-}

ppDFT = case (sing :: SPrimePower pp) of
  (SPP (STuple2 sp SO)) -> tagT $ withWitnessT pDFT sp
  spp@(SPP (STuple2 sp (SS se'))) ->
    tagT $ do
      let spp' = SPP (STuple2 sp se')
      pp'dft <- withWitnessT ppDFT spp'
      pptwid <- withWitnessT (ppTwid False) spp
      pdft <- withWitnessT pDFT sp
      return $ (pp'dft @* Id (dim pdft)) .* pptwid .* (Id (dim pp'dft) @* pdft)

ppDFTInv' = case (sing :: SPrimePower pp) of
  (SPP (STuple2 sp SO)) -> tagT $ withWitnessT pDFTInv' sp
  spp@(SPP (STuple2 sp (SS se'))) ->
    tagT $ do
      let spp' = SPP (STuple2 sp se')
      pp'dftInv' <- withWitnessT ppDFTInv' spp'
      pptwidInv <- withWitnessT (ppTwid True) spp
      pdftInv' <- withWitnessT pDFTInv' sp
      return $ (Id (dim pp'dftInv') @* pdftInv') .* pptwidInv .*
                 (pp'dftInv' @* Id (dim pdftInv'))

ppCRT = case (sing :: SPrimePower pp) of
  (SPP (STuple2 sp SO)) -> tagT $ withWitnessT pCRT sp
  spp@(SPP (STuple2 sp (SS se'))) ->
    tagT $ do
      let spp' = SPP (STuple2 sp se')
      pp'dft <- withWitnessT ppDFT spp'
      pptwid <- withWitnessT (ppTwidHat False) spp
      pcrt <- withWitnessT pCRT sp
      return $
        (pp'dft @* Id (dim pcrt)) .* pptwid .*
        -- save some work when p=2
        (if dim pcrt > 1 then Id (dim pp'dft) @* pcrt else Id (dim pp'dft))

ppCRTInv' = case (sing :: SPrimePower pp) of
  (SPP (STuple2 sp SO)) -> tagT $ withWitnessT pCRTInv' sp
  spp@(SPP (STuple2 sp (SS se'))) ->
    tagT $ do
      let spp' = SPP (STuple2 sp se')
      pp'dftInv' <- withWitnessT ppDFTInv' spp'
      pptwidInv <- withWitnessT (ppTwidHat True) spp
      pcrtInv' <- withWitnessT pCRTInv' sp
      return $
        (Id (dim pp'dftInv') @* pcrtInv') .* pptwidInv .*
        (pp'dftInv' @* Id (dim pcrtInv'))

butterfly :: (Additive r, Unbox r, Elt r) => Trans r
butterfly = trans 2 $ \arr ->
            fromFunction (extent arr) $
                             \(sh:.j) -> case j of
                                           0 -> arr ! (sh:.0) +
                                                arr ! (sh:.1)
                                           1 -> arr ! (sh:.0) -
                                                arr ! (sh:.1)

-- DFT_p, CRT_p, scaled DFT_p^{ -1 } and CRT_p^{ -1 }
pDFT, pDFTInv', pCRT, pCRTInv' ::
  forall p r . (Prim p, CRTrans r, Unbox r, Elt r)
  => TaggedT p Maybe (Trans r)

{-# INLINABLE pDFT #-}
{-# INLINABLE pDFTInv' #-}
{-# INLINABLE pCRT #-}
{-# INLINABLE pCRTInv' #-}

pDFT = let pval = proxy valuePrime (Proxy::Proxy p)
       in if pval == 2
          then return butterfly
          else do (omegaPPow, _) <- crtInfoPrime
                  return $ trans pval $ mulMat $ force $
                         fromFunction (Z :. pval :. pval)
                                          (\(Z:.i:.j) -> omegaPPow (i*j))

pDFTInv' = let pval = proxy valuePrime (Proxy::Proxy p)
           in if pval == 2
              then return butterfly
              else do (omegaPPow, _) <- crtInfoPrime
                      return $ trans pval $ mulMat $ force $
                             fromFunction (Z :. pval :. pval)
                                              (\(Z:.i:.j) -> omegaPPow (-i*j))

pCRT = let pval = proxy valuePrime (Proxy::Proxy p)
       in if pval == 2
          then return $ Id 1
          else do (omegaPPow, _) <- crtInfoPrime
                  return $ trans (pval-1) $ mulMat $ force $
                         fromFunction (Z :. pval-1 :. pval-1)
                                          (\(Z:.i:.j) -> omegaPPow ((i+1)*j))

-- crt_p * this = \hat{p}*I, for all prime p.
pCRTInv' =
  let pval = proxy valuePrime (Proxy::Proxy p)
  in if pval == 2 then return $ Id 1
     else do
       (omegaPPow, _) <- crtInfoPrime
       return $ trans (pval-1) $  mulMat $ force $
              fromFunction (Z :. pval-1 :. pval-1)
                               (\(Z:.i:.j) -> omegaPPow (negate i*(j+1)) -
                                              omegaPPow (j+1))

-- twiddle factors for DFT_pp and CRT_pp decompositions
ppTwid, ppTwidHat ::
  forall pp r . (PPow pp, CRTrans r, Unbox r, Elt r)
  => Bool -> TaggedT pp Maybe (Trans r)

{-# INLINABLE ppTwid #-}
{-# INLINABLE ppTwidHat #-}

ppTwid inv =
  let pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      ppval = valuePP pp
  in do
    (omegaPPPow, _) <- crtInfoPPow
    return $ trans ppval $ mulDiag $ force $
                           fromFunction (Z :. ppval)
                           (\(Z:.i) -> let (iq,ir) = i `divMod` p
                                           pow = (if inv then negate else id)
                                                 ir * digitRev (p,e-1) iq
                                       in omegaPPPow pow)

ppTwidHat inv =
  let pp@(p,e) = proxy ppPPow (Proxy :: Proxy pp)
      pptot = totientPP pp
  in do
    (omegaPPPow, _) <- crtInfoPPow
    return $ trans pptot $ mulDiag $ force $
                           fromFunction (Z :. pptot)
                           (\(Z:.i) -> let (iq,ir) = i `divMod` (p-1)
                                           pow = (if inv then negate else id)
                                                 (ir+1) * digitRev (p,e-1) iq
                                       in omegaPPPow pow)
