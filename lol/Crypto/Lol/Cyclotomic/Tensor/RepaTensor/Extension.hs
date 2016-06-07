{-# LANGUAGE BangPatterns, ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude,
             PolyKinds, ScopedTypeVariables, TemplateHaskell,
             TypeFamilies, TypeOperators #-}

-- | RT-specific functions for embedding/twacing in various bases

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.Extension
( twacePowDec', twaceCRT', embedPow', embedDec', embedCRT'
, coeffs', powBasisPow', crtSetDec'
) where

import           Crypto.Lol.CRTrans
import qualified Crypto.Lol.Cyclotomic.Tensor                     as T
import           Crypto.Lol.Cyclotomic.Tensor.RepaTensor.CRT
import           Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon as RT
import           Crypto.Lol.Prelude                               as LP

import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZmStar

import Control.Applicative
import Control.Arrow       (first, second)

import           Data.Coerce
import           Data.Default
import           Data.Maybe
import           Data.Reflection              (reify)
import qualified Data.Vector                  as V
import qualified Data.Vector.Unboxed          as U
import           Data.Vector.Unboxed.Deriving

-- Default instances
instance Default Z where def = Z
instance (Default a, Default b) => Default (a:.b) where def = def:.def

-- derived Unbox instances
derivingUnbox "DIM1"
  [t| (Z:.Int) -> Int |]
  [| \(Z:.i) -> i |]
  [| (Z :.) |]

-- | The "tweaked trace" function in either the powerful or decoding
-- basis of the m'th cyclotomic ring to the mth cyclotomic ring when
-- @m | m'@.
twacePowDec' :: forall m m' r . (m `Divides` m', Unbox r)
                 => Arr m' r -> Arr m r
twacePowDec'
  = let indices = proxy extIndicesPowDec (Proxy::Proxy '(m, m'))
    in coerce $ \ !arr -> force $ backpermute (extent indices) (indices !) arr

-- | The "tweaked trace" function in the CRT
-- basis of the m'th cyclotomic ring to the mth cyclotomic ring when
-- @m | m'@.
twaceCRT' :: forall mon m m' r .
             (m `Divides` m', CRTrans mon r, Unbox r, Elt r)
             => mon (Arr m' r -> Arr m r)
twaceCRT' = do
  g' :: Arr m' r <- gCRT
  gInv <- gInvCRT
  embed :: Arr m r -> Arr m' r <- embedCRT'
  (_, m'hatinv) <- proxyT crtInfo (Proxy::Proxy m')
  let hatRatioInv = m'hatinv * fromIntegral (proxy valueHatFact (Proxy::Proxy m))
      -- tweak = mhat * g' / (m'hat * g)
      tweak = (coerce $ \x -> force . RT.map (* hatRatioInv) . RT.zipWith (*) x) (embed gInv) g' :: Arr m' r
      indices = proxy extIndicesCRT (Proxy::Proxy '(m, m'))
  return $
    -- take true trace after mul-by-tweak
    coerce (\ !arr -> sumS . backpermute (extent indices) (indices !) . RT.zipWith (*) arr) tweak

embedPow', embedDec' :: forall m m' r .
             (m `Divides` m', Unbox r, Additive r)
             => Arr m r -> Arr m' r
-- | Embeds an array in the powerful basis of the the mth cyclotomic ring
-- to an array in the powerful basis of the m'th cyclotomic ring when @m | m'@
embedPow'
  = let indices = proxy baseIndicesPow (Proxy::Proxy '(m, m'))
    in coerce $ \ !arr -> force $ fromFunction (extent indices)
                       (\idx -> let (j0,j1) = (indices ! idx)
                                in if j0 == 0 then arr ! j1 else zero)
-- | Embeds an array in the decoding basis of the the mth cyclotomic ring
-- to an array in the decoding basis of the m'th cyclotomic ring when @m | m'@
embedDec'
  = let indices = proxy baseIndicesDec (Proxy::Proxy '(m, m'))
    in coerce $ \ !arr -> force $
                       fromFunction (extent indices)
                         (\idx -> maybe zero
                                  (\(sh,b) -> if b then negate (arr ! sh)
                                              else arr ! sh)
                                  (indices ! idx))

-- | Embeds an array in the CRT basis of the the mth cyclotomic ring
-- to an array in the CRT basis of the m'th cyclotomic ring when @m | m'@
embedCRT' :: forall mon m m' r . (m `Divides` m', CRTrans mon r, Unbox r)
             => mon (Arr m r -> Arr m' r)
embedCRT' = do
  -- first check existence of CRT transform of index m'
  _ <- proxyT crtInfo (Proxy::Proxy m') :: mon (CRTInfo r)
  let idxs = proxy baseIndicesCRT (Proxy::Proxy '(m,m'))
  return $ coerce $ \ !arr -> (force $ backpermute (extent idxs) (idxs !) arr)

-- | maps an array in the powerful/decoding basis, representing an
-- O_m' element, to an array of arrays representing O_m elements in
-- the same type of basis
coeffs' :: forall m m' r . (m `Divides` m', Unbox r)
             => Arr m' r -> [Arr m r]
coeffs' =
  let indices = proxy extIndicesCoeffs (Proxy::Proxy '(m, m'))
  in coerce $ \ !arr -> V.toList $
  V.map (\idxs -> force $ backpermute (extent idxs) (idxs !) arr) indices

-- | The powerful extension basis, wrt the powerful basis.
-- Outputs a list of arrays in O_m' that are an O_m basis for O_m'
powBasisPow' :: forall m m' r . (m `Divides` m', Ring r, Unbox r)
                => Tagged m [Arr m' r]
powBasisPow' = return $
  let (_, phi, phi', _) = proxy T.indexInfo (Proxy::Proxy '(m,m'))
      idxs = proxy T.baseIndicesPow (Proxy::Proxy '(m,m'))
  in LP.map (\k -> Arr $ force $ fromFunction (Z :. phi')
                         (\(Z:.j) -> let (j0,j1) = idxs U.! j
                                     in if j0==k && j1==0 then one else zero))
      [0..phi' `div` phi - 1]

-- | A list of arrays representing the mod-p CRT set of the
-- extension O_m'/O_m
crtSetDec' :: forall m m' fp .
              (m `Divides` m', PrimeField fp, Coprime (PToF (CharOf fp)) m',
               Unbox fp)
              => Tagged m [Arr m' fp]
crtSetDec' = return $
  let m'p = Proxy :: Proxy m'
      p = proxy valuePrime (Proxy::Proxy (CharOf fp))
      phi = proxy totientFact m'p

      d = proxy (order p) m'p
      h :: Int = proxy valueHatFact m'p
      hinv = recip $ fromIntegral h
  in reify d $ \(_::Proxy d) ->
       let twCRTs' :: T.Matrix (GF fp d)
             = fromMaybe (error "internal error: crtSetDec': twCRTs") $ proxyT T.twCRTs m'p
           zmsToIdx = proxy T.zmsToIndexFact m'p
           elt j i = T.indexM twCRTs' j (zmsToIdx i)
           trace' = trace :: GF fp d -> fp
           cosets = proxy (partitionCosets p) (Proxy::Proxy '(m,m'))
       in LP.map (\is -> Arr $ force $ fromFunction (Z :. phi)
                          (\(Z:.j) -> hinv * trace'
                                      (sum $ LP.map (elt j) is))) cosets


-- convert memoized reindexing Vectors to Arrays, for convenience and speed

extIndicesPowDec :: forall m m' . (m `Divides` m')
                    => Tagged '(m, m') (Array U DIM1 DIM1)
extIndicesPowDec = do
  idxs <- T.extIndicesPowDec
  return $ fromUnboxed (Z :. U.length idxs) $ U.map (Z:.) idxs

extIndicesCRT :: forall m m' . (m `Divides` m')
                 => Tagged '(m, m') (Array U DIM2 DIM1)
extIndicesCRT =
  let phi = proxy totientFact (Proxy::Proxy m)
      phi' = proxy totientFact (Proxy::Proxy m')
  in do
    idxs <- T.extIndicesCRT
    return $ fromUnboxed (Z :. phi :. phi' `div` phi) $ U.map (Z:.) idxs

baseIndicesPow :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (Array U DIM1 (Int,DIM1))

baseIndicesDec :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (Array U DIM1 (Maybe (DIM1, Bool)))

baseIndicesCRT :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (Array U DIM1 DIM1)

baseIndicesPow = do
  idxs <- T.baseIndicesPow
  return $ fromUnboxed (Z :. U.length idxs) $ U.map (second (Z:.)) idxs

baseIndicesDec = do
  idxs <- T.baseIndicesDec
  return $ fromUnboxed (Z :. U.length idxs) $ U.map (liftA (first (Z:.))) idxs

baseIndicesCRT = do
  idxs <- T.baseIndicesCRT
  return $ fromUnboxed (Z :. U.length idxs) $ U.map (Z:.) idxs

extIndicesCoeffs :: forall m m' . (m `Divides` m')
                    => Tagged '(m, m') (V.Vector (Array U DIM1 DIM1))
extIndicesCoeffs =
  V.map (\arr -> fromUnboxed (Z :. U.length arr) $
                 U.map (Z:.) arr) <$> T.extIndicesCoeffs
