{-|
Module      : Crypto.Lol.Cyclotomic.Tensor.CPP.Extension
Description : Embedding/twacing in various bases for CPP.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

CPP Tensor-specific functions for embedding/twacing in various bases.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Crypto.Lol.Cyclotomic.Tensor.CPP.Extension
( embedPow', embedDec', embedCRT'
, twacePowDec', twaceCRT'
, coeffs', powBasisPow'
, crtSetDec'
, backpermute'
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor as T
import Crypto.Lol.Prelude           as LP hiding (lift, null)
import Crypto.Lol.Types.FiniteField
import Crypto.Lol.Types.ZmStar


import Control.Applicative hiding (empty)
import Control.Monad.Trans (lift)

import           Data.Maybe
import           Data.Reflection      (reify)
import qualified Data.Vector          as V
import           Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as U


-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute' :: (Storable a) =>
             U.Vector Int -- ^ @is@ index vector (of length @n@)
             -> Vector a   -- ^ @xs@ value vector
             -> Vector a
{-# INLINABLE backpermute' #-}
backpermute' is v = generate (U.length is) (\i -> v ! (is U.! i))

embedPow', embedDec' :: forall m m' r . (Additive r, Storable r, m `Divides` m')
                     => Tagged '(m, m') (Vector r -> Vector r)
{-# INLINABLE embedPow' #-}
{-# INLINABLE embedDec' #-}
-- | Embeds an vector in the powerful basis of the @m@th cyclotomic
-- ring to a vector in the powerful basis of the @m'@th cyclotomic
-- ring
embedPow' = tag $ (\indices arr -> generate (U.length indices) $ \idx ->
  let (j0,j1) = indices U.! idx
  in if j0 == 0
     then arr ! j1
     else zero) $ baseIndicesPow @m @m'
-- | Same as 'embedPow'', but for the decoding basis.
embedDec' = tag $ (\indices arr -> generate (U.length indices)
  (\idx -> maybe LP.zero
    (\(sh,b) -> if b then negate (arr ! sh) else arr ! sh)
    (indices U.! idx))) $ baseIndicesDec @m @m'

-- | Embeds an vector in the CRT basis of the the mth cyclotomic ring
-- to an vector in the CRT basis of the m'th cyclotomic ring when @m | m'@
embedCRT' :: forall m m' mon r . (CRTrans mon r, Storable r, m `Divides` m')
          => TaggedT '(m, m') mon (Vector r -> Vector r)
embedCRT' =
  (lift (proxyT crtInfo (Proxy::Proxy m') :: mon (CRTInfo r))) >>
  (tagT $ pure $ backpermute' $ baseIndicesCRT @m @m')

-- | maps a vector in the powerful/decoding basis, representing an
-- O_m' element, to a vector of arrays representing O_m elements in
-- the same type of basis
coeffs' :: forall m m' r . (Storable r, m `Divides` m')
        => Tagged '(m, m') (Vector r -> [Vector r])
coeffs' = tag $ flip (\x -> V.toList . V.map (`backpermute'` x))
          $ extIndicesCoeffs @m @m'

-- | The "tweaked trace" function in either the powerful or decoding
-- basis of the m'th cyclotomic ring to the mth cyclotomic ring when
-- @m | m'@.
twacePowDec' :: forall m m' r . (Storable r, m `Divides` m')
             => Tagged '(m, m') (Vector r -> Vector r)
{-# INLINABLE twacePowDec' #-}
twacePowDec' = tag $ backpermute' $ extIndicesPowDec @m @m'

kronToVec :: forall m r . (Fact m, Ring r, Storable r) => Kron r -> Vector r
kronToVec v = generate (totientFact @m) (flip (indexK v) 0)

twaceCRT' :: forall mon m m' r .
             (Storable r, CRTrans mon r, m `Divides` m')
             => TaggedT '(m, m') mon (Vector r -> Vector r)
{-# INLINE twaceCRT' #-}
twaceCRT' = tagT $ do
  g'    <- kronToVec @m' <$> (gCRTK @m')
  gInv  <- kronToVec @m  <$> (gInvCRTK @m)
  embed <- untagT $ embedCRT' @m @m'
  (_, m'hatinv) <- proxyT crtInfo (Proxy::Proxy m')
  let phi = totientFact @m
      phi' = totientFact @m'
      mhat = fromIntegral $ valueHatFact @m
      hatRatioInv = m'hatinv * mhat
      reltot = phi' `div` phi
      -- tweak = mhat * g' / (m'hat * g)
      tweak = SV.map (* hatRatioInv) $ SV.zipWith (*) (embed gInv) g'
      indices = extIndicesCRT @m @m'
  return $ \ arr -> -- take true trace after mul-by-tweak
    let v = backpermute' indices (SV.zipWith (*) tweak arr)
    in generate phi $ \i -> foldl1' (+) $ SV.unsafeSlice (i*reltot) reltot v

-- | The powerful extension basis, wrt the powerful basis.
-- Outputs a list of vectors in O_m' that are an O_m basis for O_m'
powBasisPow' :: forall m m' r . (m `Divides` m', Ring r, SV.Storable r)
                => Tagged '(m, m') [SV.Vector r]
powBasisPow' = do
  let (_, phi, phi', _) = indexInfo @m @m'
      idxs = baseIndicesPow @m @m'
  return $ LP.map (\k -> generate phi' $ \j ->
                           let (j0,j1) = idxs U.! j
                          in if j0==k && j1==0 then one else zero)
    [0..phi' `div` phi - 1]

-- | A list of vectors representing the mod-p CRT set of the
-- extension O_m'/O_m
crtSetDec' :: forall m m' fp .
  (m `Divides` m', PrimeField fp, Coprime (PToF (CharOf fp)) m', SV.Storable fp)
  => Tagged '(m, m') [SV.Vector fp]
crtSetDec' =
  let p = valuePrime @(CharOf fp)
      phi = totientFact @m'
      d = order @m' p
      h :: Int = valueHatFact @m'
      hinv = recip $ fromIntegral h
  in reify d $ \(_::Proxy d) -> do
      let twCRTs' :: Kron (GF fp d)
            = fromMaybe (error "internal error: crtSetDec': twCRTs") $ twCRTs @m'
          zmsToIdx = T.zmsToIndexFact @m'
          elt j i = indexK twCRTs' j (zmsToIdx i)
          trace' = trace :: GF fp d -> fp -- to avoid recomputing powTraces
          cosets = partitionCosets @m @m' p
      return $ LP.map (\is -> generate phi
                          (\j -> hinv * trace'
                                      (LP.sum $ LP.map (elt j) is))) cosets
