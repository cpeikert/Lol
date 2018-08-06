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

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

embedPow', embedDec' :: (Additive r, Storable r, m `Divides` m')
                     => Tagged '(m, m') (Vector r -> Vector r)
{-# INLINABLE embedPow' #-}
{-# INLINABLE embedDec' #-}
-- | Embeds an vector in the powerful basis of the @m@th cyclotomic
-- ring to a vector in the powerful basis of the @m'@th cyclotomic
-- ring
embedPow' = (\indices arr -> generate (U.length indices) $ \idx ->
  let (j0,j1) = indices U.! idx
  in if j0 == 0
     then arr ! j1
     else zero) <$> baseIndicesPow
-- | Same as 'embedPow'', but for the decoding basis.
embedDec' = (\indices arr -> generate (U.length indices)
  (\idx -> maybe LP.zero
    (\(sh,b) -> if b then negate (arr ! sh) else arr ! sh)
    (indices U.! idx))) <$> baseIndicesDec

-- | Embeds an vector in the CRT basis of the the mth cyclotomic ring
-- to an vector in the CRT basis of the m'th cyclotomic ring when @m | m'@
embedCRT' :: forall mon m m' r . (CRTrans mon r, Storable r, m `Divides` m')
          => TaggedT '(m, m') mon (Vector r -> Vector r)
embedCRT' =
  (lift (proxyT crtInfo (Proxy::Proxy m') :: mon (CRTInfo r))) >>
  (pureT $ backpermute' <$> baseIndicesCRT)

-- | maps a vector in the powerful/decoding basis, representing an
-- O_m' element, to a vector of arrays representing O_m elements in
-- the same type of basis
coeffs' :: (Storable r, m `Divides` m')
        => Tagged '(m, m') (Vector r -> [Vector r])
coeffs' = flip (\x -> V.toList . V.map (`backpermute'` x))
          <$> extIndicesCoeffs

-- | The "tweaked trace" function in either the powerful or decoding
-- basis of the m'th cyclotomic ring to the mth cyclotomic ring when
-- @m | m'@.
twacePowDec' :: forall m m' r . (Storable r, m `Divides` m')
             => Tagged '(m, m') (Vector r -> Vector r)
{-# INLINABLE twacePowDec' #-}
twacePowDec' = backpermute' <$> extIndicesPowDec

kronToVec :: forall mon m r . (Monad mon, Fact m, Ring r, Storable r)
  => TaggedT m mon (Kron r) -> TaggedT m mon (Vector r)
kronToVec v = do
  vmat <- v
  let n = proxy totientFact (Proxy::Proxy m)
  return $ generate n (flip (indexK vmat) 0)

twaceCRT' :: forall mon m m' r .
             (Storable r, CRTrans mon r, m `Divides` m')
             => TaggedT '(m, m') mon (Vector r -> Vector r)
{-# INLINE twaceCRT' #-}
twaceCRT' = tagT $ do
  g' <- proxyT (kronToVec gCRTK) (Proxy::Proxy m')
  gInv <- proxyT (kronToVec gInvCRTK) (Proxy::Proxy m)
  embed <- proxyT embedCRT' (Proxy::Proxy '(m,m'))
  indices <- pure $ proxy extIndicesCRT (Proxy::Proxy '(m,m'))
  (_, m'hatinv) <- proxyT crtInfo (Proxy::Proxy m')
  let phi = proxy totientFact (Proxy::Proxy m)
      phi' = proxy totientFact (Proxy::Proxy m')
      mhat = fromIntegral $ proxy valueHatFact (Proxy::Proxy m)
      hatRatioInv = m'hatinv * mhat
      reltot = phi' `div` phi
      -- tweak = mhat * g' / (m'hat * g)
      tweak = SV.map (* hatRatioInv) $ SV.zipWith (*) (embed gInv) g'
  return $ \ arr -> -- take true trace after mul-by-tweak
    let v = backpermute' indices (SV.zipWith (*) tweak arr)
    in generate phi $ \i -> foldl1' (+) $ SV.unsafeSlice (i*reltot) reltot v

-- | The powerful extension basis, wrt the powerful basis.
-- Outputs a list of vectors in O_m' that are an O_m basis for O_m'
powBasisPow' :: forall m m' r . (m `Divides` m', Ring r, SV.Storable r)
                => Tagged '(m, m') [SV.Vector r]
powBasisPow' = do
  (_, phi, phi', _) <- indexInfo
  idxs <- baseIndicesPow
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
  let m'p = Proxy :: Proxy m'
      p = proxy valuePrime (Proxy::Proxy (CharOf fp))
      phi = proxy totientFact m'p
      d = proxy (order p) m'p
      h :: Int = proxy valueHatFact m'p
      hinv = recip $ fromIntegral h
  in reify d $ \(_::Proxy d) -> do
      let twCRTs' :: Kron (GF fp d)
            = fromMaybe (error "internal error: crtSetDec': twCRTs") $ proxyT twCRTs m'p
          zmsToIdx = proxy T.zmsToIndexFact m'p
          elt j i = indexK twCRTs' j (zmsToIdx i)
          trace' = trace :: GF fp d -> fp -- to avoid recomputing powTraces
      cosets <- partitionCosets p
      return $ LP.map (\is -> generate phi
                          (\j -> hinv * trace'
                                      (LP.sum $ LP.map (elt j) is))) cosets
