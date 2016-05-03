{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies,
             TypeOperators #-}

-- | CT-specific functions for embedding/twacing in various bases

module Crypto.Lol.Cyclotomic.Tensor.CTensor.Extension
( embedPow', embedDec', embedCRT'
, twacePowDec' -- , twaceCRT'
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
import           Data.Vector.Generic  as G (Vector, generate, length, (!))
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed  as U


-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@ but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute' :: (Vector v a)
             => U.Vector Int -- ^ @is@ index vector (of length @n@)
             -> v a   -- ^ @xs@ value vector
             -> v a
--{-# INLINE backpermute' #-}
backpermute' is v = generate (G.length is) (\i -> v ! (is ! i))

embedPow', embedDec' :: (Additive r, Vector v r, m `Divides` m')
                     => Tagged '(m, m') (v r -> v r)
-- | Embeds an vector in the powerful basis of the the mth cyclotomic ring
-- to an vector in the powerful basis of the m'th cyclotomic ring when @m | m'@
embedPow' = (\indices arr -> generate (U.length indices) $ \idx ->
  let (j0,j1) = indices ! idx
  in if j0 == 0
     then arr ! j1
     else zero) <$> baseIndicesPow
-- | Embeds an vector in the decoding basis of the the mth cyclotomic ring
-- to an vector in the decoding basis of the m'th cyclotomic ring when @m | m'@
embedDec' = (\indices arr -> generate (U.length indices)
  (\idx -> maybe LP.zero
    (\(sh,b) -> if b then negate (arr ! sh) else arr ! sh)
    (indices U.! idx))) <$> baseIndicesDec

-- | Embeds an vector in the CRT basis of the the mth cyclotomic ring
-- to an vector in the CRT basis of the m'th cyclotomic ring when @m | m'@
embedCRT' :: forall mon m m' v r . (CRTrans mon r, Vector v r, m `Divides` m')
          => TaggedT '(m, m') mon (v r -> v r)
embedCRT' =
  (lift (proxyT crtInfo (Proxy::Proxy m') :: mon (CRTInfo r))) >>
  (pureT $ backpermute' <$> baseIndicesCRT)

-- | maps a vector in the powerful/decoding basis, representing an
-- O_m' element, to a vector of arrays representing O_m elements in
-- the same type of basis
coeffs' :: (Vector v r, m `Divides` m')
        => Tagged '(m, m') (v r -> [v r])
coeffs' = flip (\x -> V.toList . V.map (`backpermute'` x))
          <$> extIndicesCoeffs

-- | The "tweaked trace" function in either the powerful or decoding
-- basis of the m'th cyclotomic ring to the mth cyclotomic ring when
-- @m | m'@.
twacePowDec' :: forall m m' r v . (Vector v r, m `Divides` m')
             => Tagged '(m, m') (v r -> v r)
twacePowDec' = backpermute' <$> extIndicesPowDec


-- EAC: twaceCRT is defined in CTensor because it needs access to C-backend functions


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
      let twCRTs' :: Matrix (GF fp d)
            = fromMaybe (error "internal error: crtSetDec': twCRTs") $ proxyT twCRTs m'p
          zmsToIdx = proxy T.zmsToIndexFact m'p
          elt j i = indexM twCRTs' j (zmsToIdx i)
          trace' = trace :: GF fp d -> fp -- to avoid recomputing powTraces
      cosets <- partitionCosets p
      return $ LP.map (\is -> generate phi
                          (\j -> hinv * trace'
                                      (sum $ LP.map (elt j) is))) cosets
