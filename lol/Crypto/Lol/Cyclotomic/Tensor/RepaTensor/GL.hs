{-# LANGUAGE BangPatterns, ConstraintKinds, FlexibleContexts, GADTs,
             MultiParamTypeClasses, NoImplicitPrelude, RankNTypes,
             RebindableSyntax, ScopedTypeVariables #-}

-- | The @G@ and @L@ transforms for Repa arrays.

module Crypto.Lol.Cyclotomic.Tensor.RepaTensor.GL
( fL, fLInv, fGPow, fGDec, fGInvPow, fGInvDec
) where

import Crypto.Lol.Cyclotomic.Tensor.RepaTensor.RTCommon as RT
import Crypto.Lol.Prelude                               as LP
import Data.Coerce

fL, fLInv, fGPow, fGDec :: (Fact m, Additive r, Unbox r, Elt r)
  => Arr m r -> Arr m r
{-# INLINABLE fL #-}
{-# INLINABLE fLInv #-}
{-# INLINABLE fGPow #-}
{-# INLINABLE fGDec #-}

fGInvPow, fGInvDec ::
 (Fact m, IntegralDomain r, ZeroTestable r, Unbox r, Elt r)
  => Arr m r -> Maybe (Arr m r)
{-# INLINABLE fGInvPow #-}
{-# INLINABLE fGInvDec #-}

-- | Arbitrary-index @L@ transform, which converts from decoding-basis
-- to powerful-basis representation.
fL = eval $ fTensor $ ppTensor pL
-- | Arbitrary-index @L^{ -1 }@ transform, which converts from
-- powerful-basis to decoding-basis representation.
fLInv = eval $ fTensor $ ppTensor pLInv
-- | Arbitrary-index multiplication by @g_m@ in the powerful basis.
fGPow = eval $ fTensor $ ppTensor pGPow
-- | Arbitrary-index multiplication by @g_m@ in the decoding basis.
fGDec = eval $ fTensor $ ppTensor pGDec
-- | Arbitrary-index division by @g_m@ in the powerful
-- basis. Outputs 'Nothing' if the input is not evenly divisible by
-- @g_m@.  Warning: not constant time!
fGInvPow = wrapGInv' pGInvPow'
-- | Arbitrary-index division by @g_m@ in the decoding
-- basis. Outputs 'Nothing' if the input is no evenly divisible by
-- @g_m@.  Warning: not constant time!
fGInvDec = wrapGInv' pGInvDec'

wrapGInv' :: forall m r .
  (Fact m, IntegralDomain r, ZeroTestable r, Unbox r, Elt r)
  => (forall p . Prime p => Tagged p (Trans r))
  -> Arr m r -> Maybe (Arr m r)
wrapGInv' ginv =
  let fGInv = eval $ fTensor $ ppTensor ginv
      oddrad = fromIntegral $ proxy oddRadicalFact (Proxy::Proxy m)
  in (`divCheck` oddrad) . fGInv
{-# INLINABLE wrapGInv' #-}

-- | This is not a constant-time algorithm!  Depending on its usage,
-- it might provide a timing side-channel.
divCheck :: (IntegralDomain r, ZeroTestable r, Unbox r)
            => Arr m r -> r -> Maybe (Arr m r)
divCheck = coerce $  \ !arr den ->
  let qrs = force $ RT.map (`divMod` den) arr
      pass = foldAllS (&&) True $ RT.map (isZero . snd) qrs
      out = force $ RT.map fst qrs
  in if pass then Just out else Nothing
{-# INLINABLE divCheck #-}

pWrap :: forall p r . Prime p
         => (forall rep . Source rep r => Int -> Array rep DIM2 r -> Array D DIM2 r)
         -> Tagged p (Trans r)
pWrap f = let pval = proxy valuePrime (Proxy::Proxy p)
              -- special case: return identity function for p=2
          in return $ if pval > 2
                      then trans  (pval-1) $ f pval
                      else Id 1
{-# INLINABLE pWrap #-}


pL, pLInv, pGPow, pGDec :: (Prime p, Additive r, Unbox r, Elt r)
  => Tagged p (Trans r)

pGInvPow', pGInvDec' :: (Prime p, Ring r, Unbox r, Elt r)
  => Tagged p (Trans r)
{-# INLINABLE pL #-}
{-# INLINABLE pLInv #-}
{-# INLINABLE pGPow #-}
{-# INLINABLE pGDec #-}
{-# INLINABLE pGInvPow' #-}
{-# INLINABLE pGInvDec' #-}

pL = pWrap (\_ !arr ->
             fromFunction (extent arr) $
             \ (i':.i) -> sumAllS $ extract (Z:.0) (Z:.(i+1)) $ slice arr (i':.All))

pLInv = pWrap (\_ !arr ->
                let f (i' :. 0) = arr! (i' :. 0)
                    f (i' :. i) = arr! (i' :. i) - arr! (i' :. i-1)
                in fromFunction (extent arr) f)


-- multiplicaton by g_p=1-zeta_p in power basis.
-- this is "wrong" for p=2 but we never use that case thanks to pWrap.
pGPow = pWrap (\p !arr ->
                let f (i':.0) = arr! (i':.p-2) + arr! (i':.0)
                    f (i':.i) = arr! (i':.p-2) + arr! (i':.i) - arr! (i':.i-1)
                in fromFunction (extent arr) f)

-- multiplication by g_p=1-zeta_p in decoding basis
pGDec = pWrap (\_ !arr ->
                let f (i':.0) = arr! (i':.0) + sumAllS (slice arr (i':.All))
                    f (i':.i) = arr! (i':.i) - arr! (i':.i-1)
                in fromFunction (extent arr) f)

-- CJP: profiling suggests that this does two read passes through the
-- array; see if we can rewrite to make it one

-- doesn't do division by (odd) p
pGInvPow' =
  pWrap (\p !arr ->
          let f (i':.i) =
                let col = slice arr (i':.All)
                in fromIntegral (p-i-1) * sumAllS (extract (Z:.0) (Z:.i+1) col) +
                   fromIntegral (-i-1) * sumAllS (extract (Z:.i+1) (Z:.p-i-2) col)
          in fromFunction (extent arr) f)

-- doesn't do division by (odd) p
pGInvDec' =
  pWrap (\p !arr ->
          let f (i':.i) =
                let col = slice arr (i':.All)
                    nats = fromFunction (Z:.p-1) (\(Z:.j) -> fromIntegral j+1)
                in (sumAllS $ RT.zipWith (*) col nats) -
                   fromIntegral p * sumAllS (extract (Z:.i+1) (Z:.p-i-2) col)
          in fromFunction (extent arr) f)
