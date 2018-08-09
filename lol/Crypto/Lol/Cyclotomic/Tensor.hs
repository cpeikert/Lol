{-|
Module      : Crypto.Lol.Cyclotomic.Tensor
Description : Interface for cyclotomic tensors, and
              helper functions for tensor indexing.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

  \( \def\Z{\mathbb{Z}} \)
  \( \def\Tw{\text{Tw}} \)
  \( \def\Tr{\text{Tr}} \)
  \( \def\CRT{\text{CRT}} \)
  \( \def\O{\mathcal{O}} \)

Interface for cyclotomic tensors, and helper functions for tensor
indexing.
-}

{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Crypto.Lol.Cyclotomic.Tensor
( TensorPowDec(..)
, TensorG(..)
, TensorCRT(..)
, TensorGaussian(..)
, TensorGSqNorm(..)
, TensorCRTSet(..)
-- * Top-level CRT functions
, hasCRTFuncs
, scalarCRT, mulGCRT, divGCRT, crt, crtInv, twaceCRT, embedCRT
-- * Special vectors/matrices
, Kron, indexK, gCRTK, gInvCRTK, twCRTs
-- * Tensor indexing
, zmsToIndexFact
, indexInfo
, extIndicesPowDec, extIndicesCRT, extIndicesCoeffs
, baseIndicesPow, baseIndicesDec, baseIndicesCRT
, digitRev
)
where

import Crypto.Lol.CRTrans
import Crypto.Lol.Prelude        as LP hiding (lift, (*>))
import Crypto.Lol.Types.IFunctor

import           Algebra.Module          as Module (C)
import           Control.Applicative
import           Control.Monad.Random
import           Data.Foldable           (Foldable)
import           Data.Singletons.Prelude hiding ((:-))
import           Data.Traversable
import           Data.Tuple              (swap)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U

-- | Encapsulates linear transformations needed for cyclotomic ring
-- arithmetic.

-- | The type @t m r@ represents a cyclotomic coefficient tensor of
-- index \(m\) over base ring \(r\).  Most of the methods represent linear
-- transforms corresponding to operations in particular bases.
-- CRT-related methods are wrapped in 'Maybe' because they are
-- well-defined only when a CRT basis exists over the ring \(r\) for
-- index \(m\).

-- | __WARNING:__ as with all fixed-point arithmetic, the methods
-- in 'TensorPowDec' may result in overflow (and thereby incorrect answers
-- and potential security flaws) if the input arguments are too close
-- to the bounds imposed by the base type.  The acceptable range of
-- inputs for each method is determined by the linear transform it
-- implements.

class (ForallFact1 Functor  t, ForallFact1 Applicative t,
       ForallFact1 Foldable t, ForallFact1 Traversable t,
       -- include Functor and Foldable because the other ForallFact1
       -- constraints don't imply them
       IFunctor t, IFElt t r, Additive r)
  => TensorPowDec t r where

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: Fact m => r -> t m r

  -- | Convert between the decoding-basis and powerful-basis
  -- representations.
  powToDec, decToPow :: Fact m => t m r -> t m r

  -- | The @twace@ linear transformation, which is the same in both the
  -- powerful and decoding bases.
  twacePowDec :: (m `Divides` m') => t m' r -> t m r

  -- | The @embed@ linear transformations, for the powerful and
  -- decoding bases.
  embedPow, embedDec :: (m `Divides` m') => t m r -> t m' r

  -- | Map a tensor in the powerful/decoding/CRT basis, representing
  -- an \(\O_{m'}\) element, to a vector of tensors representing
  -- \(\O_m\) elements in the same kind of basis.
  coeffs :: (m `Divides` m') => t m' r -> [t m r]

  -- CJP: we have to use tags due to this bug:
  -- https://ghc.haskell.org/trac/ghc/ticket/14266 "It was a surprise
  -- to me that I can see no way to allow to write an instance when
  -- the method has a a locally-polymorphic but ambiguous method
  -- types."

  -- | The relative powerful basis of \( \O_{m'}/\O_{m} \),
  -- w.r.t. the powerful basis of \( \O_{m'} \).
  powBasisPow :: (m `Divides` m') => Tagged m [t m' r]

-- | Encapsulates multiplication and division by \(g_m\)
class TensorPowDec t r => TensorG t r where
  -- | Multiply by \(g_m\) in the powerful/decoding basis
  mulGPow, mulGDec :: Fact m => t m r -> t m r

  -- | Divide by \(g_m\) in the powerful/decoding basis.  The 'Maybe'
  -- output indicates that the operation may fail, which happens
  -- exactly when the input is not divisible by \(g_m\).
  divGPow, divGDec :: Fact m => t m r -> Maybe (t m r)

-- | Encapsulates functions related to the Chinese-remainder
-- representation/transform.
class (TensorPowDec t r, CRTrans mon r, ForallFact2 (Module.C r) t r)
  => TensorCRT t mon r where
  -- | A tuple of all the operations relating to the CRT basis, in a
  -- single 'Maybe' value for safety.  Clients should typically not
  -- use this method directly, but instead call the corresponding
  -- top-level functions: the elements of the tuple correpond to the
  -- functions 'scalarCRT', 'mulGCRT', 'divGCRT', 'crt', 'crtInv'.
  crtFuncs :: Fact m =>
              mon (    r -> t m r, -- scalarCRT
                   t m r -> t m r, -- mulGCRT
                   t m r -> t m r, -- divGCRT
                   t m r -> t m r, -- crt
                   t m r -> t m r) -- crtInv

  -- | A tuple of all the extension-related operations involving the
  -- CRT bases, for safety.  Clients should typically not use this
  -- method directly, but instead call the corresponding top-level
  -- functions: the elements of the tuple correpond to the functions
  -- 'twaceCRT', 'embedCRT'.
  crtExtFuncs :: (m `Divides` m') =>
                 mon (t m' r -> t m  r, -- twaceCRT
                      t m  r -> t m' r) -- embedCRT

-- | A coefficient tensor that supports Gaussian sampling.
class TensorGaussian t q where
  -- | Sample from the "tweaked" Gaussian error distribution \(t\cdot D\)
  -- in the decoding basis, where \(D\) has scaled variance \(v\).
  tweakedGaussianDec :: (ToRational v, Fact m, MonadRandom rnd)
                        => v -> rnd (t m q)

-- | A coefficient tensor that supports taking norms under the
-- canonical embedding.
class TensorGSqNorm t r where
  -- | Given the coefficient tensor of \(e\) with respect to the
  -- decoding basis of \(R\), yield the (scaled) squared norm of
  -- \(g_m \cdot e\) under the canonical embedding, namely,
  -- \(\hat{m}^{-1} \cdot \| \sigma(g_m \cdot e) \|^2\).
  gSqNormDec :: Fact m => t m r -> r

-- | A 'TensorPowDec' that supports relative CRT sets for the element type
-- 'fp' representing a prime-order finite field.
class (TensorPowDec t fp) => TensorCRTSet t fp where
  -- CJP: see above for why we use a Tagged type here

  -- | Relative mod-@p@ CRT set of \( \O_{m'}/\O_{m} \) in the
  -- decoding basis.
  crtSetDec :: (m `Divides` m', Coprime (PToF (CharOf fp)) m')
    => Tagged m [t m' fp]

-- | Convenience value indicating whether 'crtFuncs' exists.
hasCRTFuncs :: forall t m r mon . (TensorCRT t mon r, Fact m) => mon ()
{-# INLINABLE hasCRTFuncs #-}
hasCRTFuncs = do
  (_,_,_,_,_) <- crtFuncs @t @mon @r @m
  return ()

-- | Yield a tensor for a scalar in the CRT basis.  (This function is
-- simply an appropriate entry from 'crtFuncs'.)
scalarCRT :: (TensorCRT t mon r, Fact m) => mon (r -> t m r)
{-# INLINABLE scalarCRT #-}
scalarCRT = (\(f,_,_,_,_) -> f) <$> crtFuncs

mulGCRT, divGCRT, crt, crtInv ::
  (TensorCRT t mon r, Fact m) => mon (t m r -> t m r)
{-# INLINABLE mulGCRT #-}
{-# INLINABLE divGCRT #-}
{-# INLINABLE crt #-}
{-# INLINE crtInv #-}

-- | Multiply by \(g_m\) in the CRT basis. (This function is simply an
-- appropriate entry from 'crtFuncs'.)
mulGCRT = (\(_,f,_,_,_) -> f) <$> crtFuncs
-- | Divide by \(g_m\) in the CRT basis.  (This function is simply an
-- appropriate entry from 'crtFuncs'.)
divGCRT = (\(_,_,f,_,_) -> f) <$> crtFuncs
-- | The CRT transform.  (This function is simply an appropriate entry
-- from 'crtFuncs'.)
crt = (\(_,_,_,f,_) -> f) <$> crtFuncs
-- | The inverse CRT transform.  (This function is simply an
-- appropriate entry from 'crtFuncs'.)
crtInv = (\(_,_,_,_,f) -> f) <$> crtFuncs

-- | The "tweaked trace" function for tensors in the CRT basis:
-- For cyclotomic indices \(m \mid m'\),
-- \(\Tw(x) = (\hat{m}/\hat{m}') \cdot \Tr((g'/g) \cdot x)\).
-- (This function is simply an appropriate entry from 'crtExtFuncs'.)
twaceCRT :: forall t m m' mon r . (TensorCRT t mon r, m `Divides` m')
            => mon (t m' r -> t m r)
{-# INLINABLE twaceCRT #-}
twaceCRT = hasCRTFuncs @t @m' @r *>
           hasCRTFuncs @t @m  @r *>
           (fst <$> crtExtFuncs)

-- | Embed a tensor with index \(m\) in the CRT basis to a tensor with
-- index \(m'\) in the CRT basis.
-- (This function is simply an appropriate entry from 'crtExtFuncs'.)
embedCRT :: forall t m m' mon r . (TensorCRT t mon r, m `Divides` m')
            => mon (t m r -> t m' r)
embedCRT = hasCRTFuncs @t @m' @r *>
           hasCRTFuncs @t @m  @r *>
           (snd <$> crtExtFuncs)

fKron :: forall m r mon . (Fact m, Monad mon)
          -- higher-rank argument needs to be tagged (pp can't be
          -- ambiguous) because otherwise we can't invoke fKron on an
          -- ambiguous forall'd value
         => (forall pp . (PPow pp) => TaggedT pp mon (KronC r))
         -> mon (Kron r)
fKron mat = go $ sUnF (sing :: SFactored m)
  where go :: Sing (pplist :: [PrimePower]) -> mon (Kron r)
        go spps = case spps of
          SNil -> return MNil
          (SCons spp rest) -> do
            rest' <- go rest
            mat' <- withWitnessT mat spp
            return $ MKron rest' mat'

-- | For a prime power \(p^e\), converts any matrix \(M\) for
-- prime \(p\) to \(\vec{1}_(p^{e-1}) \otimes M\), where \(\vec{1}\)
-- denotes the all-1s vector.
ppKron :: forall pp r mon . (PPow pp, Monad mon)
          -- higher-rank argument needs to be tagged (pp can't be
          -- ambiguous) because otherwise we can't invoke ppKron on an
          -- ambiguous forall'd value
          => (forall p . (Prime p) => TaggedT p mon (KronC r))
          -> TaggedT pp mon (KronC r)
ppKron mat = tagT $ case (sing :: SPrimePower pp) of
  pp@(SPP (STuple2 (sp :: Sing p) _)) -> do
    (MC h w f) <- withWitnessT mat sp
    let d = withSingI pp (valuePPow @pp) `div` withSingI sp (valuePrime @p)
    return $ MC (h*d) w (f . (`mod` h))

-- deeply embedded DSL for Kronecker products of matrices

data KronC r =
  MC Int Int                        -- dims
  (Int -> Int -> r)                 -- yields element i,j

-- | A Kronecker product of zero of more matrices over @r@.
data Kron r = MNil | MKron (Kron r) (KronC r) -- snoc list

-- | Extract the @(i,j)@ element of a 'Kron'.
indexK :: Ring r => Kron r -> Int -> Int -> r
indexK MNil 0 0 = LP.one
indexK (MKron m (MC r c mc)) i j =
  let (iq,ir) = i `divMod` r
      (jq,jr) = j `divMod` c
      in indexK m iq jq * mc ir jr

gCRTK, gInvCRTK :: forall m mon r . (Fact m, CRTrans mon r) => mon (Kron r)
-- | A \(\varphi(m)\)-by-1 matrix of the CRT coefficients of \(g_m\), for
-- \(m\)th cyclotomic.
gCRTK = fKron @m gCRTPPow
-- | A \(\varphi(m)\)-by-1 matrix of the inverse CRT coefficients of \(g_m\),
-- for \(m\)th cyclotomic.
gInvCRTK = fKron @m gInvCRTPPow

-- | The "tweaked" \(\CRT^*\) matrix:
-- \(\CRT^* \cdot \text{diag}(\sigma(g_m))\).
twCRTs :: forall m mon r . (Fact m, CRTrans mon r) => mon (Kron r)
twCRTs = fKron @m twCRTsPPow

-- | The "tweaked" \(\CRT^*\) matrix (for prime powers):
-- \(\CRT^* \cdot \text{diag}(\sigma(g_p))\).
twCRTsPPow :: forall pp mon r .
  (PPow pp, CRTrans mon r) => TaggedT pp mon (KronC r)
twCRTsPPow = do
  let phi    = totientPPow @pp
      iToZms = indexToZmsPPow @pp
      jToPow = indexToPowPPow @pp
  (wPow, _) <- crtInfo
  (MC _ _ gCRT) <- gCRTPPow
  return $ MC phi phi (\j i -> wPow (jToPow j * negate (iToZms i)) * gCRT i 0)

gCRTPPow, gInvCRTPPow :: (PPow pp, CRTrans mon r) => TaggedT pp mon (KronC r)
gCRTPPow = ppKron gCRTPrime
gInvCRTPPow = ppKron gInvCRTPrime

gCRTPrime, gInvCRTPrime :: forall p mon r .
  (Prime p, CRTrans mon r) => TaggedT p mon (KronC r)

-- | A \((p-1)\)-by-1 matrix of the CRT coefficients of \(g_p\), for
-- \(p\)th cyclotomic.
gCRTPrime = do
  let p = valuePrime @p
  (wPow, _) <- crtInfo
  return $ MC (p-1) 1 $ if p == 2 then const $ const one
                        else (\i _ -> one - wPow (i+1))

-- | A \((p-1)\)-by-1 matrix of the inverse CRT coefficients of \(g_p\),
-- for the \(p\)th cyclotomic.
gInvCRTPrime = do
  let p = valuePrime @p
  (wPow, phatinv) <- crtInfo
  return $ MC (p-1) 1 $
    if p == 2 then const $ const one
    else (\i -> const $ phatinv *
                sum [fromIntegral j * wPow ((i+1)*(p-1-j)) | j <- [1..p-1]])

-- Reindexing functions

-- | Base-\(p\) digit reversal; input and output are in \([p^e]\).
digitRev :: PP -> Int -> Int
digitRev (_,0) 0 = 0
-- CJP: use accumulator to avoid multiple exponentiations?
digitRev (p,e) j
  | e >= 1 = let (q,r) = j `divMod` p
             in r * (p^(e-1)) + digitRev (p,e-1) q

indexToPowPPow, indexToZmsPPow :: forall pp . PPow pp => Int -> Int
indexToPowPPow = indexToPow (ppPPow @pp)
indexToZmsPPow = indexToZms (ppPPow @pp)

-- | Convert a \(\Z_m^*\) index to a linear tensor index in \([m]\).
zmsToIndexFact :: forall m . Fact m => (Int -> Int)
zmsToIndexFact = zmsToIndex (ppsFact @m)

-- | For a prime power \(p^e\), map a tensor index to the corresponding
-- power \(j \in [\varphi(p^e)]\), as in the powerful basis.
indexToPow :: PP -> Int -> Int
-- CJP: use accumulator to avoid multiple exponentiations?
indexToPow (p,e) j = let (jq,jr) = j `divMod` (p-1)
                     in p^(e-1)*jr + digitRev (p,e-1) jq

-- | For a prime power \(p^e\), map a tensor index to the corresponding
-- element \(i \in \Z_{p^e}^*\).
indexToZms :: PP -> Int -> Int
indexToZms (p,_) i = let (i1,i0) = i `divMod` (p-1)
                       in p*i1 + i0 + 1

-- | Convert a \(\Z_m^*\) index to a linear tensor index.
zmsToIndex :: [PP] -> Int -> Int
zmsToIndex [] _ = 0
zmsToIndex (pp:rest) i = zmsToIndexPP pp (i `mod` valuePP pp)
                         + totientPP pp * zmsToIndex rest i

-- | Inverse of 'indexToZms'.
zmsToIndexPP :: PP -> Int -> Int
zmsToIndexPP (p,_) i = let (i1,i0) = i `divMod` p
                       in (p-1)*i1 + i0 - 1

-- Index correspondences for ring extensions

-- | Correspondences between the one-dim indexes into a basis of
-- \(\O_{m'}\), and pair indices into [extension basis of \(
-- \O_{m'}/\O_m \)] \(\otimes\) [basis of \(\O_m\)]. The
-- correspondences are the same for Pow, Dec, and CRT bases because
-- they all have such a factorization. The first argument is the list
-- of \((\varphi(m),\varphi(m'))\) pairs for the (merged) prime powers
-- of \(m\),\(m'\).
toIndexPair :: [(Int,Int)] -> Int -> (Int,Int)
fromIndexPair :: [(Int,Int)] -> (Int,Int) -> Int

toIndexPair [] 0 = (0,0)
toIndexPair ((phi,phi'):rest) i' =
  let (i'q,i'r) = i' `divMod` phi'
      (i'rq,i'rr) = i'r `divMod` phi
      (i'q1,i'q0) = toIndexPair rest i'q
  in (i'rq + i'q1*(phi' `div` phi), i'rr + i'q0*phi)

fromIndexPair [] (0,0) = 0
fromIndexPair ((phi,phi'):rest) (i1,i0) =
  let (i0q,i0r) = i0 `divMod` phi
      (i1q,i1r) = i1 `divMod` (phi' `div` phi)
      i = fromIndexPair rest (i1q,i0q)
  in (i0r + i1r*phi) + i*phi'

-- | A collection of useful information for working with tensor
-- extensions. The first component is a list of triples \((p,e,e')\)
-- where \(e\), \(e'\) are respectively the exponents of prime \(p\) in \(m\),
-- \(m'\). The next two components are \(\varphi(m)\) and \(\varphi(m')\). The
-- final component is a pair \( ( \varphi(p^e), \varphi(p^{e'}))\) for each
-- triple in the first component.
indexInfo :: forall m m' . (m `Divides` m')
             => ([(Int,Int,Int)], Int, Int, [(Int,Int)])
indexInfo = let pps  = ppsFact @m
                pps' = ppsFact @m'
                mpps = mergePPs pps pps'
                phi  = totientFact @m
                phi' = totientFact @m'
                tots = totients mpps
            in (mpps, phi, phi', tots)

-- | A vector of \(\varphi(m)\) entries, where the \(i\)th entry is
-- the index into the powerful/decoding basis of \(\O_{m'}\) of the
-- \(i\)th entry of the powerful/decoding basis of \(\O_m\).
extIndicesPowDec :: forall m m' . (m `Divides` m') => U.Vector Int
{-# INLINABLE extIndicesPowDec #-}
extIndicesPowDec =
  let (_, phi, _, tots) = indexInfo @m @m'
  in U.generate phi (fromIndexPair tots . (0,))

-- | A vector of \(\varphi(m)\) blocks of \(\varphi(m')/\varphi(m)\) consecutive
-- entries. Each block contains all those indices into the CRT basis
-- of \(\O_{m'}\) that "lie above" the corresponding index into the CRT
-- basis of \(\O_m\).
extIndicesCRT :: forall m m' . (m `Divides` m') => U.Vector Int
extIndicesCRT =
  let (_, phi, phi', tots) = indexInfo @m @m'
  in U.generate phi'
     (fromIndexPair tots . swap . (`divMod` (phi' `div` phi)))

baseWrapper :: forall m m' a . (m `Divides` m', U.Unbox a)
               => ([(Int,Int,Int)] -> Int -> a)
               -> U.Vector a
baseWrapper f =
  let (mpps, _, phi', _) = indexInfo @m @m'
  in U.generate phi' (f mpps)

-- | A lookup table for 'toIndexPair' applied to indices \([\varphi(m')]\).
baseIndicesPow :: forall m m' . (m `Divides` m') => U.Vector (Int,Int)
baseIndicesPow = baseWrapper @m @m' (toIndexPair . totients)
{-# INLINABLE baseIndicesPow #-}

-- | A lookup table for 'baseIndexDec' applied to indices \([\varphi(m')]\).
baseIndicesDec :: forall m m' . (m `Divides` m') => U.Vector (Maybe (Int,Bool))
-- this one is more complicated; requires the prime powers
baseIndicesDec = baseWrapper @m @m' baseIndexDec
{-# INLINABLE baseIndicesDec #-}

-- | Same as 'baseIndicesPow', but only includes the second component
-- of each pair.
baseIndicesCRT :: forall m m' . (m `Divides` m') => U.Vector Int
baseIndicesCRT =
  baseWrapper @m @m' (\pps -> snd . toIndexPair (totients pps))

-- | The \(i_0\)th entry of the \(i_1\)th vector is
-- 'fromIndexPair' \((i_1,i_0)\).
extIndicesCoeffs :: forall m m' . (m `Divides` m')
                    => V.Vector (U.Vector Int)
extIndicesCoeffs =
  let (_, phi, phi', tots) = indexInfo @m @m'
  in V.generate (phi' `div` phi)
     (\i1 -> U.generate phi (\i0 -> fromIndexPair tots (i1,i0)))

-- | Convenient reindexing functions

-- | Maps an index of the extension ring array to its corresponding
-- index in the base ring array (if it exists), with sign, under the
-- decoding basis.
baseIndexDec :: [(Int,Int,Int)] -> Int -> Maybe (Int, Bool)
baseIndexDec [] 0 = Just (0,False)
baseIndexDec ((p,e,e'):rest) i'
   = let (i'q, i'r) = i' `divMod` totientPP (p,e')
         phi = totientPP (p,e)
         curr
           | p>2 && e==0 && e' > 0 = case i'r of
               0 -> Just (0,False)
               1 -> Just (0,True)
               _ -> Nothing
           | otherwise = if i'r < phi then Just (i'r,False) else Nothing
     in do
       (i,b) <- curr
       (j,b') <- baseIndexDec rest i'q
       return (i + phi*j, b /= b')

-- the first list of pps must "divide" the other.  result is a list of
-- all (prime, min e, max e).
mergePPs :: [PP] -> [PP] -> [(Int,Int,Int)]
mergePPs [] pps = LP.map (\(p,e) -> (p,0,e)) pps
mergePPs allpps@((p,e):pps) ((p',e'):pps')
  | p == p' && e <= e' = (p,  e, e') : mergePPs pps pps'
  | p > p'  = (p', 0, e') : mergePPs allpps pps'

totients :: [(Int, Int, Int)] -> [(Int,Int)]
totients = LP.map (\(p,e,e') -> (totientPP (p,e), totientPP (p,e')))
