{-|
Module      : Crypto.Lol.Cyclotomic.Tensor
Description : Interface for cyclotomic tensors, and
              helper functions for tensor indexing.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
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

{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TupleSections           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Crypto.Lol.Cyclotomic.Tensor
( Tensor(..)
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
import Crypto.Lol.Prelude           as LP hiding (lift, (*>))
import Crypto.Lol.Types.FiniteField

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Random
import           Data.Constraint
import           Data.Singletons.Prelude hiding ((:-))
import           Data.Traversable
import           Data.Tuple              (swap)
import qualified Data.Vector             as V
import qualified Data.Vector.Unboxed     as U

-- | 'Tensor' encapsulates all the core linear transformations needed
-- for cyclotomic ring arithmetic.

-- | The type @t m r@ represents a cyclotomic coefficient tensor of
-- index \(m\) over base ring \(r\).  Most of the methods represent linear
-- transforms corresponding to operations in particular bases.
-- CRT-related methods are wrapped in 'Maybe' because they are
-- well-defined only when a CRT basis exists over the ring \(r\) for
-- index \(m\).

-- | The superclass constraints are for convenience, to ensure that we
-- can sample error tensors of 'Double's.

-- | __WARNING:__ as with all fixed-point arithmetic, the methods
-- in 'Tensor' may result in overflow (and thereby incorrect answers
-- and potential security flaws) if the input arguments are too close
-- to the bounds imposed by the base type.  The acceptable range of
-- inputs for each method is determined by the linear transform it
-- implements.

class (TElt t Double, TElt t (Complex Double)) => Tensor t where

  -- | Constraints needed by @t@ to hold type @r@.
  type TElt t r :: Constraint

  -- | Properties that hold for any index. Use with '\\'.
  entailIndexT :: Tagged (t m r)
                  (Fact m :- (Applicative (t m), Traversable (t m)))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailEqT :: Tagged (t m r)
               ((Eq r, Fact m, TElt t r) :- Eq (t m r))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailZTT :: Tagged (t m r)
               ((ZeroTestable r, Fact m, TElt t r) :- ZeroTestable (t m r))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailNFDataT :: Tagged (t m r)
                   ((NFData r, Fact m, TElt t r) :- NFData (t m r))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailRandomT :: Tagged (t m r)
                   ((Random r, Fact m, TElt t r) :- Random (t m r))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailShowT :: Tagged (t m r)
                 ((Show r, Fact m, TElt t r) :- Show (t m r))
  -- | Holds for any (legal) fully-applied tensor. Use with '\\'.
  entailModuleT :: Tagged (GF fp d, t m fp)
                   ((GFCtx fp d, Fact m, TElt t fp) :- Module (GF fp d) (t m fp))

  -- | Convert a scalar to a tensor in the powerful basis.
  scalarPow :: (Additive r, Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Additive r, Fact m, TElt t r) => t m r -> t m r

  -- | Multiply by \(g_m\) in the powerful/decoding basis
  mulGPow, mulGDec :: (Ring r, Fact m, TElt t r) => t m r -> t m r

  -- | Divide by \(g_m\) in the powerful/decoding basis.  The 'Maybe'
  -- output indicates that the operation may fail, which happens
  -- exactly when the input is not divisible by \(g_m\).
  divGPow, divGDec :: (ZeroTestable r, IntegralDomain r, Fact m, TElt t r)
                      => t m r -> Maybe (t m r)

  -- | A tuple of all the operations relating to the CRT basis, in a
  -- single 'Maybe' value for safety.  Clients should typically not
  -- use this method directly, but instead call the corresponding
  -- top-level functions: the elements of the tuple correpond to the
  -- functions 'scalarCRT', 'mulGCRT', 'divGCRT', 'crt', 'crtInv'.
  crtFuncs :: (CRTrans mon r, Fact m, TElt t r) =>
              mon (    r -> t m r, -- scalarCRT
                   t m r -> t m r, -- mulGCRT
                   t m r -> t m r, -- divGCRT
                   t m r -> t m r, -- crt
                   t m r -> t m r) -- crtInv

  -- | Sample from the "tweaked" Gaussian error distribution \(t\cdot D\)
  -- in the decoding basis, where \(D\) has scaled variance \(v\).
  tGaussianDec :: (OrdFloat q, Random q, TElt t q,
                   ToRational v, Fact m, MonadRandom rnd)
                  => v -> rnd (t m q)

  -- | Given the coefficient tensor of \(e\) with respect to the
  -- decoding basis of \(R\), yield the (scaled) squared norm of
  -- \(g_m \cdot e\) under the canonical embedding, namely,
  -- \(\hat{m}^{-1} \cdot \| \sigma(g_m \cdot e) \|^2\).
  gSqNormDec :: (Ring r, Fact m, TElt t r) => t m r -> r

  -- | The @twace@ linear transformation, which is the same in both the
  -- powerful and decoding bases.
  twacePowDec :: (Ring r, m `Divides` m', TElt t r) => t m' r -> t m r

  -- | The @embed@ linear transformations, for the powerful and
  -- decoding bases.
  embedPow, embedDec :: (Additive r, m `Divides` m', TElt t r)
                        => t m r -> t m' r

  -- | A tuple of all the extension-related operations involving the
  -- CRT bases, for safety.  Clients should typically not use this
  -- method directly, but instead call the corresponding top-level
  -- functions: the elements of the tuple correpond to the functions
  -- 'twaceCRT', 'embedCRT'.
  crtExtFuncs :: (CRTrans mon r, m `Divides` m', TElt t r) =>
                 mon (t m' r -> t m  r, -- twaceCRT
                      t m  r -> t m' r) -- embedCRT

  -- | Map a tensor in the powerful\/decoding\/CRT basis, representing
  -- an \(\O_{m'}\) element, to a vector of tensors representing
  -- \(\O_m\) elements in the same kind of basis.
  coeffs :: (Ring r, m `Divides` m', TElt t r) => t m' r -> [t m r]

  -- | The powerful extension basis w.r.t. the powerful basis.
  powBasisPow :: (Ring r, TElt t r, m `Divides` m') => Tagged m [t m' r]

  -- | A list of tensors representing the mod-@p@ CRT set of the extension.
  crtSetDec :: (m `Divides` m', PrimeField fp, Coprime (PToF (CharOf fp)) m',
                TElt t fp)
               => Tagged m [t m' fp]

  -- | Potentially optimized version of 'fmap' for types that satisfy 'TElt'.
  fmapT :: (Fact m, TElt t a, TElt t b) => (a -> b) -> t m a -> t m b

  -- | Potentially optimized zipWith for types that satisfy 'TElt'.
  zipWithT :: (Fact m, TElt t a, TElt t b, TElt t c)
              => (a -> b -> c) -> t m a -> t m b -> t m c

  -- | Potentially optimized unzip for types that satisfy 'TElt'.
  unzipT :: (Fact m, TElt t (a,b), TElt t a, TElt t b)
            => t m (a,b) -> (t m a, t m b)

-- | Convenience value indicating whether 'crtFuncs' exists.
hasCRTFuncs :: forall t m mon r . (CRTrans mon r, Tensor t, Fact m, TElt t r)
               => TaggedT (t m r) mon ()
{-# INLINABLE hasCRTFuncs #-}
hasCRTFuncs = tagT $ do
  (_ :: r -> t m r,_,_,_,_) <- crtFuncs
  return ()

-- | Yield a tensor for a scalar in the CRT basis.  (This function is
-- simply an appropriate entry from 'crtFuncs'.)
scalarCRT :: (CRTrans mon r, Tensor t, Fact m, TElt t r) => mon (r -> t m r)
{-# INLINABLE scalarCRT #-}
scalarCRT = (\(f,_,_,_,_) -> f) <$> crtFuncs

mulGCRT, divGCRT, crt, crtInv ::
  (CRTrans mon r, Tensor t, Fact m, TElt t r) => mon (t m r -> t m r)
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
twaceCRT :: forall t m m' mon r . (CRTrans mon r, Tensor t, m `Divides` m', TElt t r)
            => mon (t m' r -> t m r)
{-# INLINABLE twaceCRT #-}
twaceCRT = proxyT hasCRTFuncs (Proxy::Proxy (t m' r)) *>
           proxyT hasCRTFuncs (Proxy::Proxy (t m  r)) *>
           (fst <$> crtExtFuncs)

-- | Embed a tensor with index \(m\) in the CRT basis to a tensor with
-- index \(m'\) in the CRT basis.
-- (This function is simply an appropriate entry from 'crtExtFuncs'.)
embedCRT :: forall t m m' mon r . (CRTrans mon r, Tensor t, m `Divides` m', TElt t r)
            => mon (t m r -> t m' r)
embedCRT = proxyT hasCRTFuncs (Proxy::Proxy (t m' r)) *>
           proxyT hasCRTFuncs (Proxy::Proxy (t m  r)) *>
           (snd <$> crtExtFuncs)

fKron :: forall m r mon . (Fact m, Monad mon)
         => (forall pp . (PPow pp) => TaggedT pp mon (KronC r))
         -> TaggedT m mon (Kron r)
fKron mat = tagT $ go $ sUnF (sing :: SFactored m)
  where go :: Sing (pplist :: [PrimePower]) -> mon (Kron r)
        go spps = case spps of
          SNil -> return MNil
          (SCons spp rest) -> do
            rest' <- go rest
            mat' <- withWitnessT mat spp
            return $ MKron rest' mat'

-- | For a prime power \(p^e\), converts any matrix \(M\) for
-- prime \(p\) to \(\vece{1}_(p^{e-1}) \otimes M\), where \(\vece{1}\)
-- denotes the all-1s vector.
ppKron :: forall pp r mon . (PPow pp, Monad mon)
          => (forall p . (Prime p) => TaggedT p mon (KronC r))
          -> TaggedT pp mon (KronC r)
ppKron mat = tagT $ case (sing :: SPrimePower pp) of
  pp@(SPP (STuple2 sp _)) -> do
    (MC h w f) <- withWitnessT mat sp
    let d = withWitness valuePPow pp `div` withWitness valuePrime sp
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

gCRTK, gInvCRTK :: (Fact m, CRTrans mon r) => TaggedT m mon (Kron r)
-- | A \(\varphi(m)\)-by-1 matrix of the CRT coefficients of \(g_m\), for
-- \(m\)th cyclotomic.
gCRTK = fKron gCRTPPow
-- | A \(\varphi(m)\)-by-1 matrix of the inverse CRT coefficients of \(g_m\),
-- for \(m\)th cyclotomic.
gInvCRTK = fKron gInvCRTPPow

-- | The "tweaked" \(\CRT^*\) matrix:
-- \(\CRT^* \cdot \text{diag}(\sigma(g_m))\).
twCRTs :: (Fact m, CRTrans mon r) => TaggedT m mon (Kron r)
twCRTs = fKron twCRTsPPow

-- | The "tweaked" \(\CRT^*\) matrix (for prime powers):
-- \(\CRT^* \cdot \text{diag}(\sigma(g_p))\).
twCRTsPPow :: (PPow pp, CRTrans mon r) => TaggedT pp mon (KronC r)
twCRTsPPow = do
  phi    <- pureT totientPPow
  iToZms <- pureT indexToZmsPPow
  jToPow <- pureT indexToPowPPow
  (wPow, _) <- crtInfo
  (MC _ _ gCRT) <- gCRTPPow

  return $ MC phi phi (\j i -> wPow (jToPow j * negate (iToZms i)) * gCRT i 0)

gCRTPPow, gInvCRTPPow :: (PPow pp, CRTrans mon r) => TaggedT pp mon (KronC r)
gCRTPPow = ppKron gCRTPrime
gInvCRTPPow = ppKron gInvCRTPrime

gCRTPrime, gInvCRTPrime :: (Prime p, CRTrans mon r) => TaggedT p mon (KronC r)

-- | A \((p-1)\)-by-1 matrix of the CRT coefficients of \(g_p\), for
-- \(p\)th cyclotomic.
gCRTPrime = do
  p <- pureT valuePrime
  (wPow, _) <- crtInfo
  return $ MC (p-1) 1 $ if p == 2 then const $ const one
                        else (\i _ -> one - wPow (i+1))

-- | A \((p-1)\)-by-1 matrix of the inverse CRT coefficients of \(g_p\),
-- for the \(p\)th cyclotomic.
gInvCRTPrime = do
  p <- pureT valuePrime
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

indexToPowPPow, indexToZmsPPow :: PPow pp => Tagged pp (Int -> Int)
indexToPowPPow = indexToPow <$> ppPPow
indexToZmsPPow = indexToZms <$> ppPPow

-- | Convert a \(\Z_m^*\) index to a linear tensor index in \([m]\).
zmsToIndexFact :: Fact m => Tagged m (Int -> Int)
zmsToIndexFact = zmsToIndex <$> ppsFact

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
             => Tagged '(m, m') ([(Int,Int,Int)], Int, Int, [(Int,Int)])
indexInfo = let pps = proxy ppsFact (Proxy::Proxy m)
                pps' = proxy ppsFact (Proxy::Proxy m')
                mpps = mergePPs pps pps'
                phi = proxy totientFact (Proxy::Proxy m)
                phi' = proxy totientFact (Proxy::Proxy m')
                tots = totients mpps
            in tag (mpps, phi, phi', tots)

-- | A vector of \(\varphi(m)\) entries, where the \(i\)th entry is
-- the index into the powerful\/decoding basis of \(\O_{m'}\) of the
-- \(i\)th entry of the powerful/decoding basis of \(\O_m\).
extIndicesPowDec :: (m `Divides` m') => Tagged '(m, m') (U.Vector Int)
{-# INLINABLE extIndicesPowDec #-}
extIndicesPowDec = do
  (_, phi, _, tots) <- indexInfo
  return $ U.generate phi (fromIndexPair tots . (0,))

-- | A vector of \(\varphi(m)\) blocks of \(\varphi(m')/\varphi(m)\) consecutive
-- entries. Each block contains all those indices into the CRT basis
-- of \(\O_{m'}\) that "lie above" the corresponding index into the CRT
-- basis of \(\O_m\).
extIndicesCRT :: forall m m' . (m `Divides` m')
                 => Tagged '(m, m') (U.Vector Int)
extIndicesCRT = do
  (_, phi, phi', tots) <- indexInfo
  return $ U.generate phi'
           (fromIndexPair tots . swap . (`divMod` (phi' `div` phi)))

baseWrapper :: forall m m' a . (m `Divides` m', U.Unbox a)
               => ([(Int,Int,Int)] -> Int -> a)
               -> Tagged '(m, m') (U.Vector a)
baseWrapper f = do
  (mpps, _, phi', _) <- indexInfo
  return $ U.generate phi' (f mpps)

-- | A lookup table for 'toIndexPair' applied to indices \([\varphi(m')]\).
baseIndicesPow :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector (Int,Int))
baseIndicesPow = baseWrapper (toIndexPair . totients)
{-# INLINABLE baseIndicesPow #-}

-- | A lookup table for 'baseIndexDec' applied to indices \([\varphi(m')]\).
baseIndicesDec :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector (Maybe (Int,Bool)))
-- this one is more complicated; requires the prime powers
baseIndicesDec = baseWrapper baseIndexDec
{-# INLINABLE baseIndicesDec #-}

-- | Same as 'baseIndicesPow', but only includes the second component
-- of each pair.
baseIndicesCRT :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector Int)
baseIndicesCRT =
  baseWrapper (\pps -> snd . toIndexPair (totients pps))

-- | The \(i_0\)th entry of the \(i_1\)th vector is
-- 'fromIndexPair' \((i_1,i_0)\).
extIndicesCoeffs :: forall m m' . (m `Divides` m')
                    => Tagged '(m, m') (V.Vector (U.Vector Int))
extIndicesCoeffs = do
  (_, phi, phi', tots) <- indexInfo
  return $ V.generate (phi' `div` phi)
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
