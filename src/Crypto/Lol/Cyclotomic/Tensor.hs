{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             NoImplicitPrelude, RankNTypes, ScopedTypeVariables, 
             TupleSections, TypeFamilies, TypeOperators, 
             UndecidableInstances #-}

-- | Interface for cyclotomic tensors, and helper functions for tensor
-- indexing.

module Crypto.Lol.Cyclotomic.Tensor
( Tensor(..)
-- * Top-level CRT functions
, hasCRTFuncs
, scalarCRT, mulGCRT, divGCRT, crt, crtInv, twaceCRT, embedCRT
-- * Tensor indexing
, Matrix, indexM, twCRTs
, zmsToIndexFact
, indexInfo
, extIndicesPowDec, extIndicesCRT, extIndicesCoeffs
, baseIndicesPow, baseIndicesDec, baseIndicesCRT
, digitRev
)
where

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude as LP hiding (lift, (*>))
import Crypto.Lol.Types.FiniteField

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad.Random
import           Data.Constraint
import           Data.Singletons.Prelude hiding ((:-))
import           Data.Traversable
import           Data.Tuple           (swap)
import qualified Data.Vector          as V
import qualified Data.Vector.Unboxed  as U

-- | 'Tensor' encapsulates all the core linear transformations needed
-- for cyclotomic ring arithmetic.

-- | The type @t m r@ represents a cyclotomic coefficient tensor of
-- index @m@ over base ring @r@.  Most of the methods represent linear
-- transforms corresponding to operations in particular bases.
-- CRT-related methods are wrapped in 'Maybe' because they are
-- well-defined only when a CRT basis exists over the ring @r@ for
-- index @m@.

-- | The superclass constraint is for convenience, to ensure that we
-- can sample error tensors of 'Double's.

class (TElt t Double, TElt t (Complex Double))
      => Tensor (t :: Factored -> * -> *) where

  type TElt t r :: Constraint

  -- | Properties that hold for any index. Use with '\\'.
  entailIndexT :: Tagged (t m r)
                  (Fact m :- (Applicative (t m), Traversable (t m)))
  
  -- | Properties that hold for any (legal) fully-applied tensor. Use
  -- with '\\'.
  entailFullT :: Tagged (t m r)
                 ((Fact m, TElt t r) :- 
                  (Eq (t m r), ZeroTestable (t m r), Ring (t m r), 
                   NFData (t m r), Random (t m r)))

  -- | Converts a scalar to a tensor in the powerful basis
  scalarPow :: (Fact m, TElt t r) => r -> t m r

  -- | 'l' converts from decoding-basis representation to
  -- powerful-basis representation; 'lInv' is its inverse.
  l, lInv :: (Fact m, TElt t r) => t m r -> t m r

  -- | Multiply by @g@ in the powerful/decoding basis
  mulGPow, mulGDec :: (Fact m, TElt t r) => t m r -> t m r

  -- | Divide by @g@ in the powerful/decoding basis.  The 'Maybe'
  -- output indicates that the operation may fail, which happens
  -- exactly when the input is not divisible by @g@.
  divGPow, divGDec :: (Fact m, TElt t r) => t m r -> Maybe (t m r)

  -- | A tuple of all the operations relating to the CRT basis, in a
  -- single 'Maybe' value for safety.  Clients should typically not
  -- use this method directly, but instead call the corresponding
  -- top-level functions: the elements of the tuple correpond to the
  -- functions 'scalarCRT', 'mulGCRT', 'divGCRT', 'crt', 'crtInv'.
  crtFuncs :: (Fact m, TElt t r, CRTrans r) =>
              Maybe (    r -> t m r, -- scalarCRT
                     t m r -> t m r, -- mulGCRT
                     t m r -> t m r, -- divGCRT
                     t m r -> t m r, -- crt
                     t m r -> t m r) -- crtInv

  -- | Sample from the "skewed" Gaussian error distribution @t*D@
  -- in the decoding basis, where @D@ has scaled variance @v@.
  tGaussianDec :: (Fact m, OrdFloat q, Random q, TElt t q,
                   ToRational v, MonadRandom rnd)
                  => v -> rnd (t m q)

  -- | The @twace@ linear transformation, which is the same in both the
  -- powerful and decoding bases.
  twacePowDec :: (m `Divides` m', TElt t r) => t m' r -> t m r

  -- | The @embed@ linear transformations, for the powerful and
  -- decoding bases.
  embedPow, embedDec :: (m `Divides` m', TElt t r)
                        => t m r -> t m' r

  -- | A tuple of all the extension-related operations involving the
  -- CRT bases, for safety.  Clients should typically not use this
  -- method directly, but instead call the corresponding top-level
  -- functions: the elements of the tuple correpond to the functions
  -- 'twaceCRT', 'embedCRT'.
  crtExtFuncs :: (m `Divides` m', TElt t r, CRTrans r) =>
                 Maybe (t m' r -> t m  r, -- twaceCRT
                        t m  r -> t m' r) -- embedCRT

  -- | Map a tensor in the powerful\/decoding\/CRT basis, representing
  -- an @O_m'@ element, to a vector of tensors representing @O_m@
  -- elements in the same kind of basis.
  coeffs :: (m `Divides` m', TElt t r) => t m' r -> [t m r]

  -- | The powerful extension basis w.r.t. the powerful basis.
  powBasisPow :: (m `Divides` m', TElt t r) => Tagged m [t m' r]

  -- | A list of tensors representing the mod-@p@ CRT set of the
  -- extension.
  crtSetDec :: (m `Divides` m', PrimeField fp,
                Coprime (PToF (CharOf fp)) m', TElt t fp)
               => Tagged m [t m' fp]

  -- | Potentially optimized version of 'fmap' when the input and
  -- output element types satisfy 'TElt'.
  fmapT :: (Fact m, TElt t a, TElt t b) => (a -> b) -> t m a -> t m b
  -- | Potentially optimized monadic 'fmap'.
  fmapTM :: (Monad mon, Fact m, TElt t a, TElt t b)
             => (a -> mon b) -> t m a -> mon (t m b)

-- | Convenience value indicating whether 'crtFuncs' exists.
hasCRTFuncs :: forall t m r . (Tensor t, Fact m, TElt t r, CRTrans r)
               => TaggedT (t m r) Maybe ()
hasCRTFuncs = tagT $ do
  (_ :: r -> t m r,_,_,_,_) <- crtFuncs
  return ()

-- | Yield a tensor for a scalar in the CRT basis.  (This function is
-- simply an appropriate entry from 'crtFuncs'.)
scalarCRT :: (Tensor t, Fact m, TElt t r, CRTrans r) => Maybe (r -> t m r)
scalarCRT = (\(f,_,_,_,_) -> f) <$> crtFuncs


mulGCRT, divGCRT, crt, crtInv :: (Tensor t, Fact m, TElt t r, CRTrans r)
  => Maybe (t m r -> t m r)
-- | Multiply by @g@ in the CRT basis. (This function is simply an
-- appropriate entry from 'crtFuncs'.)
mulGCRT = (\(_,f,_,_,_) -> f) <$> crtFuncs
-- | Divide by @g@ in the CRT basis.  (This function is simply an
-- appropriate entry from 'crtFuncs'.)
divGCRT = (\(_,_,f,_,_) -> f) <$> crtFuncs
-- | The CRT transform.  (This function is simply an appropriate entry
-- from 'crtFuncs'.)
crt = (\(_,_,_,f,_) -> f) <$> crtFuncs
-- | The inverse CRT transform.  (This function is simply an
-- appropriate entry from 'crtFuncs'.)
crtInv = (\(_,_,_,_,f) -> f) <$> crtFuncs

-- | The "tweaked trace" function for tensors in the CRT basis:
-- For cyclotomic indices m | m',
-- @Tw(x) = (mhat\/m\'hat) * Tr(g\'\/g * x)@.
-- (This function is simply an appropriate entry from 'crtExtFuncs'.)
twaceCRT :: forall t r m m' . (Tensor t, m `Divides` m', TElt t r, CRTrans r)
            => Maybe (t m' r -> t m r)
twaceCRT = proxyT hasCRTFuncs (Proxy::Proxy (t m' r)) *>
           proxyT hasCRTFuncs (Proxy::Proxy (t m  r)) *>
           (fst <$> crtExtFuncs)


-- | Embed a tensor with index @m@ in the CRT basis to a tensor with
-- index @m'@ in the CRT basis.
-- (This function is simply an appropriate entry from 'crtExtFuncs'.)
embedCRT :: forall t r m m' . (Tensor t, m `Divides` m', TElt t r, CRTrans r)
            => Maybe (t m r -> t m' r)
embedCRT = proxyT hasCRTFuncs (Proxy::Proxy (t m' r)) *>
           proxyT hasCRTFuncs (Proxy::Proxy (t m  r)) *>
           (snd <$> crtExtFuncs)

fMatrix :: forall m r mon . (Fact m, Monad mon, Ring r)
           => (forall pp . (PPow pp) => TaggedT pp mon (MatrixC r))
           -> TaggedT m mon (Matrix r)
fMatrix mat = tagT $ go $ sUnF (sing :: SFactored m)
  where go :: Sing (pplist :: [PrimePower]) -> mon (Matrix r)
        go spps = case spps of
          SNil -> return MNil
          (SCons spp rest) -> do
            rest' <- go rest
            mat' <- withWitnessT mat spp
            return $ MKron rest' mat'

-- deeply embedded DSL for Kronecker products of matrices

data MatrixC r = 
  MC (Int -> Int -> r)           -- yields element i,j
  Int Int                        -- dims

-- | A Kronecker product of zero of more matrices over @r@.
data Matrix r = MNil | MKron (Matrix r) (MatrixC r)

-- | Extract the @(i,j)@ element of a 'Matrix'.
indexM :: Ring r => Matrix r -> Int -> Int -> r
indexM MNil 0 0 = LP.one
indexM (MKron m (MC mc r c)) i j =
  let (iq,ir) = i `divMod` r
      (jq,jr) = j `divMod` c
      in indexM m iq jq * mc ir jr

-- | The "tweaked" CRT^* matrix: @CRT^* . diag(sigma(g_m))@.
twCRTs :: (Fact m, CRTrans r) => TaggedT m Maybe (Matrix r)
twCRTs = fMatrix twCRTsPPow

-- | The "tweaked" CRT^* matrix (for prime powers): @CRT^* * diag(sigma(g_p))@.
twCRTsPPow :: (PPow pp, CRTrans r) => TaggedT pp Maybe (MatrixC r)
twCRTsPPow = do
  phi    <- pureT totientPPow
  iToZms <- pureT indexToZmsPPow
  jToPow <- pureT indexToPowPPow
  (wPow, _) <- crtInfoPPow
  gEmb <- gEmbPPow

  return $ MC (\j i -> let i' = iToZms i
                       in wPow (jToPow j * negate i') * gEmb i') phi phi

-- Reindexing functions

-- | Base-p digit reversal; input and output are in @[p^e]@.
digitRev :: PP -> Int -> Int
digitRev (_,0) 0 = 0
-- CJP: use accumulator to avoid multiple exponentiations?
digitRev (p,e) j 
  | e >= 1 = let (q,r) = j `divMod` p
             in r * (p^(e-1)) + digitRev (p,e-1) q

indexToPowPPow, indexToZmsPPow :: PPow pp => Tagged pp (Int -> Int)
indexToPowPPow = indexToPow <$> ppPPow
indexToZmsPPow = indexToZms <$> ppPPow

-- | Convert a @Z_m^*@ index to a linear tensor index in @[m]@.
zmsToIndexFact :: Fact m => Tagged m (Int -> Int)
zmsToIndexFact = zmsToIndex <$> ppsFact

-- | For a prime power @p^e@, map a tensor index to the corresponding
-- power j in @[phi(p^e)]@, as in the powerful basis.
indexToPow :: PP -> Int -> Int
-- CJP: use accumulator to avoid multiple exponentiations?
indexToPow (p,e) j = let (jq,jr) = j `divMod` (p-1)
                     in p^(e-1)*jr + digitRev (p,e-1) jq

-- | For a prime power @p^e@, map a tensor index to the corresponding
-- element i in @Z_{p^e}^*@.
indexToZms :: PP -> Int -> Int
indexToZms (p,_) i = let (i1,i0) = i `divMod` (p-1)
                       in p*i1 + i0 + 1 

-- | Convert a Z_m^* index to a linear tensor index.
zmsToIndex :: [PP] -> Int -> Int
zmsToIndex [] _ = 0
zmsToIndex (pp:rest) i = zmsToIndexPP pp (i `mod` valuePP pp)
                         + (totientPP pp) * zmsToIndex rest i

-- | Inverse of 'indexToZms'.
zmsToIndexPP :: PP -> Int -> Int
zmsToIndexPP (p,_) i = let (i1,i0) = i `divMod` p
                       in (p-1)*i1 + i0 - 1

-- Index correspondences for ring extensions

-- | Correspondences between the linear indexes into a basis of O_m',
-- and pair indices into (extension basis) \otimes (basis of O_m).
-- The work the same for Pow,Dec,CRT bases because all these bases
-- have that factorization.  The first argument is the list of
-- @(phi(m),phi(m'))@ pairs for the (merged) prime powers of @m@,@m'@.
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
-- extensions.  The first component is a list of triples @(p,e,e')@
-- where @e@, @e'@ are respectively the exponents of prime @p@ in @m@,
-- @m'@.  The next two components are @phi(m)@ and @phi(m')@.  The
-- final component is a pair @(phi(p^e), phi(p^e'))@ for each triple
-- in the first component.
indexInfo :: forall m m' . (m `Divides` m')
             => Tagged '(m, m') ([(Int,Int,Int)], Int, Int, [(Int,Int)])
indexInfo = let pps = proxy ppsFact (Proxy::Proxy m)
                pps' = proxy ppsFact (Proxy::Proxy m')
                mpps = mergePPs pps pps'
                phi = totientPPs pps
                phi' = totientPPs pps'
                tots = totients mpps
            in tag (mpps, phi, phi', tots)

-- | A vector of @phi(m)@ entries, where the @i@th entry is the index
-- into the powerful\/decoding basis of @O_m'@ of the
-- @i@th entry of the powerful\/decoding basis of @O_m@.
extIndicesPowDec :: (m `Divides` m') => Tagged '(m, m') (U.Vector Int)
extIndicesPowDec = do
  (_, phi, _, tots) <- indexInfo
  return $ U.generate phi (fromIndexPair tots . (0,))

-- | A vector of @phi(m)@ blocks of @phi(m')\/phi(m)@ consecutive
-- entries. Each block contains all those indices into the CRT basis
-- of @O_m'@ that "lie above" the corresponding index into the CRT
-- basis of @O_m@.
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

-- | A lookup table for 'toIndexPair' applied to indices @[phi(m')]@.
baseIndicesPow :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector (Int,Int))
-- | A lookup table for 'baseIndexDec' applied to indices @[phi(m')]@.
baseIndicesDec :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector (Maybe (Int,Bool)))

-- | Same as 'baseIndicesPow', but only includes the second component
-- of each pair.
baseIndicesCRT :: forall m m' . (m `Divides` m')
                  => Tagged '(m, m') (U.Vector Int)

baseIndicesPow = baseWrapper (toIndexPair . totients)

-- this one is more complicated; requires the prime powers
baseIndicesDec = baseWrapper baseIndexDec

baseIndicesCRT =
  baseWrapper (\pps -> snd . toIndexPair (totients pps))


-- | The @i0@th entry of the @i1@th vector is 'fromIndexPair' @(i1,i0)@.
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
