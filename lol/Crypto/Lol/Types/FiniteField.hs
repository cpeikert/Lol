{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             GeneralizedNewtypeDeriving, InstanceSigs, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances #-}

-- CJP: need PolyKinds to allow d to have non-* kind

-- | Basic (unoptimized) finite field arithmetic.

module Crypto.Lol.Types.FiniteField
( GF                            -- export type but not constructor
, PrimeField, GFCtx
, size, trace, toList, fromList
, IrreduciblePoly(..), X(..), (^^)
, TensorCoeffs(..)
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Factored
import Crypto.Lol.Prelude
import Crypto.Lol.Reflects

import Algebra.Additive     as Additive (C)
import Algebra.Field        as Field (C)
import Algebra.Module       as Module (C)
import Algebra.Ring         as Ring (C)
import Algebra.ZeroTestable as ZeroTestable (C)
import MathObj.Polynomial

import Math.NumberTheory.Primes.Factorisation

import           Control.Applicative hiding ((*>))
import           Control.DeepSeq
import           Control.Monad
import qualified Data.Vector         as V

--import qualified Debug.Trace as DT

-- | A finite field of given degree over @F_p@.
newtype GF fp d = GF (Polynomial fp)
                  deriving (Eq, Show, Additive.C, ZeroTestable.C, NFData)

-- the second argument, though phantom, affects representation
type role GF representational representational

-- | Constraint synonym for a prime field.
type PrimeField fp = (Enumerable fp, Field fp, Eq fp, ZeroTestable fp,
                      Prime (CharOf fp), IrreduciblePoly fp)

-- | Constraint synonym for a finite field.
type GFCtx fp d = (PrimeField fp, Reflects d Int)

instance (GFCtx fp d) => Enumerable (GF fp d) where
  values = GF <$> fromCoeffs <$>
           -- d-fold cartesian product of Fp values
           replicateM (proxy value (Proxy::Proxy d)) values

instance (GFCtx fp d) => Ring.C (GF fp d) where

  one = GF one

  (*) = let poly = proxy irreduciblePoly (Proxy :: Proxy d)
        in \(GF f) (GF g) -> GF $ (f*g) `mod` poly

  fromInteger = GF . fromInteger

instance (GFCtx fp d) => Field.C (GF fp d) where

  recip = let g = proxy irreduciblePoly (Proxy :: Proxy d)
          in \(GF f) -> let (_,(a,_)) = extendedGCD f g
                           in GF a

instance (GFCtx fp d) => CRTrans Maybe (GF fp d) where

  crtInfo :: forall m . (Reflects m Int) => TaggedT m Maybe (CRTInfo (GF fp d))
  crtInfo = tagT $ (,) <$> omegaPow <*> scalarInv
    where
      omegaPow :: Maybe (Int -> GF fp d)
      omegaPow =
        let size' = proxy size (Proxy :: Proxy (GF fp d))
            mval = proxy value (Proxy :: Proxy m)
            (q,r) = (size'-1) `quotRem` mval
            gen = head $ filter isPrimitive values
            omega = gen^q
            omegaPows = V.iterateN mval (*omega) one
        in if r == 0
           then Just $ (omegaPows V.!) . (`mod` mval)
           else Nothing
      scalarInv :: Maybe (GF fp d)
      scalarInv = Just $ recip $ fromIntegral $ valueHat $
                  (proxy value (Proxy::Proxy m) :: Int)

-- | This wrapper for a list of coefficients is used to define a
-- @GF(p^d)@-module structure for tensors over @F_p@ of dimension @n@, where
-- @d | n@.
newtype TensorCoeffs a = Coeffs {unCoeffs :: [a]} deriving (Additive.C)
instance (Additive fp, Ring (GF fp d), Reflects d Int)
  => Module.C (GF fp d) (TensorCoeffs fp) where

  r *> (Coeffs fps) =
    let dval = proxy value (Proxy::Proxy d)
        n = length fps
    in if n `mod` dval /= 0 then
                error $ "FiniteField: d (= " ++ show dval ++
                          ") does not divide n (= " ++ show n ++ ")"
       else Coeffs $ concat ((toList . (r *) . fromList) <$> chunksOf dval fps)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n > 0 = let (h,t) = splitAt n xs in h : chunksOf n t
  | otherwise = error "chunksOf: non-positive n"

-- | Yield a list of length exactly @d@ (i.e., including trailing zeros)
-- of the @fp@-coefficients with respect to the power basis
toList :: forall fp d . (Reflects d Int, Additive fp) => GF fp d -> [fp]
toList = let dval = proxy value (Proxy::Proxy d)
         in \(GF p) -> let l = coeffs p
                       in l ++ (replicate (dval - length l) zero)

-- | Yield a field element given up to @d@ coefficients with respect
-- to the power basis.
fromList :: forall fp d . (Reflects d Int) => [fp] -> GF fp d
fromList = let dval = proxy value (Proxy::Proxy d)
           in \cs -> if length cs <= dval then GF $ fromCoeffs cs
                     else error $ "FiniteField.fromList: length " ++
                              show (length cs) ++ " > degree " ++ show dval

sizePP :: forall fp d . (GFCtx fp d) => Tagged (GF fp d) PP
sizePP = tag (proxy valuePrime (Proxy::Proxy (CharOf fp)),
              proxy value (Proxy::Proxy d))

-- | The order of the field: @size (GF fp d) = p^d@
size :: (GFCtx fp d) => Tagged (GF fp d) Int
size = uncurry (^) <$> sizePP

isPrimitive :: forall fp d . (GFCtx fp d) => GF fp d -> Bool
isPrimitive = let q = proxy size (Proxy :: Proxy (GF fp d))
                  ps = map (fromIntegral . fst) $ factorise $
                       fromIntegral $ q-1
                  exps = map ((q-1) `div`) ps
              in \g -> not (isZero g) && all (\e -> g^e /= 1) exps

dotp :: (Ring a) => [a] -> [a] -> a
dotp a b = sum $ zipWith (*) a b

-- | Trace into the prime subfield.
trace :: forall fp d . (GFCtx fp d) => GF fp d -> fp
trace = let ts = proxy powTraces (Proxy::Proxy (GF fp d))
        in \(GF f) -> dotp ts (coeffs f)

-- | Traces of the power basis elements 1, x, x^2, ..., x^(d-1).
powTraces :: forall fp d . (GFCtx fp d) => Tagged (GF fp d) [fp]
powTraces =
  --DT.trace ("FiniteField.powTraces: p = " ++
  --          show (proxy value (Proxy::Proxy (CharOf fp)) :: Int) ++
  --          ", d = " ++ show (proxy value (Proxy::Proxy d) :: Int)) $
  let d = proxy value (Proxy :: Proxy d)
  in tag $ map trace' $ take d $
     iterate (* (GF (X ^^ 1))) (one :: GF fp d)

-- helper that computes trace via brute force: sum frobenius
-- automorphisms
trace' :: (GFCtx fp d) => GF fp d -> fp
trace' e = let (p,d) = witness sizePP e
               (GF t) = sum $ take d $ iterate (^p) e
               -- t is a constant polynomial
           in head $ coeffs t

-- | Represents fields over which we can get irreducible
-- polynomials of desired degrees.  (An instance of this class is
-- defined in 'Crypto.Lol.Types.IrreducibleChar2' and exported from
-- 'Crypto.Lol'.)
class Field fp => IrreduciblePoly fp where
  irreduciblePoly :: (Reflects d Int) => Tagged d (Polynomial fp)

-- | Convenience data type for writing 'IrreduciblePoly' instances.
data X = X

-- | Convenience function for writing 'IrreduciblePoly' instances.
(^^) :: Ring a => X -> Int -> Polynomial a
X ^^ i | i >= 0 = fromCoeffs $ replicate i 0 ++ [1]
_ ^^ _ = error "FiniteField.(^^) only defined for non-negative exponents."
