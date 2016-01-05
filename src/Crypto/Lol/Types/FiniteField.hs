{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             RoleAnnotations, ScopedTypeVariables, TypeFamilies,
             UndecidableInstances #-}

-- CJP: need PolyKinds to allow deg to have non-* kind

-- | Basic (unoptimized) finite field arithmetic.

module Crypto.Lol.Types.FiniteField
( PrimeField, GF   -- export type but not constructor
, trace
, size
, IrreduciblePoly(..), X(..), (^^)
) where

import Crypto.Lol.CRTrans
import Crypto.Lol.Factored
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Reflects

import Algebra.Additive     as Additive (C)
import Algebra.Field        as Field (C)
import Algebra.Ring         as Ring (C)
import Algebra.ZeroTestable as ZeroTestable (C)
import MathObj.Polynomial

import Math.NumberTheory.Primes.Factorisation

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import qualified Data.Vector         as V

--import qualified Debug.Trace as DT

-- | A finite field of given degree over @F_p@.
newtype GF fp deg = GF (Polynomial fp)
                  deriving (Eq, Show, Additive.C, ZeroTestable.C, NFData)

-- the second argument, though phantom, affects representation
type role GF representational representational

type PrimeField fp = (Enumerable fp, Field fp, Eq fp, ZeroTestable fp,
                      Prim (CharOf fp), IrreduciblePoly fp)

type GFCtx fp deg = (PrimeField fp, Reflects deg Int)

instance (GFCtx fp deg) => Enumerable (GF fp deg) where
  values = GF <$> fromCoeffs <$>
           -- d-fold cartesian product of Fp values
           replicateM (proxy value (Proxy::Proxy deg)) values

instance (GFCtx fp deg) => Ring.C (GF fp deg) where

  one = GF one

  (*) = let poly = proxy irreduciblePoly (Proxy :: Proxy deg)
        in \(GF f) (GF g) -> GF $ (f*g) `mod` poly

  fromInteger = GF . fromInteger

instance (GFCtx fp deg) => Field.C (GF fp deg) where

  recip = let g = proxy irreduciblePoly (Proxy :: Proxy deg)
          in \(GF f) -> let (_,(a,_)) = extendedGCD f g
                           in GF a

instance (GFCtx fp deg) => CRTrans (GF fp deg) where

  crtInfo m = (,) <$> omegaPow <*> scalarInv
    where
      omegaPow =
        let size' = proxy size (Proxy :: Proxy (GF fp deg))
            (q,r) = (size'-1) `quotRem` m
            gen = head $ filter isPrimitive values
            omega = gen^q
            omegaPows = V.iterateN m (*omega) one
        in if r == 0
           then Just $ (omegaPows V.!) . (`mod` m)
           else Nothing
      scalarInv = Just $ recip $ fromIntegral $ valueHat m

sizePP :: forall fp deg . (GFCtx fp deg) => Tagged (GF fp deg) PP
sizePP = tag (proxy valuePrime (Proxy::Proxy (CharOf fp)),
              proxy value (Proxy::Proxy deg))

-- | The order of the field: @size (GF fp deg) = p^deg@
size :: (GFCtx fp deg) => Tagged (GF fp deg) Int
size = uncurry (^) <$> sizePP

isPrimitive :: forall fp deg . (GFCtx fp deg) => GF fp deg -> Bool
isPrimitive = let q = proxy size (Proxy :: Proxy (GF fp deg))
                  ps = map (fromIntegral . fst) $ factorise $
                       fromIntegral $ q-1
                  exps = map ((q-1) `div`) ps
              in \g -> not (isZero g) && all (\e -> g^e /= 1) exps

dotp :: (Ring a) => [a] -> [a] -> a
dotp a b = sum $ zipWith (*) a b

-- | Trace into the prime subfield.
trace :: forall fp deg . (GFCtx fp deg) => GF fp deg -> fp
trace = let ts = proxy powTraces (Proxy::Proxy (GF fp deg))
        in \(GF f) -> dotp ts (coeffs f)

-- | Traces of the power basis elements 1, x, x^2, ..., x^(deg-1).
powTraces :: forall fp deg . (GFCtx fp deg) => Tagged (GF fp deg) [fp]
powTraces =
  --DT.trace ("FiniteField.powTraces: p = " ++
  --          show (proxy value (Proxy::Proxy (CharOf fp)) :: Int) ++
  --          ", d = " ++ show (proxy value (Proxy::Proxy deg) :: Int)) $
  let d = proxy value (Proxy :: Proxy deg)
  in tag $ map trace' $ take d $
     iterate (* (GF (X ^^ 1))) (one :: GF fp deg)

-- helper that computes trace via brute force: sum frobenius
-- automorphisms
trace' :: (GFCtx fp deg) => GF fp deg -> fp
trace' e = let (p,d) = witness sizePP e
               (GF t) = sum $ take d $ iterate (^p) e
               -- t is a constant polynomial
           in head $ coeffs t

-- | Represents fields over which we can get irreducible
-- polynomials of desired degrees.  (An instance of this class is
-- defined in 'Crypto.Lol.Types.IrreducibleChar2' and exported from
-- 'Crypto.Lol'.)
class Field fp => IrreduciblePoly fp where
  irreduciblePoly :: (Reflects deg Int) => Tagged deg (Polynomial fp)

-- | Convenience data type for writing 'IrreduciblePoly' instances.
data X = X

-- | Convenience function for writing 'IrreduciblePoly' instances.
(^^) :: Ring a => X -> Int -> Polynomial a
X ^^ i | i >= 0 = fromCoeffs $ replicate i 0 ++ [1]
_ ^^ _ = error "FiniteField.(^^) only defined for non-negative exponents."
