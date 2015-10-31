{-# LANGUAGE NoImplicitPrelude, RebindableSyntax, PolyKinds, TypeFamilies, 
             DataKinds, FlexibleContexts, ConstraintKinds #-}

-- | Prime-order fields.

module Crypto.Lol.Types.PrimeField where

import Crypto.Lol.LatticePrelude as LP
import Crypto.Lol.Reflects

import MathObj.Polynomial

-- | Constraint synonym for prime-order fields.
type PrimeField fp = (Enumerable fp, Eq fp, ZeroTestable fp, Field fp,
       IrreduciblePoly fp)

-- | The characteristic of a field, represented as a type.
type family CharOf (fp :: k) :: Nat

-- | Represents prime-order fields over which we can get irreducible
-- polynomials of desired degree.  (An instance of this class is
-- defined in 'Crypto.Lol.Types.IrreducibleChar2' and exported from
-- 'Crypto.Lol'.)
class (Ring fp, Prime (CharOf fp)) => IrreduciblePoly fp where
  irreduciblePoly :: (Reflects deg Int) => Tagged deg (Polynomial fp)

-- | Convenience function for writing 'IrreduciblePoly' instances.
taggedProxy :: Tagged s (Proxy s)
taggedProxy = tag Proxy

-- | Convenience data type for writing 'IrreduciblePoly' instances.
data X = X

-- | Convenience function for writing 'IrreduciblePoly' instances.
(^) :: (Ring a) => X -> Int -> Polynomial a
X ^ i | i >= 0 = fromCoeffs $ (replicate i 0) ++ [1]
