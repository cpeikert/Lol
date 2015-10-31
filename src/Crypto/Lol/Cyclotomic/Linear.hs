{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             GeneralizedNewtypeDeriving, KindSignatures,
             MultiParamTypeClasses, NoImplicitPrelude, RoleAnnotations,
             ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

-- | Functions from one cyclotomic ring to another that are linear
-- over a common subring.

module Crypto.Lol.Cyclotomic.Linear
( Linear, ExtendLinIdx
, linearDec, evalLin, extendLin
) where

import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.LatticePrelude

import Algebra.Additive as Additive (C)

import Control.Applicative
import Control.DeepSeq

-- | An @E@-linear function from @R@ to @S@.
newtype Linear t z (e::Factored) (r::Factored) (s::Factored) = D [Cyc t s z]
  deriving (NFData)

-- TODO: have constructor for both relative Pow basis of R/E?

-- some params are phantom but matter for safety
type role Linear representational nominal representational representational nominal

-- | Construct an @E@-linear function given a list of its output values
-- (in @S@) on the relative decoding basis of @R/E@.  The number of
-- elements in the list must not exceed the size of the basis.
linearDec :: forall t z e r s .
             (e `Divides` r, e `Divides` s, CElt t z)
             => [Cyc t s z] -> Linear t z e r s
linearDec cs = let ps = proxy powBasis (Proxy::Proxy e) `asTypeOf` cs
               in if length cs <= length ps then D (adviseCRT <$> cs)
               else error $ "linearDec: too many entries: "
                           ++ show (length cs) ++ " versus "
                           ++ show (length ps)

-- | Evaluates the given linear function on the input.
evalLin :: forall t z e r s .
           (e `Divides` r, e `Divides` s, CElt t z)
           => Linear t z e r s -> Cyc t r z -> Cyc t s z
evalLin (D cs) r = sum (zipWith (*) cs $
                        embed <$> (coeffsCyc Dec r :: [Cyc t e z]))

instance Additive (Cyc t s z) => Additive.C (Linear t z e r s) where
  zero = D []

  (D as) + (D bs) = D $ sumall as bs
    where sumall [] ys = ys
          sumall xs [] = xs
          sumall (x:xs) (y:ys) = x+y : sumall xs ys

  negate (D as) = D $ negate <$> as

instance (Reduce z zq, Fact s, CElt t z, CElt t zq)
         => Reduce (Linear t z e r s) (Linear t zq e r s) where
  reduce (D cs) = D $ reduce <$> cs

instance (CElt t zp, CElt t z, z ~ LiftOf zp, Lift zp z, Fact s)
         => Lift' (Linear t zp e r s) where
  type LiftOf (Linear t zp e r s) = Linear t (LiftOf zp) e r s

  lift (D cs) = D $ liftCyc Dec <$> cs

-- | A convenient constraint synonym for extending a linear function
-- to larger rings.
type ExtendLinIdx e r s e' r' s' =
  (e ~ FGCD r e', r' ~ FLCM r e', -- these imply R'=R\otimes_E E'
   e' ~ (e * (r' / r)), -- just to help GHC. This is implied by previous two constraints
   e' `Divides` s', s `Divides` s', -- these imply lcm(s,e')|s' <==> (S+E') \subseteq S'
   Fact r) -- need Fact r because nothing else gives it

-- | Extend an @E@-linear function @R->S@ to an @E'@-linear function
-- @R\'->S\'@.  (Mathematically, such extension only requires
-- @lcm(r,e\') | r\'@ (not equality), but this generality would
-- significantly complicate the implementation, and for our purposes
-- there's no reason to use any larger @r'@.)
extendLin :: (ExtendLinIdx e r s e' r' s', CElt t z)
           => Linear t z e r s -> Linear t z e' r' s'
-- CJP: this simple implementation works because R/E and R'/E' have
-- identical decoding bases, because R' \cong R \otimes_E E'.  If we
-- relax the constraint on E then we'd have to change the
-- implementation to something more difficult.
extendLin (D cs) = D (embed <$> cs)
