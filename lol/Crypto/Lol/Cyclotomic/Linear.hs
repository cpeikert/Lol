{-|
Module      : Crypto.Lol.Cyclotomic.Linear
Description : Functions from one cyclotomic ring to another that are linear
              over a common subring.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

  \( \def\lcm{\text{lcm}} \)

Functions from one cyclotomic ring to another that are linear
over a common subring.
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Cyclotomic.Linear
( Linear, ExtendLinIdx
, linearDec, evalLin, extendLin
) where

import Crypto.Lol.Cyclotomic.Cyc
import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Proto.Lol.RqProduct (RqProduct)
import Crypto.Proto.Lol.LinearRq (LinearRq(LinearRq))

import Algebra.Additive as Additive (C)

import Control.Applicative
import Control.DeepSeq

import Data.Word

-- | An \(E\)-linear function from \(R\) to \(S\).

-- CJP: also have constructor for relative Pow basis of R/E?  So far
-- not needed.
newtype Linear t z (e::Factored) (r::Factored) (s::Factored) = RD [Cyc t s z]

deriving instance (NFData (Cyc t s z)) => NFData (Linear t z e r s)

-- some params are phantom but matter for safety
type role Linear representational nominal representational representational nominal

-- | Construct an \(E\)-linear function given a list of its output values
-- (in \(S\)) on the relative decoding basis of \(R/E\).  The number of
-- elements in the list must not exceed the size of the basis.
linearDec :: forall t z e r s .
             (e `Divides` r, e `Divides` s, CElt t z)
             => [Cyc t s z] -> Linear t z e r s
linearDec ys = let ps = proxy powBasis (Proxy::Proxy e) `asTypeOf` ys
               in if length ys <= length ps then RD (adviseCRT <$> ys)
               else error $ "linearDec: too many entries: "
                        ++ show (length ys) ++ " versus "
                        ++ show (length ps)

-- | Evaluates the given linear function on the input.
evalLin :: forall t z e r s .
           (e `Divides` r, e `Divides` s, CElt t z)
           => Linear t z e r s -> Cyc t r z -> Cyc t s z
evalLin (RD ys) r = sum (zipWith (*) ys $
                         embed <$> (coeffsDec r :: [Cyc t e z]))

instance Additive (Cyc t s z) => Additive.C (Linear t z e r s) where
  zero = RD []

  (RD as) + (RD bs) = RD $ sumall as bs
    where sumall [] ys = ys
          sumall xs [] = xs
          sumall (x:xs) (y:ys) = x+y : sumall xs ys

  negate (RD as) = RD $ negate <$> as

instance (Reduce z zq, Fact s, CElt t z, CElt t zq)
         => Reduce (Linear t z e r s) (Linear t zq e r s) where
  reduce (RD ys) = RD $ reduce <$> ys

type instance LiftOf (Linear t zp e r s) = Linear t (LiftOf zp) e r s

-- | lifts with respect to powerful basis, for best geometry
instance (CElt t zp, CElt t z, z ~ LiftOf zp, Lift zp z, Fact s)
         => Lift' (Linear t zp e r s) where
  lift (RD ys) = RD $ liftPow <$> ys

-- | A convenient constraint synonym for extending a linear function
-- to larger rings.
type ExtendLinIdx e r s e' r' s' =
  (Fact r, e ~ FGCD r e', r' ~ FLCM r e', -- these imply R'=R\otimes_E E'
   e' `Divides` s', s `Divides` s') -- lcm(s,e')|s' <=> (S+E') \subseteq S'

-- | Extend an \(E\)-linear function \(R\to S\) to an \(E'\)-linear
-- function \(R'\to S'\). (Mathematically, such extension only requires
-- \(\lcm(r,e') | r'\) (not equality), but this generality would
-- significantly complicate the implementation, and for our purposes
-- there's no reason to use any larger \(r'\).)
extendLin :: (ExtendLinIdx e r s e' r' s')
           => Linear t z e r s -> Linear t z e' r' s'
-- CJP: this simple implementation works because R/E and R'/E' have
-- identical decoding bases, because R' \cong R \otimes_E E'.  If we
-- relax the constraint on E then we'd have to change the
-- implementation to something more difficult.
extendLin (RD ys) = RD (embed <$> ys)

instance (Reflects e Word32, Reflects r Word32,
          Protoable (Cyc t s zq), ProtoType (t s zq) ~ RqProduct)
  => Protoable (Linear t zq e r s) where
  type ProtoType (Linear t zq e r s) = LinearRq
  toProto (RD cs) = LinearRq (proxy value (Proxy::Proxy e)) (proxy value (Proxy::Proxy r)) $ toProto cs
  fromProto (LinearRq e r cs) =
    let e' = proxy value (Proxy::Proxy e)
        r' = proxy value (Proxy::Proxy r)
    in if e == e' && r == r'
       then RD <$> fromProto cs
       else error $ "Could not deserialize Linear: types imply e=" ++
              show e' ++ " and r=" ++ show r' ++
              ", but serializd object is for e=" ++
              show e ++ " and r=" ++ show r
