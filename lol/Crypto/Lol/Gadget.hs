{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses, NoImplicitPrelude,
             PolyKinds, ScopedTypeVariables, TupleSections, TypeFamilies,
             UndecidableInstances #-}

-- | Interfaces for "gadgets," decomposition, and error correction.

module Crypto.Lol.Gadget
( Gadget(..), Decompose(..), Correct(..)
, TrivGad, BaseBGad
) where

import Crypto.Lol.Prelude

import Control.Applicative
import Control.Arrow

-- | Dummy type representing the gadget @[1]@.
data TrivGad
-- | Dummy type representing the gadget @[1,b,b^2,...]@.
data BaseBGad b

-- | "Gadget" vectors, parameterized by an index type.

class Ring u => Gadget gad u where
  -- | The gadget vector over @u@.
  gadget :: Tagged gad [u]

  -- | Yield an error-tolerant encoding of an element with respect to
  -- the gadget.  (Mathematically, this should just be the product of
  -- the input with the gadget, but it is a class method to allow for
  -- optimized implementations.)
  encode :: u -> Tagged gad [u]
  encode s = ((* s) <$>) <$> gadget

-- | Decomposition relative to a gadget.

class (Gadget gad u, Reduce (DecompOf u) u) => Decompose gad u where
  -- | The ring that @u@ decomposes over.
  type DecompOf u

  -- | Yield a short vector @x@ such that @\<g, x\> = u@.
  decompose :: u -> Tagged gad [DecompOf u]

-- | Error correction relative to a gadget.

class Gadget gad u => Correct gad u where

  -- | Error-correct a "noisy" encoding of an element (see 'encode'),
  -- returning the encoded element and the error vector.
  correct :: Tagged gad [u] -> (u, [LiftOf u])


-- instances for products

instance (Gadget gad a, Gadget gad b) => Gadget gad (a,b) where

  gadget = (++) <$> (map (,zero) <$> gadget) <*> (map (zero,) <$> gadget)

instance (Decompose gad a, Decompose gad b, DecompOf a ~ DecompOf b)
         => Decompose gad (a,b) where

  type DecompOf (a,b) = DecompOf a
  decompose (a,b) = (++) <$> decompose a <*> decompose b

instance (Correct gad a, Correct gad b,
          Mod a, Mod b, Field a, Field b, Lift' a, Lift' b,
          ToInteger (LiftOf a), ToInteger (LiftOf b))
    => Correct gad (a,b) where

  correct =
    let gada = gadget :: Tagged gad [a]
        gadb = gadget :: Tagged gad [b]
        ka = length gada
        qaval = toInteger $ proxy modulus (Proxy::Proxy a)
        qbval = toInteger $ proxy modulus (Proxy::Proxy b)
        qamod = fromIntegral qaval
        qbmod = fromIntegral qbval
        qainv = recip qamod
        qbinv = recip qbmod
    in \tv ->
        let v = untag tv
            (wa,wb) = splitAt ka v
            (va,xb) = unzip $
                      (\(a,b) -> let x = toInteger $ lift b
                                 in (qbinv * (a - fromIntegral x), x)) <$> wa
            (vb,xa) = unzip $
                      (\(a,b) -> let x = toInteger $ lift a
                                 in (qainv * (b - fromIntegral x), x)) <$> wb
            (sa,ea) = (qbmod *) ***
                      zipWith (\x e -> x + qbval * toInteger e) xb $
                      correct (tag va `asTypeOf` gada)
            (sb,eb) = (qamod *) ***
                      zipWith (\x e -> x + qaval * toInteger e) xa $
                      correct (tag vb `asTypeOf` gadb)
        in ((sa,sb), ea ++ eb)


{- CJP: strawman class for the more general view of LWE secrets as
"module characters," i.e., module homomorphisms into a particular
range.  This is probably wrong, though.

class Character u where       -- Module superclass(es)?
  type CharRange u
  data Char u                   -- need data for injectivity

  evalChar :: Char u -> u -> CharRange u

class (Gadget gad u, Character u) => Correct gad u where

  -- | Correct a "noisy" encoding of an LWE secret (i.e., a
  -- 'ModuleHomom' on 'u').
  correct :: Tagged gad [CharRange u] -> Char u

encode :: (Correct gad u) => Char u -> Tagged gad [CharRange u]
encode s = pasteT $ evalMH s <$> peelT gadget

-}

