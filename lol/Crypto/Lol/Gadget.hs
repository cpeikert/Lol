{-|
Module      : Crypto.Lol.Gadget
Description : Interfaces for "gadgets," decomposition, and error correction.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Interfaces for "gadgets," decomposition, and error correction.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Gadget
( Gadget(..), Decompose(..), Correct(..)
, decomposeT, decomposeList, decomposeMatrix
, TrivGad, BaseBGad
) where

import Crypto.Lol.Prelude

import MathObj.Matrix hiding (one, zero, zipWith)

import Control.Applicative
import Control.Arrow
import Data.Traversable

-- | Dummy type representing the gadget \( [1] \).
data TrivGad
-- | Dummy type representing the gadget \( [1,b,b^2,\ldots] \).
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

  -- | Yield a short vector \( x \) such that \( \langle g, x\rangle = u \).
  decompose :: u -> Tagged gad [DecompOf u]

-- | Alternative to 'decompose'.
decomposeT :: Decompose gad u => u -> TaggedT gad [] (DecompOf u)
decomposeT = peelT . decompose

-- | Decompose a list entry-wise.
decomposeList :: Decompose gad u => [u] -> Tagged gad [DecompOf u]
decomposeList = fmap concat . traverse decompose

-- | Decompose a matrix entry-wise.
decomposeMatrix :: forall gad u . (Decompose gad u)
                   => Matrix u -> Tagged gad (Matrix (DecompOf u))
decomposeMatrix m = do
  l <- length <$> (gadget :: Tagged gad [u]) -- CJP: avoid scoped type vars?
  fromColumns (l * numRows m) (numColumns m) <$>
    traverse decomposeList (columns m)

-- | Error correction relative to a gadget.
class Gadget gad u => Correct gad u where

  -- | Error-correct a "noisy" encoding of an element (see 'encode'),
  -- returning the encoded element and the error vector.
  correct :: Tagged gad [u] -> (u, [LiftOf u])


-- | Product ring: concatenate gadgets over component rings
instance (Gadget gad a, Gadget gad b) => Gadget gad (a,b) where

  gadget = (++) <$> (map (,zero) <$> gadget) <*> (map (zero,) <$> gadget)

-- | Product ring: concatenate decompositions for component rings
instance (Decompose gad a, Decompose gad b, DecompOf a ~ DecompOf b)
         => Decompose gad (a,b) where

  type DecompOf (a,b) = DecompOf a
  decompose (a,b) = (++) <$> decompose a <*> decompose b

-- | Product ring
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

