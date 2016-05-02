{-# LANGUAGE ConstraintKinds, DataKinds, DeriveTraversable,
             FlexibleContexts, GeneralizedNewtypeDeriving, KindSignatures,
             MultiParamTypeClasses, RoleAnnotations, ScopedTypeVariables,
             TypeFamilies, UndecidableInstances #-}

-- | Provides applicative-like functions for indexed vectors

module Crypto.Lol.Types.IZipVector
( IZipVector, iZipVector, unIZipVector, unzipIZV
) where

import Crypto.Lol.Factored

import Algebra.ZeroTestable as ZeroTestable

import Control.DeepSeq
import Data.Data
import Data.Functor.Trans.Tagged

import Data.Vector as V


-- | Indexed Zip Vector: a wrapper around a (boxed) 'Vector' that has
-- zip-py 'Applicative' behavior, analogous to
-- 'Control.Applicative.ZipList' for lists.  The index @m@ enforces
-- proper lengths (and is necessary to implement 'pure').

newtype IZipVector (m :: Factored) a =
  IZipVector { -- | Deconstructor
               unIZipVector :: Vector a}
  -- not deriving Read, Monoid, Alternative, Monad[Plus], IsList
  -- because of different semantics and/or length restriction
  deriving (Show, Eq, NFData, Functor, Foldable, Traversable, ZeroTestable.C)

-- the first argument, though phantom, affects representation
type role IZipVector representational representational

-- | Smart constructor that checks whether length of input is right
-- (should be totient of @m@).
iZipVector :: forall m a . (Fact m) => Vector a -> Maybe (IZipVector m a)
iZipVector = let n = proxy totientFact (Proxy::Proxy m)
            in \vec -> if n == V.length vec
                       then Just $ IZipVector vec
                       else Nothing

-- | Unzip an IZipVector.
unzipIZV :: IZipVector m (a,b) -> (IZipVector m a, IZipVector m b)
unzipIZV (IZipVector v) = let (va,vb) = V.unzip v
                          in (IZipVector va, IZipVector vb)

-- don't export
repl :: forall m a . (Fact m) => a -> IZipVector m a
repl = let n = proxy totientFact (Proxy::Proxy m)
       in IZipVector . V.replicate n

-- Zip-py 'Applicative' instance.
instance (Fact m) => Applicative (IZipVector m) where
  pure = repl
  (IZipVector f) <*> (IZipVector a) = IZipVector $ V.zipWith ($) f a

-- no ZeroTestable instance for Vectors, so define here
instance (ZeroTestable.C a) => ZeroTestable.C (Vector a) where
  isZero = V.all isZero
