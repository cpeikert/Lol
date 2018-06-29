{-# LANGUAGE TypeFamilies #-}

module Crypto.Lol.Types.IFunctor
where

import Crypto.Lol.Factored

import Data.Constraint

class IFunctor f where
  type IFElt f a :: Constraint

  fmapI :: (IFElt f a, IFElt f b, Fact m) => (a -> b) -> f m a -> f m b

  zipWithI :: (IFElt f a, IFElt f b, IFElt f c, Fact m)
    => (a -> b -> c) -> f m a -> f m b -> f m c

