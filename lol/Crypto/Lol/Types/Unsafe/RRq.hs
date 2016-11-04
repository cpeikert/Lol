{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             PolyKinds, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

-- | \( \def\Z{\mathbb{Z}} \)
--   \( \def\R{\mathbb{R}} \)
-- An implementation of the additive quotient group \(\R/(q\Z)\).

module Crypto.Lol.Types.RRq
( RRq(..)
) where

import Algebra.Additive     as Additive (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.DeepSeq

import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

-- invariant: 0 <= x < q
-- | The ring \(\R/(q\Z)\) of reals modulo 'q', using
-- underlying floating type 'r'.
newtype RRq q r = RRq r
    deriving (Eq, Ord, ZeroTestable.C, Show, NFData)

{-# INLINABLE reduce' #-}
reduce' :: forall q r . (Reflects q r, RealField r) => r -> RRq q r
reduce' = let q = proxy value (Proxy::Proxy q)
          in \x -> RRq $ x - q * floor (x / q)

-- puts value in range [-q/2, q/2)
decode' :: forall q r . (Reflects q r, Ord r, Ring r)
           => RRq q r -> r
decode' = let qval = proxy value (Proxy::Proxy q)
          in \(RRq x) -> if x + x < qval then x else x - qval

instance (Reflects q r, RealField r, Additive (RRq q r))
  => Reduce r (RRq q r) where
  reduce = reduce'

type instance LiftOf (RRq q r) = r

instance (Reflects q r, Reduce r (RRq q r), Ord r, Ring r)
  => Lift' (RRq q r) where
  lift = decode'

-- instance of Additive
instance (Reflects q r, RealField r, Ord r) => Additive.C (RRq q r) where

  {-# INLINABLE zero #-}
  zero = RRq zero

  {-# INLINABLE (+) #-}
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (RRq x) (RRq y) ->
        let z = x + y
        in RRq (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (RRq x) = reduce' $ negate x

instance (ToInteger i, RealField r, Reflects q i, Reflects q r)
  => Subgroup (ZqBasic q i) (RRq q r) where
  fromSubgroup = reduce' . fromIntegral . lift
