{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             NoImplicitPrelude, PolyKinds, RebindableSyntax,
             ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

-- | An implementation of the additive quotient group @RR/qZ@, where @RR@
-- denotes the real numbers.

module Crypto.Lol.Types.RRq
( RRq
) where

import Algebra.Additive     as Additive (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
import Control.DeepSeq

import Crypto.Lol.Prelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.ZqBasic

-- for the Elt instance
import qualified Data.Array.Repa.Eval as E

-- for the Unbox instances
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

-- invariant: 0 <= x < q
-- | The ring @RR_q@ of reals modulo 'q', using underlying floating
-- type 'r'.
newtype RRq q r = RRq r
    deriving (Eq, Ord, ZeroTestable.C, E.Elt, Show, NFData, Storable)

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

-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (RRq q r) = MV_RRq (U.MVector s r)
newtype instance U.Vector (RRq q r) = V_RRq (U.Vector r)

-- Unbox, when underlying representation is
instance U.Unbox r => U.Unbox (RRq q r)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox r => M.MVector U.MVector (RRq q r) where
  basicLength (MV_RRq v) = M.basicLength v
  basicUnsafeSlice z n (MV_RRq v) = MV_RRq $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_RRq v1) (MV_RRq v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_RRq v) = M.basicInitialize v
  basicUnsafeNew n = MV_RRq <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (RRq x) = MV_RRq <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_RRq v) z = RRq <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_RRq v) z (RRq x) = M.basicUnsafeWrite v z x
  basicClear (MV_RRq v) = M.basicClear v
  basicSet (MV_RRq v) (RRq x) = M.basicSet v x
  basicUnsafeCopy (MV_RRq v1) (MV_RRq v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_RRq v1) (MV_RRq v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_RRq v) n = MV_RRq <$> M.basicUnsafeGrow v n

instance U.Unbox r => G.Vector U.Vector (RRq q r) where
  basicUnsafeFreeze (MV_RRq v) = V_RRq <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_RRq v) = MV_RRq <$> G.basicUnsafeThaw v
  basicLength (V_RRq v) = G.basicLength v
  basicUnsafeSlice z n (V_RRq v) = V_RRq $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_RRq v) z = RRq <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_RRq mv) (V_RRq v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
