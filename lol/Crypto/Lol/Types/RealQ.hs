{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Crypto.Lol.Types.RealQ (RealQ) where

import Algebra.Additive as Additive (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
import Control.DeepSeq

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Reflects

-- for the Elt instance
import qualified Data.Array.Repa.Eval as E
import Data.Serialize
-- for the Unbox instances
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

newtype RealQ q r i = RealQ r
  deriving (Eq, Ord, ZeroTestable.C, E.Elt, Show, NFData, Storable, Read, Serialize)

{-# INLINABLE reduce' #-}
reduce' :: forall q r i . 
  (Reflects q i, ToInteger i, IntegralDomain r) 
  => r -> RealQ q r i
reduce' = RealQ . (`mod` (fromIntegral (proxy value (Proxy::Proxy q) :: i)))

-- puts value in range [-q/2, q/2)
decode' :: forall q r i . 
  (Reflects q i, Ord r, Additive r, ToInteger i, Ring r)
  => RealQ q r i -> r
decode' = let qval = fromIntegral (proxy value (Proxy::Proxy q) :: i)
          in \(RealQ x) -> if x + x < qval
                           then x
                           else x - qval

instance (Reflects q i, Additive (RealQ q r i), Additive r, 
          ToInteger i, IntegralDomain r) 
  => Reduce r (RealQ q r i) where
  reduce = reduce'

type instance LiftOf (RealQ q r i) = r

instance (Reflects q i, Reduce r (RealQ q r i), Ord r, ToInteger i, Ring r) 
  => Lift' (RealQ q r i) where
  lift = decode'

-- convenience synonym for many instances
type ReflectsTI q r = (Reflects q r, Ord r, IntegralDomain r)

instance (Reflects q i, Additive (RealQ q r i), ToInteger i) 
  => Mod (RealQ q r i) where
  type ModRep (RealQ q r i) = i

  modulus = retag (value :: Tagged q i)

-- instance of Additive
instance (Reflects q i, Ring r, Ord r, ToInteger i, IntegralDomain r) 
  => Additive.C (RealQ q r i) where

  {-# INLINABLE zero #-}
  zero = RealQ zero

  {-# INLINABLE (+) #-}
  (+) = let qval = fromIntegral (proxy value (Proxy::Proxy q) :: i)
        in \ (RealQ x) (RealQ y) ->
        let z = x + y
        in RealQ (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (RealQ x) = reduce' $ negate x

-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (RealQ q r i) = MV_RealQ (U.MVector s r)
newtype instance U.Vector (RealQ q r i) = V_RealQ (U.Vector r)

-- Unbox, when underlying representation is
instance U.Unbox r => U.Unbox (RealQ q r i)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox r => M.MVector U.MVector (RealQ q r i) where
  basicLength (MV_RealQ v) = M.basicLength v
  basicUnsafeSlice z n (MV_RealQ v) = MV_RealQ $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_RealQ v1) (MV_RealQ v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_RealQ v) = M.basicInitialize v
  basicUnsafeNew n = MV_RealQ <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (RealQ x) = MV_RealQ <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_RealQ v) z = RealQ <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_RealQ v) z (RealQ x) = M.basicUnsafeWrite v z x
  basicClear (MV_RealQ v) = M.basicClear v
  basicSet (MV_RealQ v) (RealQ x) = M.basicSet v x
  basicUnsafeCopy (MV_RealQ v1) (MV_RealQ v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_RealQ v1) (MV_RealQ v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_RealQ v) n = MV_RealQ <$> M.basicUnsafeGrow v n

instance U.Unbox r => G.Vector U.Vector (RealQ q r i) where
  basicUnsafeFreeze (MV_RealQ v) = V_RealQ <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_RealQ v) = MV_RealQ <$> G.basicUnsafeThaw v
  basicLength (V_RealQ v) = G.basicLength v
  basicUnsafeSlice z n (V_RealQ v) = V_RealQ $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_RealQ v) z = RealQ <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_RealQ mv) (V_RealQ v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
