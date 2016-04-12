{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, NoImplicitPrelude, PolyKinds, 
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}

module Crypto.Lol.Types.RealQ (RealQ, RealMod) where

import Algebra.Additive as Additive (C)
import Algebra.ZeroTestable as ZeroTestable (C)

import Control.Applicative
import Control.DeepSeq

import Crypto.Lol.CRTrans
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Proto.Coeffs
import Crypto.Lol.Types.Proto.RealQList

-- for the Elt instance
import qualified Data.Array.Repa.Eval as E
import Data.Foldable (toList)
import Data.Reflection
import Data.Serialize
import Data.Sequence as S (fromList)
-- for the Unbox instances
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U

import Foreign.Storable

-- invariant: 0 <= x < q
newtype RealQ q r = RealQ {unRealQ :: r}
  deriving (Eq, Ord, ZeroTestable.C, E.Elt, Show, NFData, Storable, Read, Serialize)

data RealMod q

instance (Reifies q i, ToInteger i, Ring r) => Reflects (RealMod (q :: *)) r where
  value = tag $ fromIntegral $ reflect (Proxy::Proxy q)

{-# INLINABLE reduce' #-}
reduce' :: forall q r . (Reflects q r, Field r, RealRing r)
  => r -> RealQ q r
reduce' x = 
  let q = proxy value (Proxy::Proxy q)
      y = floor $ x / q
  in RealQ $ x-q*y

-- puts value in range [-q/2, q/2)
decode' :: forall q r . 
  (Reflects q r, Ord r, Additive r, Ring r)
  => RealQ q r -> r
decode' = let qval = proxy value (Proxy::Proxy q)
          in \(RealQ x) -> if x + x < qval
                           then x
                           else x - qval

instance (Reflects q r, Field r, RealRing r, Additive (RealQ q r)) 
  => Reduce r (RealQ q r) where
  reduce = reduce'

type instance LiftOf (RealQ q r) = r

instance (Reflects q r, Reduce r (RealQ q r), Ord r, Ring r) 
  => Lift' (RealQ q r) where
  lift = decode'
{-
instance (Reflects q r, Additive (RealQ q r)) 
  => Mod (RealQ q r) where
  type ModRep (RealQ q r) = r

  modulus = retag (value :: Tagged q r)
-}
-- instance of Additive
instance (Reflects q r, RealRing r, Field r, Ord r) 
  => Additive.C (RealQ q r) where

  {-# INLINABLE zero #-}
  zero = RealQ zero

  {-# INLINABLE (+) #-}
  (+) = let qval = proxy value (Proxy::Proxy q)
        in \ (RealQ x) (RealQ y) ->
        let z = x + y
        in RealQ (if z >= qval then z - qval else z)

  {-# INLINABLE negate #-}
  negate (RealQ x) = reduce' $ negate x

instance (Reflects q Double) => Protoable [RealQ q Double] where
  type ProtoType [RealQ q Double] = Coeffs
  toProto xs = Rqs $ 
    RealQList (round (proxy value (Proxy::Proxy q) :: Double)) $ 
      fromList $ map unRealQ xs
  fromProto (Rqs (RealQList q' xs)) = 
    let q = round (proxy value (Proxy::Proxy q) :: Double)
    in if q == q'
       then map reduce $ toList xs
       else error $ "Mismatched q value in Protoable instance for RealQ. Expected " ++ (show q) ++ ", got " ++ (show q') ++ "."


-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (RealQ q r) = MV_RealQ (U.MVector s r)
newtype instance U.Vector (RealQ q r) = V_RealQ (U.Vector r)

-- Unbox, when underlying representation is
instance U.Unbox r => U.Unbox (RealQ q r)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox r => M.MVector U.MVector (RealQ q r) where
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

instance U.Unbox r => G.Vector U.Vector (RealQ q r) where
  basicUnsafeFreeze (MV_RealQ v) = V_RealQ <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_RealQ v) = MV_RealQ <$> G.basicUnsafeThaw v
  basicLength (V_RealQ v) = G.basicLength v
  basicUnsafeSlice z n (V_RealQ v) = V_RealQ $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_RealQ v) z = RealQ <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_RealQ mv) (V_RealQ v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
