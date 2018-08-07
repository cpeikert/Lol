{-|
Module      : Crypto.Lol.Types.IZipVector
Description : Provides applicative-like functions for indexed vectors.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

Provides applicative-like functions for indexed vectors.
-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RoleAnnotations            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Lol.Types.IZipVector
( IZipVector, iZipVector, unIZipVector, unzipIZV
) where

import Crypto.Lol.Prelude              as LP
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Proto
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic

import Crypto.Proto.Lol.K
import Crypto.Proto.Lol.Kq
import Crypto.Proto.Lol.KqProduct
import Crypto.Proto.Lol.R
import Crypto.Proto.Lol.Rq
import Crypto.Proto.Lol.RqProduct

import Algebra.ZeroTestable as ZeroTestable

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Except

import Data.Constraint  hiding ((***))
import Data.Foldable    as F
import Data.Sequence    as S
import Data.Traversable

import           Data.Vector (Vector)
import qualified Data.Vector as V


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
iZipVector = let n = totientFact @m
            in \vec -> if n == V.length vec
                       then Just $ IZipVector vec
                       else Nothing

-- | Unzip an IZipVector.
unzipIZV :: IZipVector m (a,b) -> (IZipVector m a, IZipVector m b)
unzipIZV (IZipVector v) = let (va,vb) = V.unzip v
                          in (IZipVector va, IZipVector vb)

zipIZV :: IZipVector m a -> IZipVector m b -> IZipVector m (a,b)
zipIZV (IZipVector a) (IZipVector b) = IZipVector $ V.zip a b

-- don't export
repl :: forall m a . (Fact m) => a -> IZipVector m a
repl = let n = totientFact @m
       in IZipVector . V.replicate n

-- Zip-py 'Applicative' instance.
instance (Fact m) => Applicative (IZipVector m) where
  pure = repl
  (IZipVector f) <*> (IZipVector a) = IZipVector $ V.zipWith ($) f a

-- no ZeroTestable instance for Vectors, so define here
instance (ZeroTestable.C a) => ZeroTestable.C (Vector a) where
  isZero = V.all isZero

instance (Fact m) => Protoable (IZipVector m Int64) where
  type ProtoType (IZipVector m Int64) = R

  toProto (IZipVector xs') =
    let m = fromIntegral $ valueFact @m
        xs = S.fromList $ V.toList xs'
    in R{..}

  fromProto R{..} = do
    let m' = valueFact @m :: Int
        n  = totientFact @m
        ys' = V.fromList $ F.toList xs
        len = F.length xs
    unless (m' == fromIntegral m) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected m=" ++ show m' ++ ", got " ++ show m
    unless (len == n) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected n=" ++ show n  ++ ", got " ++ show len
    return $ IZipVector ys'

instance (Fact m) => Protoable (IZipVector m Double) where
  type ProtoType (IZipVector m Double) = K

  toProto (IZipVector xs') =
    let m = fromIntegral $ valueFact @m
        xs = S.fromList $ V.toList xs'
    in K{..}

  fromProto K{..} = do
    let m' = valueFact @m :: Int
        n  = totientFact @m
        ys' = V.fromList $ F.toList xs
        len = F.length xs
    unless (m' == fromIntegral m) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected m=" ++ show m' ++ ", got " ++ show m
    unless (len == n) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected n=" ++ show n  ++ ", got " ++ show len
    return $ IZipVector ys'

instance (Fact m, Reflects q Int64) => Protoable (IZipVector m (ZqBasic q Int64)) where
  type ProtoType (IZipVector m (ZqBasic q Int64)) = RqProduct

  toProto (IZipVector xs') =
    let m = fromIntegral $ valueFact @m
        q = fromIntegral (value @q :: Int64)
        xs = S.fromList $ V.toList $ V.map LP.lift xs'
    in RqProduct $ S.singleton Rq{..}

  fromProto (RqProduct xs') = do
    let rqs = F.toList xs'
        m' = valueFact @m :: Int
        q' = value @q :: Int64
        n  = totientFact @m
    unless (F.length rqs == 1) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected one Rq, but list has length " ++ show (F.length rqs)
    let [Rq{..}] = rqs
        ys' = V.fromList $ F.toList xs
        len = F.length xs
    unless (m' == fromIntegral m) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected m=" ++ show m' ++ ", got " ++ show m
    unless (len == n) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected n=" ++ show n  ++ ", got " ++ show len
    unless (fromIntegral q' == q) $ throwError $
        "An error occurred while reading the proto type for CT.\n\
        \Expected q=" ++ show q' ++ ", got " ++ show q
    return $ IZipVector $ V.map reduce ys'

instance (Fact m, Reflects q Double) => Protoable (IZipVector m (RRq q Double)) where
  type ProtoType (IZipVector m (RRq q Double)) = KqProduct

  toProto (IZipVector xs') =
    let m = fromIntegral $ valueFact @m
        q = round (value @q :: Double)
        xs = S.fromList $ V.toList $ V.map LP.lift xs'
    in KqProduct $ S.singleton Kq{..}

  fromProto (KqProduct xs') = do
    let rqs = F.toList xs'
        m' = valueFact @m :: Int
        q' = round (value @q :: Double)
        n  = totientFact @m
    unless (F.length rqs == 1) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected one Rq, but list has length " ++ show (F.length rqs)
    let [Kq{..}] = rqs
        ys' = V.fromList $ F.toList xs
        len = F.length xs
    unless (m' == fromIntegral m) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected m=" ++ show m' ++ ", got " ++ show m
    unless (len == n) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected n=" ++ show n  ++ ", got " ++ show len
    unless (q' == q) $ throwError $
      "An error occurred while reading the proto type for CT.\n\
      \Expected q=" ++ show q' ++ ", got " ++ show q
    return $ IZipVector $ V.map reduce ys'

instance (Protoable (IZipVector m (ZqBasic q Int64)),
          ProtoType (IZipVector m (ZqBasic q Int64)) ~ RqProduct,
          Protoable (IZipVector m b), ProtoType (IZipVector m b) ~ RqProduct)
  => Protoable (IZipVector m (ZqBasic q Int64,b)) where
  type ProtoType (IZipVector m (ZqBasic q Int64, b)) = RqProduct

  toProto = toProtoProduct RqProduct rqs
  fromProto = fromProtoNestRight RqProduct rqs

instance (Protoable (IZipVector m (RRq q Double)),
          ProtoType (IZipVector m (RRq q Double)) ~ KqProduct,
          Protoable (IZipVector m b), ProtoType (IZipVector m b) ~ KqProduct)
  => Protoable (IZipVector m (RRq q Double,b)) where
  type ProtoType (IZipVector m (RRq q Double, b)) = KqProduct

  toProto = toProtoProduct KqProduct kqs
  fromProto = fromProtoNestRight KqProduct kqs

toProtoProduct :: forall m a b c .
  (Protoable (IZipVector m a), Protoable (IZipVector m b),
   ProtoType (IZipVector m a) ~ ProtoType (IZipVector m b))
  => (Seq c -> ProtoType (IZipVector m a))
  -> (ProtoType (IZipVector m a) -> Seq c)
  -> IZipVector m (a,b)
  -> ProtoType (IZipVector m a)
toProtoProduct box unbox xs =
  let (as,bs) = unzipIZV xs
      as' = unbox $ toProto as
      bs' = unbox $ toProto bs
  in box $ as' >< bs'

-- for tuples like (a, (b, c))
fromProtoNestRight ::
  (MonadError String mon,
   Protoable (IZipVector m a), Protoable (IZipVector m b),
   ProtoType (IZipVector m a) ~ ProtoType (IZipVector m b))
  => (Seq c -> ProtoType (IZipVector m a))
  -> (ProtoType (IZipVector m a)-> Seq c)
  -> ProtoType (IZipVector m a)
  -> mon (IZipVector m (a,b))
fromProtoNestRight box unbox xs = do
  let ys = unbox xs
  unless (F.length ys >= 2) $ throwError $
    "Expected list of length >= 2, received list of length " ++ show (F.length ys)
  let (a :< bs) = viewl ys
  a' <- fromProto $ box $ singleton a
  bs' <- fromProto $ box bs
  return $ zipIZV a' bs'

instance ForallFact2 Protoable IZipVector Int64 where
  entailFact2 = Sub Dict

instance Reflects q Int64 => ForallFact2 Protoable IZipVector (ZqBasic q Int64) where
  entailFact2 = Sub Dict

instance ForallFact2 Protoable IZipVector Double where
  entailFact2 = Sub Dict

instance Reflects q Double => ForallFact2 Protoable IZipVector (RRq q Double) where
  entailFact2 = Sub Dict
