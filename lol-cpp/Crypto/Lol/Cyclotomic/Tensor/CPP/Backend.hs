{-|
Module      : Crypto.Lol.Cyclotomic.Tensor.CPP.Backend
Description : Transforms Haskell types into C counterparts.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2018
License     : GPL-3
Maintainer  : ecrockett0@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the functions to transform Haskell types into their
C counterpart, and to transform polymorphic Haskell functions into C funtion
calls in a type-safe way.
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Crypto.Lol.Cyclotomic.Tensor.CPP.Backend
( dcrtZq, dcrtinvZq
, dlZq, dlinvZq
, dmulgpowZq, dmulgdecZq
, dginvpowZq, dginvdecZq
, dmulZq
, dcrtC, dcrtinvC
, dlC, dlinvC
, dmulgpowC, dmulgdecC
, dginvpowC, dginvdecC
, dmulC
, dlDouble, dlinvDouble
, dmulgpowDouble, dmulgdecDouble
, dginvpowDouble, dginvdecDouble
, dgaussdecDouble
, dnormDouble
, dlRRq,dlinvRRq
, dlInt64, dlinvInt64
, dmulgpowInt64, dmulgdecInt64
, dginvpowInt64, dginvdecInt64
, dnormInt64
, marshalFactors
, CPP
, withArray, withPtrArray
) where

import Crypto.Lol.Prelude              as LP (Complex, PP, Proxy (..),
                                              Tagged, map, mapM_, proxy,
                                              tag)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic

import Control.Arrow                 ((***))
import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList,
                                            unsafeToForeignPtr0)
import Data.Vector.Storable.Internal (getPtr)

import Foreign.ForeignPtr    (touchForeignPtr)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr           (Ptr, castPtr, plusPtr)
import Foreign.Storable      (Storable (..))

-- | Convert a list of prime powers to a suitable C representation.
marshalFactors :: [PP] -> Vector CPP
marshalFactors = SV.fromList . LP.map (fromIntegral *** fromIntegral)

-- http://stackoverflow.com/questions/6517387/vector-vector-foo-ptr-ptr-foo-io-a-io-a
-- | Evaluates a C function that takes an "a** ptr" on a list of Vectors.
withPtrArray :: (Storable a) => [Vector a] -> (Ptr (Ptr a) -> IO b) -> IO b
withPtrArray v f = do
  let vs = LP.map SV.unsafeToForeignPtr0 v
      ptrV = LP.map (\(fp,_) -> getPtr fp) vs
  res <- withArray ptrV f
  LP.mapM_ (\(fp,_) -> touchForeignPtr fp) vs
  return res

-- Note: These types need to be the same, otherwise something goes wrong on the C end...
-- | C representation of a prime power.
type CPP = (Int16, Int16)

instance (Storable a, Storable b)
  => Storable (a,b) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)
  alignment _ = max (alignment (undefined :: a)) (alignment (undefined :: b))
  peek p = do
    a <- peek (castPtr p :: Ptr a)
    b <- peek (castPtr (plusPtr p (sizeOf a)) :: Ptr b)
    return (a,b)
  poke p (a,b) = do
    poke (castPtr p :: Ptr a) a
    poke (castPtr (plusPtr p (sizeOf a)) :: Ptr b) b

dcrtZq :: forall q . Reflects q Int64 => Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtZq ruptr pout totm pfac numFacts =
  tensorCRTRq (castPtr pout) totm pfac numFacts (castPtr ruptr) (value @q)

dcrtinvZq :: forall q . Reflects q Int64 => Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) ->  Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtinvZq ruptr minv pout totm pfac numFacts =
  tensorCRTInvRq (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv) (value @q)

dlZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlZq pout totm pfac numFacts =
  tensorLRq (castPtr pout) totm pfac numFacts (value @q)

dlinvZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvZq pout totm pfac numFacts =
  tensorLInvRq (castPtr pout) totm pfac numFacts (value @q)

dmulgpowZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgpowZq pout totm pfac numFacts =
  tensorGPowRq (castPtr pout) totm pfac numFacts (value @q)

dmulgdecZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgdecZq pout totm pfac numFacts =
  tensorGDecRq (castPtr pout) totm pfac numFacts (value @q)

dginvpowZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvpowZq pout totm pfac numFacts =
  tensorGInvPowRq (castPtr pout) totm pfac numFacts (value @q)

dginvdecZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvdecZq pout totm pfac numFacts =
  tensorGInvDecRq (castPtr pout) totm pfac numFacts (value @q)

dmulZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> IO ()
dmulZq aout bout totm =
  mulRq (castPtr aout) (castPtr bout) totm (value @q)

dcrtC :: Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtC ruptr pout totm pfac numFacts =
  tensorCRTC (castPtr pout) totm pfac numFacts (castPtr ruptr)

dcrtinvC :: Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtinvC ruptr minv pout totm pfac numFacts =
  tensorCRTInvC (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv)

dlC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlC pout = tensorLC (castPtr pout)

dlinvC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvC pout = tensorLInvC (castPtr pout)

dmulgpowC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgpowC pout = tensorGPowC (castPtr pout)

dmulgdecC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgdecC pout = tensorGDecC (castPtr pout)

dginvpowC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvpowC pout = tensorGInvPowC (castPtr pout)

dginvdecC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvdecC pout = tensorGInvDecC (castPtr pout)

dmulC :: Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()
dmulC aout bout = mulC (castPtr aout) (castPtr bout)

-- q is nominal. C++ never sees it, so it doesn't matter what it is
dlRRq :: forall q . Ptr (RRq q Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlRRq pout = tensorLRRq (castPtr pout)

-- q is nominal. C++ never sees it, so it doesn't matter what it is
dlinvRRq :: forall q . Ptr (RRq q Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvRRq pout = tensorLInvRRq (castPtr pout)

dlDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlDouble pout = tensorLDouble (castPtr pout)

dlinvDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvDouble pout = tensorLInvDouble (castPtr pout)

dnormDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dnormDouble pout = tensorNormSqD 1 (castPtr pout)

dmulgpowDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgpowDouble pout = tensorGPowDouble (castPtr pout)

dmulgdecDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgdecDouble pout = tensorGDecDouble (castPtr pout)

dginvpowDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvpowDouble pout = tensorGInvPowDouble (castPtr pout)

dginvdecDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvdecDouble pout = tensorGInvDecDouble (castPtr pout)

dgaussdecDouble :: Ptr (Ptr (Complex Double)) -> Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
dgaussdecDouble ruptr pout totm pfac numFacts =
  tensorGaussianDec (castPtr pout) totm pfac numFacts (castPtr ruptr)

dlInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlInt64 pout = tensorLR (castPtr pout)

dlinvInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvInt64 pout = tensorLInvR (castPtr pout)

dnormInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dnormInt64 pout = tensorNormSqR 1 (castPtr pout)

dmulgpowInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgpowInt64 pout = tensorGPowR (castPtr pout)

dmulgdecInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgdecInt64 pout = tensorGDecR (castPtr pout)

dginvpowInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvpowInt64 pout = tensorGInvPowR (castPtr pout)

dginvdecInt64 :: Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvdecInt64 pout = tensorGInvDecR (castPtr pout)

foreign import ccall unsafe "tensorLR" tensorLR ::                  Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::            Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLRq" tensorLRq ::                Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorLInvRq" tensorLInvRq ::          Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLRRq" tensorLRRq ::       Ptr (RRq q Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvRRq" tensorLInvRRq :: Ptr (RRq q Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLC" tensorLC ::       Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvC" tensorLInvC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorNormSqR" tensorNormSqR ::     Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorNormSqD" tensorNormSqD ::     Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorGPowR" tensorGPowR ::                 Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGPowRq" tensorGPowRq ::               Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorGPowDouble" tensorGPowDouble ::       Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGPowC" tensorGPowC ::                 Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGDecR" tensorGDecR ::                 Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGDecRq" tensorGDecRq ::               Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorGDecDouble" tensorGDecDouble ::       Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGDecC" tensorGDecC ::                 Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO ()
foreign import ccall unsafe "tensorGInvPowR" tensorGInvPowR ::           Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO Int16
foreign import ccall unsafe "tensorGInvPowRq" tensorGInvPowRq ::         Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO Int16
foreign import ccall unsafe "tensorGInvPowDouble" tensorGInvPowDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO Int16
foreign import ccall unsafe "tensorGInvPowC" tensorGInvPowC ::           Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
foreign import ccall unsafe "tensorGInvDecR" tensorGInvDecR ::           Ptr Int64 -> Int64 -> Ptr CPP -> Int16 -> IO Int16
foreign import ccall unsafe "tensorGInvDecRq" tensorGInvDecRq ::         Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO Int16
foreign import ccall unsafe "tensorGInvDecDouble" tensorGInvDecDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> IO Int16
foreign import ccall unsafe "tensorGInvDecC" tensorGInvDecC ::           Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> IO Int16

foreign import ccall unsafe "tensorCRTRq" tensorCRTRq ::         Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Int64 -> IO ()
foreign import ccall unsafe "tensorCRTC" tensorCRTC ::           Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> IO ()
foreign import ccall unsafe "tensorCRTInvRq" tensorCRTInvRq ::   Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) -> Int64 -> IO ()
foreign import ccall unsafe "tensorCRTInvC" tensorCRTInvC ::     Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> IO ()

foreign import ccall unsafe "tensorGaussianDec" tensorGaussianDec :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) ->  IO ()

foreign import ccall unsafe "mulRq" mulRq :: Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> Int64 -> IO ()
foreign import ccall unsafe "mulC" mulC :: Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()
