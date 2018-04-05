{-|
Module      : Crypto.Lol.Cyclotomic.Tensor.CPP.Backend
Description : Transforms Haskell types into C counterparts.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
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
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Crypto.Lol.Cyclotomic.Tensor.CPP.Backend
( Dispatch
, dcrt, dcrtinv
, dgaussdec
, dl, dlinv
, dnorm
, dmulgpow, dmulgdec
, dginvpow, dginvdec
, dmul
,dcrtZq,dcrtinvZq
,dlZq,dlinvZq
,dnormZq
,dmulgpowZq,dmulgdecZq
,dginvpowZq,dginvdecZq
,dmulZq
, marshalFactors
, CPP
, withArray, withPtrArray
) where

import Crypto.Lol.Prelude       as LP (Complex, PP, Proxy (..), Tagged,
                                       map, mapM_, proxy, tag)
import Crypto.Lol.Reflects
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic

import Data.Int
import Data.Vector.Storable          as SV (Vector, fromList,
                                            unsafeToForeignPtr0)
import Data.Vector.Storable.Internal (getPtr)

import           Foreign.ForeignPtr      (touchForeignPtr)
import           Foreign.Marshal.Array   (withArray)
import           Foreign.Marshal.Utils   (with)
import           Foreign.Ptr             (Ptr, castPtr, plusPtr)
import           Foreign.Storable        (Storable (..))

import GHC.TypeLits -- for error message

-- | Convert a list of prime powers to a suitable C representation.
marshalFactors :: [PP] -> Vector CPP
marshalFactors = SV.fromList . LP.map (\(p,e) -> (fromIntegral p, fromIntegral e))

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

data ZqB64D -- for type safety purposes
data ComplexD
data DoubleD
data Int64D
data RRqD

type family CTypeOf x where
  CTypeOf (ZqBasic (q :: k) Int64) = ZqB64D
  CTypeOf Double = DoubleD
  CTypeOf Int64 = Int64D
  CTypeOf (Complex Double) = ComplexD
  CTypeOf (RRq (q :: k) Double) = RRqD

  -- EAC: See #12237 and #11990
  CTypeOf (ZqBasic (q :: k) i) = TypeError (Text "Unsupported C type: " :<>: ShowType (ZqBasic q i) :$$: Text "Use Int64 as the base ring")
  CTypeOf (Complex i) = TypeError (Text "Unsupported C type: " :<>: ShowType (Complex i) :$$: Text "Use Double as the base ring")
  CTypeOf (RRq (q :: k) i) = TypeError (Text "Unsupported C type: " :<>: ShowType (RRq q i) :$$: Text "Use Double as the base ring")
  CTypeOf a = TypeError (Text "Unsupported C type: " :<>: ShowType a)

type family EqCType a b c d where
  EqCType a b ZqB64D ZqB64D = ZqB64D
  EqCType a b RRqD RRqD = RRqD
  EqCType a b ComplexD ComplexD = ComplexD
  EqCType a b c c = TypeError (Text "Cannot call C code on a tuple of type " :<>: ShowType a)
  EqCType a b c d = TypeError (Text "You are trying to use CTensor on a tuple," :<>:
                           Text " but the tuple contains two different C types: " :$$:
                           ShowType a :<>: Text " and " :<>: ShowType b)

-- returns the modulus as a nested list of moduli
class (Tuple a) => ZqTuple a where
  type ModPairs a
  getModuli :: Tagged a (ModPairs a)

-- TODO: Kill all this pair crap

instance (Reflects q Int64) => ZqTuple (ZqBasic q Int64) where
  type ModPairs (ZqBasic q Int64) = Int64
  getModuli = tag $ proxy value (Proxy::Proxy q)

instance (Reflects q r, RealFrac r) => ZqTuple (RRq q r) where
  type ModPairs (RRq q r) = Int64
  getModuli = tag $ round (proxy value (Proxy::Proxy q) :: r)

instance (ZqTuple a, ZqTuple b) => ZqTuple (a, b) where
  type ModPairs (a,b) = (ModPairs a, ModPairs b)
  getModuli =
    let as = proxy getModuli (Proxy::Proxy a)
        bs = proxy getModuli (Proxy :: Proxy b)
    in tag (as,bs)

-- counts components in a nested tuple
class Tuple a where
  numComponents :: Tagged a Int16

instance {-# Overlappable #-} Tuple a where
  numComponents = tag 1

instance (Tuple a, Tuple b) => Tuple (a,b) where
  numComponents = tag $ proxy numComponents (Proxy::Proxy a) + proxy numComponents (Proxy::Proxy b)

-- TODO: Fix docs
-- | Class to safely match Haskell types with the appropriate C function.
class Dispatch r where
  -- | Equivalent to 'Tensor's @crt@.
  dcrt      :: Ptr (Ptr r) ->           Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @crtInv@.
  dcrtinv   :: Ptr (Ptr r) -> Ptr r ->  Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @tGaussianDec@.
  dgaussdec :: Ptr (Ptr (Complex r)) -> Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @l@.
  dl        :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @lInv@.
  dlinv     :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @gSqNormDec@.
  dnorm     :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @mulGPow@.
  dmulgpow  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @mulGDec@.
  dmulgdec  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO ()
  -- | Equivalent to 'Tensor's @divGPow@.
  dginvpow  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO Int16
  -- | Equivalent to 'Tensor's @divGDec@.
  dginvdec  :: Ptr r -> Int64 -> Ptr CPP -> Int16 -> IO Int16
  -- | Equivalent to @zipWith (*)@
  dmul :: Ptr r -> Ptr r -> Int64 -> IO ()

-- TODO: kill dgaussdec where it doesn't make sense

dcrtZq :: forall q . Reflects q Int64 => Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtZq ruptr pout totm pfac numFacts =
  tensorCRTRq (castPtr pout) totm pfac numFacts (castPtr ruptr) (proxy value (Proxy::Proxy q))

dcrtinvZq :: forall q . Reflects q Int64 => Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) ->  Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dcrtinvZq ruptr minv pout totm pfac numFacts =
  tensorCRTInvRq (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv) (proxy value (Proxy::Proxy q))

dlZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlZq pout totm pfac numFacts =
  tensorLRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dlinvZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dlinvZq pout totm pfac numFacts =
  tensorLInvRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dnormZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dnormZq = error "cannot call CT normSq on type ZqBasic"

dmulgpowZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgpowZq pout totm pfac numFacts =
  tensorGPowRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dmulgdecZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO ()
dmulgdecZq pout totm pfac numFacts =
  tensorGDecRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dginvpowZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvpowZq pout totm pfac numFacts =
  tensorGInvPowRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dginvdecZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> IO Int16
dginvdecZq pout totm pfac numFacts =
  tensorGInvDecRq (castPtr pout) totm pfac numFacts (proxy value (Proxy::Proxy q))

dmulZq :: forall q . Reflects q Int64 => Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> IO ()
dmulZq aout bout totm =
  mulRq (castPtr aout) (castPtr bout) totm (proxy value (Proxy::Proxy q))

-- products of Complex correspond to CRTExt of a Zq product
instance Dispatch (Complex Double) where
  dcrt ruptr pout totm pfac numFacts =
    tensorCRTC (castPtr pout) totm pfac numFacts (castPtr ruptr)
  dcrtinv ruptr minv pout totm pfac numFacts =
    tensorCRTInvC (castPtr pout) totm pfac numFacts (castPtr ruptr) (castPtr minv)
  dl pout =
    tensorLC (castPtr pout)
  dlinv pout =
    tensorLInvC (castPtr pout)
  dnorm = error "cannot call CT normSq on type Complex Double"
  dmulgpow pout =
    tensorGPowC (castPtr pout)
  dmulgdec pout =
    tensorGDecC (castPtr pout)
  dginvpow pout =
    tensorGInvPowC (castPtr pout)
  dginvdec pout =
    tensorGInvDecC (castPtr pout)
  dmul aout bout =
    mulC (castPtr aout) (castPtr bout)
  dgaussdec = error "cannot call CT gaussianDec on type Complex Double"

-- no support for products of Double
instance Dispatch Double where
  dcrt = error "cannot call CT Crt on type Double"
  dcrtinv = error "cannot call CT CrtInv on type Double"
  dl pout =
    tensorLDouble (castPtr pout)
  dlinv pout =
    tensorLInvDouble (castPtr pout)
  dnorm pout = tensorNormSqD 1 (castPtr pout)
  dmulgpow = error "cannot call CT mulGPow on type Double"
  dmulgdec = error "cannot call CT mulGDec on type Double"
  dginvpow = error "cannot call CT divGPow on type Double"
  dginvdec = error "cannot call CT divGDec on type Double"
  dmul = error "cannot call CT (*) on type Double"
  dgaussdec ruptr pout totm pfac numFacts =
    tensorGaussianDec (castPtr pout) totm pfac numFacts (castPtr ruptr)

-- no support for products of Z
instance Dispatch Int64 where
  dcrt = error "cannot call CT Crt on type Int64"
  dcrtinv = error "cannot call CT CrtInv on type Int64"
  dl pout =
    tensorLR (castPtr pout)
  dlinv pout =
    tensorLInvR (castPtr pout)
  dnorm pout =
    tensorNormSqR 1 (castPtr pout)
  dmulgpow pout =
    tensorGPowR (castPtr pout)
  dmulgdec pout =
    tensorGDecR (castPtr pout)
  dginvpow pout =
    tensorGInvPowR (castPtr pout)
  dginvdec pout =
    tensorGInvDecR (castPtr pout)
  dmul = error "cannot call CT (*) on type Int64"
  dgaussdec = error "cannot call CT gaussianDec on type Int64"

foreign import ccall unsafe "tensorLR" tensorLR ::                  Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvR" tensorLInvR ::            Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLRq" tensorLRq ::                Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorLInvRq" tensorLInvRq ::          Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorLDouble" tensorLDouble ::       Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvDouble" tensorLInvDouble :: Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLC" tensorLC ::       Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorLInvC" tensorLInvC :: Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorNormSqR" tensorNormSqR ::     Int16 -> Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorNormSqD" tensorNormSqD ::     Int16 -> Ptr Double -> Int64 -> Ptr CPP -> Int16          -> IO ()

foreign import ccall unsafe "tensorGPowR" tensorGPowR ::         Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGPowRq" tensorGPowRq ::       Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorGPowC" tensorGPowC ::         Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecR" tensorGDecR ::         Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGDecRq" tensorGDecRq ::       Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO ()
foreign import ccall unsafe "tensorGDecC" tensorGDecC ::         Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO ()
foreign import ccall unsafe "tensorGInvPowR" tensorGInvPowR ::   Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO Int16
foreign import ccall unsafe "tensorGInvPowRq" tensorGInvPowRq :: Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO Int16
foreign import ccall unsafe "tensorGInvPowC" tensorGInvPowC ::   Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO Int16
foreign import ccall unsafe "tensorGInvDecR" tensorGInvDecR ::   Ptr Int64 -> Int64 -> Ptr CPP -> Int16          -> IO Int16
foreign import ccall unsafe "tensorGInvDecRq" tensorGInvDecRq :: Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Int64 -> IO Int16
foreign import ccall unsafe "tensorGInvDecC" tensorGInvDecC ::   Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16          -> IO Int16

foreign import ccall unsafe "tensorCRTRq" tensorCRTRq ::         Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Int64 -> IO ()
foreign import ccall unsafe "tensorCRTC" tensorCRTC ::           Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> IO ()
foreign import ccall unsafe "tensorCRTInvRq" tensorCRTInvRq ::   Ptr (ZqBasic q Int64) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (ZqBasic q Int64)) -> Ptr (ZqBasic q Int64) -> Int64 -> IO ()
foreign import ccall unsafe "tensorCRTInvC" tensorCRTInvC ::     Ptr (Complex Double) -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) -> Ptr (Complex Double) -> IO ()

foreign import ccall unsafe "tensorGaussianDec" tensorGaussianDec :: Ptr Double -> Int64 -> Ptr CPP -> Int16 -> Ptr (Ptr (Complex Double)) ->  IO ()

foreign import ccall unsafe "mulRq" mulRq :: Ptr (ZqBasic q Int64) -> Ptr (ZqBasic q Int64) -> Int64 -> Int64 -> IO ()
foreign import ccall unsafe "mulC" mulC :: Ptr (Complex Double) -> Ptr (Complex Double) -> Int64 -> IO ()
