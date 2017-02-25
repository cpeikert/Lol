{-|
Module      : Crypto.Lol.Cyclotomic.Tensor.Repa.Instances
Description : RT-specific instances.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-2
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

RT-specific instances.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Crypto.Lol.Cyclotomic.Tensor.Repa.Instances where

-- EAC: Do not import Crypto.Lol.Types, because it exports an IrreduciblePoly
-- instance which screw with GHC. Probably #10338.
import Crypto.Lol.Types.Unsafe.Complex
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic

import Data.Array.Repa.Eval     as R
import qualified Number.Complex as C hiding (exp, signum)

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed         as U
import Data.Vector.Unboxed.Deriving




instance (R.Elt a) => R.Elt (Complex a) where
    touch (Complex' c) = do
        touch $ C.real c
        touch $ C.imag c
    zero = Complex' $ R.zero C.+: R.zero
    one = Complex' $ R.one C.+: R.zero

derivingUnbox "Complex"
  [t| forall a . (U.Unbox a) => Complex a -> (a, a) |]
  [| \ (Complex' x) -> (C.real x, C.imag x) |]
  [| \ (r, i) -> Complex' $ r C.+: i |]






deriving instance (Elt r) => Elt (RRq q r)

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
  basicUnsafeReplicate n (RRq' x) = MV_RRq <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_RRq v) z = RRq' <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_RRq v) z (RRq' x) = M.basicUnsafeWrite v z x
  basicClear (MV_RRq v) = M.basicClear v
  basicSet (MV_RRq v) (RRq' x) = M.basicSet v x
  basicUnsafeCopy (MV_RRq v1) (MV_RRq v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_RRq v1) (MV_RRq v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_RRq v) n = MV_RRq <$> M.basicUnsafeGrow v n

instance U.Unbox r => G.Vector U.Vector (RRq q r) where
  basicUnsafeFreeze (MV_RRq v) = V_RRq <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_RRq v) = MV_RRq <$> G.basicUnsafeThaw v
  basicLength (V_RRq v) = G.basicLength v
  basicUnsafeSlice z n (V_RRq v) = V_RRq $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_RRq v) z = RRq' <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_RRq mv) (V_RRq v) = G.basicUnsafeCopy mv v
  elemseq _ = seq





deriving instance (Elt i) => Elt (ZqBasic q i)

-- CJP: restored manual Unbox instances, until we have a better way
-- (NewtypeDeriving or TH)

newtype instance U.MVector s (ZqBasic q z) = MV_ZqBasic (U.MVector s z)
newtype instance U.Vector (ZqBasic q z) = V_ZqBasic (U.Vector z)

-- Unbox, when underlying representation is
instance U.Unbox z => U.Unbox (ZqBasic q z)

{- purloined and tweaked from code in `vector` package that defines
types as unboxed -}
instance U.Unbox z => M.MVector U.MVector (ZqBasic q z) where
  basicLength (MV_ZqBasic v) = M.basicLength v
  basicUnsafeSlice z n (MV_ZqBasic v) = MV_ZqBasic $ M.basicUnsafeSlice z n v
  basicOverlaps (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicOverlaps v1 v2
  basicInitialize (MV_ZqBasic v) = M.basicInitialize v
  basicUnsafeNew n = MV_ZqBasic <$> M.basicUnsafeNew n
  basicUnsafeReplicate n (ZqB x) = MV_ZqBasic <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_ZqBasic v) z = ZqB <$> M.basicUnsafeRead v z
  basicUnsafeWrite (MV_ZqBasic v) z (ZqB x) = M.basicUnsafeWrite v z x
  basicClear (MV_ZqBasic v) = M.basicClear v
  basicSet (MV_ZqBasic v) (ZqB x) = M.basicSet v x
  basicUnsafeCopy (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_ZqBasic v1) (MV_ZqBasic v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_ZqBasic v) n = MV_ZqBasic <$> M.basicUnsafeGrow v n

instance U.Unbox z => G.Vector U.Vector (ZqBasic q z) where
  basicUnsafeFreeze (MV_ZqBasic v) = V_ZqBasic <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_ZqBasic v) = MV_ZqBasic <$> G.basicUnsafeThaw v
  basicLength (V_ZqBasic v) = G.basicLength v
  basicUnsafeSlice z n (V_ZqBasic v) = V_ZqBasic $ G.basicUnsafeSlice z n v
  basicUnsafeIndexM (V_ZqBasic v) z = ZqB <$> G.basicUnsafeIndexM v z
  basicUnsafeCopy (MV_ZqBasic mv) (V_ZqBasic v) = G.basicUnsafeCopy mv v
  elemseq _ = seq