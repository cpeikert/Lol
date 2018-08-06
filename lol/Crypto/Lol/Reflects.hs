{-|
Module      : Crypto.Lol.Reflects
Description : Generic interface for reflecting types to values.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

Generic interface for reflecting types to values.
-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Lol.Reflects
( Reflects(..)
) where

import Algebra.Ring      as Ring
import Algebra.ToInteger as ToInteger
import NumericPrelude

import Crypto.Lol.Factored

import Data.Proxy
import Data.Reflection
import Data.Singletons
import GHC.TypeLits    as TL

-- | Reflection without fundep, and with tagged value. Intended only
-- for low-level code; build specialized wrappers around it for
-- specific functionality.

class Reflects a i where
  -- | Reflect the value assiated with the type @a@.
  value :: i

instance (KnownNat a, Ring.C i) => Reflects (a :: TL.Nat) i where
  value = fromIntegral $ natVal (Proxy::Proxy a)

instance (PosC a, ToInteger.C i) => Reflects a i where
  value = posToInt $ fromSing (sing :: Sing a)

instance (BinC a, ToInteger.C i) => Reflects a i where
  value = binToInt $ fromSing (sing :: Sing a)

instance (Prime p, ToInteger.C i) => Reflects p i where
  value = fromIntegral $ valuePrime @p

instance (PPow pp, ToInteger.C i) => Reflects pp i where
  value = fromIntegral $ valuePPow @pp

instance (Fact m, ToInteger.C i) => Reflects m i where
  value = fromIntegral $ valueFact @m

instance (Reifies q i, ToInteger.C i, Ring.C r)
  => Reflects (q :: *) r where
  value = fromIntegral $ reflect (Proxy::Proxy q)
