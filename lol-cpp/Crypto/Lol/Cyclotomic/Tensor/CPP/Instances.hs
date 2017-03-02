{-|
Module      : Crypto.Lol.Cyclotomic.Tensor.CPP.Instances
Description : CPP Tensor-specific instances.
Copyright   : (c) Eric Crockett, 2011-2017
                  Chris Peikert, 2011-2017
License     : GPL-3
Maintainer  : ecrockett0@email.com
Stability   : experimental
Portability : POSIX

CPP Tensor-specific instances.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Crypto.Lol.Cyclotomic.Tensor.CPP.Instances where

-- EAC: Do not import Crypto.Lol.Types, because it exports an IrreduciblePoly
-- instance which screw with GHC. Probably #10338.
import Crypto.Lol.Types.Unsafe.Complex
import Crypto.Lol.Types.Unsafe.RRq
import Crypto.Lol.Types.Unsafe.ZqBasic

import Foreign.Storable

deriving instance (Storable a) => Storable (Complex a)
deriving instance (Storable r) => Storable (RRq q r)
deriving instance (Storable i) => Storable (ZqBasic q i)