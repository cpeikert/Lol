{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Crypto.Lol.Cyclotomic.Tensor.CPP.Instances where

-- EAC: Do not import Crypto.Lol.Types, because it exports an IrreduciblePoly
-- instance which screw with GHC. Probably #10338.
import Crypto.Lol.Types.Complex
import Crypto.Lol.Types.RRq
import Crypto.Lol.Types.ZqBasic

import Foreign.Storable

deriving instance (Storable a) => Storable (Complex a)
deriving instance (Storable r) => Storable (RRq q r)
deriving instance (Storable i) => Storable (ZqBasic q i)