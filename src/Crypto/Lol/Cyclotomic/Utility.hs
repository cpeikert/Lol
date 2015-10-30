{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Lol.Cyclotomic.Utility where

import Crypto.Lol.Factored

import Control.DeepSeq

-- | Represents the powerful or decoding basis.
data Basis = Pow | Dec

instance NFData Basis where
  rnf Pow = ()
  rnf Dec = ()

-- | Represents cyclotomic rings that are rescalable over their base
-- rings.  (This is a class because it allows for more efficient
-- specialized implementations.)

class RescaleCyc c a b where
  -- | Rescale in the given basis.
  rescaleCyc :: Fact m => Basis -> c m a -> c m b
