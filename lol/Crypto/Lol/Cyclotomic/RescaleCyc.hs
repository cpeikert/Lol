{-# LANGUAGE MultiParamTypeClasses #-}

-- | A class and helper functions for rescaling cycltomic ring elements.

module Crypto.Lol.Cyclotomic.RescaleCyc where

import Crypto.Lol.Factored

-- | Represents the basis used to rescale a cyclotomic ring element.
data Basis = Pow | Dec

-- | Represents cyclotomic rings that are rescalable over their base
-- rings.  (This is a class because it allows for more efficient
-- specialized implementations.)
class RescaleCyc c a b where
  -- | Rescale in the given basis.
  rescaleCyc :: Fact m => Basis -> c m a -> c m b

-- | Specialized convenience functions.
rescalePow, rescaleDec :: (RescaleCyc c a b, Fact m) => c m a -> c m b
{-# INLINE rescalePow #-}
{-# INLINE rescaleDec #-}
rescalePow = rescaleCyc Pow
rescaleDec = rescaleCyc Dec

