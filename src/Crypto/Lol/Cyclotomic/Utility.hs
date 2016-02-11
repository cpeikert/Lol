{-# LANGUAGE MultiParamTypeClasses #-}

module Crypto.Lol.Cyclotomic.Utility where

-- | Represents cyclotomic rings that are rescalable over their base
-- rings.  (This is a class because it allows for more efficient
-- specialized implementations.)

class RescaleCyc ca cb where
  -- | Rescale in the powerful basis.
  rescalePow :: ca -> cb
  -- | Rescale in the decoding basis.
  rescaleDec :: ca -> cb
