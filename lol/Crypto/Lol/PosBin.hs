{-# LANGUAGE DataKinds, TemplateHaskell #-}

-- | Positive naturals in Peano and binary representations,
-- singletonized and promoted to the type level.  This module relies
-- on Template Haskell, so parts of the documentation may be difficult
-- to read.  See source-level comments for further details.

module Crypto.Lol.PosBin
( module Crypto.Lol.PosBinDefs
-- * Convenient synonyms for 'Pos' and 'Bin' types
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBinDefs

$(mapM posDec [1..16])

$(mapM binDec [1..128])
