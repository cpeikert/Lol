{-# LANGUAGE DataKinds, TemplateHaskell #-}

-- | This module defines types and operations for type-level
-- representation and manipulation of natural numbers, as represented
-- by their prime-power factorizations.  It relies on TH, so the
-- documentation may be difficult to read.  See source-level comments
-- for further details.

module Crypto.Lol.Factored
( module Crypto.Lol.FactoredDefs
, module Crypto.Lol.Factored
) where

import Crypto.Lol.FactoredDefs

$(mapM fType [1..128])
$(mapM fType [256,512,1024,2048])
