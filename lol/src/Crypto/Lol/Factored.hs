{-# LANGUAGE DataKinds, TemplateHaskell, TupleSections #-}

-- | This module defines types and operations for type-level
-- representation and manipulation of natural numbers, as represented
-- by their prime-power factorizations.  It relies on Template
-- Haskell, so parts of the documentation may be difficult to read.
-- See source-level comments for further details.

module Crypto.Lol.Factored
( module Crypto.Lol.FactoredDefs
-- * Convenient synonyms for 'Factored', 'PrimePower', and 'Prime' types
, module Crypto.Lol.Factored
) where

import Crypto.Lol.FactoredDefs

$(mapM fDec [1..512])
$(mapM fDec [1024,2048])

$(mapM ppDec $ (2,) <$> [1,2,3,4,5,6,7])
$(mapM ppDec $ (3,) <$> [1,2,3,4])
$(mapM ppDec $ (,1) <$> [5,7,11])

$(mapM pDec $ take 120 primes)

-- CJP: this fails to compile, as it should, because 4 is not prime
-- (sequence [ppDec (4,2)])
