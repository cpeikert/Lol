{-# LANGUAGE DataKinds, TemplateHaskell #-}

-- | Positive naturals in Peano and binary representations,
-- singletonized and promoted to type level.

module Crypto.Lol.PosBin
( module Crypto.Lol.PosBinDefs
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBinDefs

$(mapM posDec [1..16])

$(mapM binDec [1..128])

$(mapM (intDec "Prime" binType) $ take 120 primes)
