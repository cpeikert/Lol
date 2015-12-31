{-# LANGUAGE DataKinds, TemplateHaskell #-}

-- | Positive naturals in Peano and binary representations,
-- singletonized and promoted to type level.

module Crypto.Lol.PosBin
( module Crypto.Lol.PosBinDefs
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBinDefs

$(mapM (conType "P" pos) [1..16])

$(mapM (conType "B" bin) [1..128])

$(mapM (conType "Prime" bin) $ take 120 primes)
