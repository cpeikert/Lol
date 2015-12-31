{-# LANGUAGE DataKinds, TemplateHaskell #-}

module Crypto.Lol.PosBin
( module Crypto.Lol.PosBinDefs
, module Crypto.Lol.PosBin
) where

import Crypto.Lol.PosBinDefs
import Crypto.Lol.PosBinTH

$(mapM (conType "P" pos) [1..16])

$(mapM (conType "B" bin) [1..128])

$(mapM (conType "Prime" bin) $ take 120 primes)
