{-# LANGUAGE DataKinds, TemplateHaskell #-}

module Crypto.Lol.Factored
( module Crypto.Lol.FactoredDefs
, module Crypto.Lol.Factored
) where

import Crypto.Lol.FactoredDefs
import Crypto.Lol.FactoredTH

$(mapM fType [1..128])
$(mapM fType [256,512,1024,2048])
