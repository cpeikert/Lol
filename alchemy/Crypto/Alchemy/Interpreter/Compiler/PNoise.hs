{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Crypto.Alchemy.Interpreter.Compiler.PNoise where

import Algebra.Additive as Additive
import Algebra.Ring as Ring
import Language.Haskell.TH
import Crypto.Lol.Reflects
import GHC.TypeLits
import Data.Proxy
import Data.Functor.Trans.Tagged

-- | A value tagged by @pNoise =~ -log(noise rate)@.
newtype PNoise (h :: Nat) a = PN a deriving (Additive.C, Ring.C)

-- Maps PNoise to a modulus from a list, according to a heuristic
type family PNoise2Zq zqs h where
  -- pNoise unit ~= 8 bits; so 0--3 fit into a 32-bit modulus.
  PNoise2Zq (zq ': rest) 'Z                     = zq
  PNoise2Zq (zq ': rest) ('S 'Z)                = zq
  PNoise2Zq (zq ': rest) ('S ('S 'Z))           = zq
  PNoise2Zq (zq ': rest) ('S ('S ('S 'Z)))      = zq
  PNoise2Zq (zq ': rest) ('S ('S ('S ('S i))))  = PNoise2Zq rest i

data PNoiseMod = PNM Nat Nat

-- 8 bits seems like too much, because our moduli need to be slightly
-- less than 32 bits to fit into a 64 bit modulus,
-- but we'd still like to treat them as having 4 units of noise.
-- | One unit of "noise", in bits.
noiseUnit :: Double
noiseUnit = 7.5

mkQ :: Integer -> TypeQ
mkQ q =
  let lgq = ceiling $ logBase 2 (Ring.fromInteger q :: Double) / noiseUnit
  in conT 'PNM `appT` (litT $ numTyLit q) `appT` (litT $ numTyLit lgq)

instance (KnownNat x, Ring.C i) => Reflects ('PNM x y) i where
  value = tag $ Ring.fromInteger $ natVal (Proxy::Proxy x)