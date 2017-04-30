{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Crypto.Alchemy.Interpreter.Compiler.PNoise where

import Algebra.Additive as Additive (C)
import Algebra.Ring as Ring (C)
import Language.Haskell.TH
import Data.Functor.Trans.Tagged
import Data.Type.Natural
import qualified GHC.TypeLits as TL

import Crypto.Lol.Reflects

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

data PNoiseMod = PNM TL.Nat Nat

-- 8 bits seems like too much, because our moduli need to be slightly
-- less than 32 bits to fit into a 64 bit modulus,
-- but we'd still like to treat them as having 4 units of noise.
-- | One unit of "noise", in bits.
noiseUnit :: Double
noiseUnit = 7.5

mkTypeNat :: Int -> TypeQ
mkTypeNat 0 = conT 'Z
mkTypeNat x = conT 'S `appT` (mkTypeNat $ x-1)

mkTypeLit :: Integer -> TypeQ
mkTypeLit = litT . numTyLit

mkQ :: Integer -> TypeQ
mkQ q =
  let lgq = ceiling $ logBase 2 (fromInteger q :: Double) / noiseUnit
  in conT 'PNM `appT` (mkTypeLit q) `appT` (mkTypeNat lgq)

instance (Reflects x i) => Reflects ('PNM x y) i where
  value = retag $ value @x