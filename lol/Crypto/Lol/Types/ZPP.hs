{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

-- | A class for integers mod a prime power.

module Crypto.Lol.Types.ZPP
( ZPP(..)
) where

import Crypto.Lol.Prelude
import Crypto.Lol.Types.FiniteField

-- | Represents integers modulo a prime power.
class (PrimeField (ZpOf zq), Ring zq) => ZPP zq where

  -- | An implementation of the integers modulo the prime base.
  type ZpOf zq

  -- | The prime and exponent of the modulus.
  modulusZPP :: Tagged zq PP

  -- | Lift from \(\mathbb{Z}_p\) to a representative.
  liftZp :: ZpOf zq -> zq

