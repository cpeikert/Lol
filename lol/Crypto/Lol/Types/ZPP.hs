{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A class for integers mod a prime power.
--
module Crypto.Lol.Types.ZPP (

  ZPP(..)

) where

import Crypto.Lol.Cyclotomic.Tensor.Representation
import Crypto.Lol.FactoredDefs
import Crypto.Lol.LatticePrelude
import Crypto.Lol.Types.FiniteField


-- | Represents integers modulo a prime power.
--
class (PrimeField (ZpOf zq), Ring (TRep t zq)) => ZPP (t :: Factored -> * -> *) zq where

  -- | An implementation of the integers modulo the prime base.
  type ZpOf zq

  -- | The prime and exponent of the modulus.
  modulusZPP :: Tagged (t m zq) PP

  -- | Lift from @Z_p@ to a representative.
  liftZp :: Tagged (t m zq) (TRep t (ZpOf zq) -> TRep t zq)

