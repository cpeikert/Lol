{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, PolyKinds,
             ScopedTypeVariables, UndecidableInstances #-}

-- | Generic interface for reflecting types to values.

module Crypto.Lol.Reflects
( Reflects(..)
) where

import Crypto.Lol.Factored

import Data.Functor.Trans.Tagged
import Data.Proxy
import Data.Reflection
import GHC.TypeLits              as TL

-- | Reflection without fundep, and with tagged value. Intended only
-- for low-level code; build specialized wrappers around it for
-- specific functionality.

class Reflects a i where
  -- | Reflect the value assiated with the type @a@.
  value :: Tagged a i

instance (KnownNat a, Integral i) => Reflects (a :: TL.Nat) i where
  value = return $ fromIntegral $ natVal (Proxy::Proxy a)

instance (BinC a, Integral i) => Reflects a i where
  value = fromIntegral <$> valueBinC

instance (PPow pp, Integral i) => Reflects pp i where
  value = fromIntegral <$> valuePPow

instance (Fact m, Integral i) => Reflects m i where
  value = fromIntegral <$> valueFact

instance {-# OVERLAPS #-} (Reifies rei a) => Reflects (rei :: *) a where
  value = tag $ reflect (Proxy::Proxy rei)
