{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances,
             KindSignatures, MultiParamTypeClasses, NoImplicitPrelude,
             PolyKinds, ScopedTypeVariables, UndecidableInstances #-}

-- | Generic interface for reflecting types to values.

module Crypto.Lol.Reflects
( Reflects(..)
) where

import Algebra.ToInteger as ToInteger
import NumericPrelude

import Crypto.Lol.Factored

import Control.Applicative
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

instance (KnownNat a, ToInteger.C i) => Reflects (a :: TL.Nat) i where
  value = tag $ fromIntegral $ natVal (Proxy::Proxy a)

{-

instance (PosC a, ToInteger.C i) => Reflects a i where
  value = tag $ posToInt $ fromSing (sing :: Sing a)

instance (BinC a, ToInteger.C i) => Reflects a i where
  value = tag $ binToInt $ fromSing (sing :: Sing a)

-}

-- CJP: need reflections for Prime and PrimePower types because we use
-- them with ZqBasic

instance (Prim p, ToInteger.C i) => Reflects p i where
  value = fromIntegral <$> valuePrime

instance (PPow pp, ToInteger.C i) => Reflects pp i where
  value = fromIntegral <$> valuePPow

-- CJP: need this for Types.ZmStar, where we use ZqBasic m Int
instance (Fact m, ToInteger.C i) => Reflects m i where
  value = fromIntegral <$> valueFact

instance {-# OVERLAPS #-} (Reifies rei a) => Reflects (rei :: *) a where
  value = tag $ reflect (Proxy::Proxy rei)
