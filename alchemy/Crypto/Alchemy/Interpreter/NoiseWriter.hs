{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.NoiseWriter
( NoiseWriter, writeNoise )
where

import Control.Applicative
import Control.Monad.Writer.Class

import Algebra.Additive as Additive (C)
import NumericPrelude

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda

-- CJP: also need some structure to give us secret keys
newtype NoiseWriter e a = NW { unNW :: e -> a }
  deriving (Functor, Applicative)

-- CJP: should String be replaced by something more structured?  E.g.,
-- Integer to carry the largest noise coeff?  Or Double to carry the
-- noise rate?
type NoiseLog = [String]

-- | Evaluate a closed expression.  (Typically the object-language
-- type @a@ should involve ciphertexts wrapped in a 'MonadWriter' for
-- 'NoiseLog', as needed by the instances of 'Add', 'Mul', etc. for
-- 'NoiseWriter')
writeNoise :: NoiseWriter () a -> a
writeNoise = flip unNW ()

instance Lambda NoiseWriter where
  lam f  = NW $ curry $ unNW f
  ($:) = (<*>)

instance DB NoiseWriter a where
  v0  = NW snd
  s a = NW $ unNW a . fst

instance (MonadWriter NoiseLog mon, ct ~ (CT m zp (Cyc t m zq)),
          Additive.C ct {- CJP: more for extracting error -}) =>
  Add NoiseWriter (mon ct) where

  x +: y = NW $ \e -> do
    x' <- unNW x e
    y' <- unNW y e
    let z = x' + y'
    writer (z, [{- CJP: TODO; indicate result of (+:) ? -}])

  negate' = fmap (fmap negate)  -- don't log noise b/c it doesn't grow
