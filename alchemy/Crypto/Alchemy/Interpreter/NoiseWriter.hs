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
import Crypto.Alchemy.Language.Monad

-- CJP: also need some structure to give us secret keys
-- | A transformer that additionally logs the sizes of the noise terms
-- of any ciphertexts created during interpretation.
newtype NoiseWriter expr m e a =
  NW { unNW :: expr (Monadify m e) (Monadify m a) }

type family Monadify m a where
  Monadify m (a,b) = (Monadify m a, Monadify m b)
  Monadify m (a -> b) = Monadify m a -> Monadify m b
  Monadify m a = m a

-- CJP: should String be replaced by something more structured?  E.g.,
-- Integer to carry the largest noise coeff?  Or Double to carry the
-- noise rate?
type NoiseLog = [String]

writeNoise :: NoiseWriter expr m e a -> expr (Monadify m e) (Monadify m a)
writeNoise = unNW

{-

instance (Lambda expr) => Lambda (NoiseWriter expr m) where

instance (MonadWriter NoiseLog mon, ct ~ (CT m zp (Cyc t m zq)),
          Additive.C ct {- CJP: more for extracting error -}) =>
  Add (NoiseWriter expr mon) (mon ct) where

-}
