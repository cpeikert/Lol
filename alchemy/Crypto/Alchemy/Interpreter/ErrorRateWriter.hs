{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Crypto.Alchemy.Interpreter.ErrorRateWriter
( ErrorRateWriter, writeErrorRates, Monadify, ErrorRateLog )
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Typeable

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

import Crypto.Alchemy.Interpreter.KeysHints
import Crypto.Alchemy.Language.Arithmetic
import Crypto.Alchemy.Language.Lambda
import Crypto.Alchemy.Language.List
import Crypto.Alchemy.Language.Monad
import Crypto.Alchemy.Language.SHE

-- | A transformer that additionally logs the sizes of the noise terms
-- of any ciphertexts created during interpretation.
newtype ErrorRateWriter
  expr                          -- | the underyling interpreter
  k                             -- | (reader) monad that supplies the
                                -- keys for extracting error
  w                             -- | (writer) monad for logging error rates
  e                             -- | environment
  a                             -- | represented type
  = ERW { unERW :: k (expr (Monadify w e) (Monadify w a)) }

type family Monadify m a where
  Monadify m (a,b) = (Monadify m a, Monadify m b)
  Monadify m (a -> b) = Monadify m a -> Monadify m b
  Monadify m a = m a

-- CJP: could generalize to (String, Double) to allow messages, but
-- then we need pairs in the object language!  (We already need lists
-- though...)
type ErrorRateLog = [Double]

-- | Transform an expression into (a monadic) one that logs error
-- rates, where the needed keys are obtained from the monad.
writeErrorRates :: ErrorRateWriter expr k w e a
                -> k (expr (Monadify w e) (Monadify w a))
writeErrorRates = unERW

instance (Lambda expr, Applicative k)
  => Lambda (ErrorRateWriter expr k w) where

  lam (ERW f) = ERW $ lam <$> f
  (ERW f) $: (ERW a) = ERW $ ($:) <$> f <*> a

  v0     = ERW $ pure v0
  s (ERW a) = ERW $ s <$> a


{- attempt to abstract the pattern of "convert a pure obj-lang function
to one which writes the error rate of its final output"

liftWriteError2 ::
  (MonadWriter ErrorRateLog w, MonadReader Keys k, Typeable cyc,
   List_ expr, MonadWriter_ expr,
   ErrorRate expr, ErrorRateCtx expr c z)
  => Proxy cyc -> expr e (a -> b -> c) -> k (expr e (w a -> w b -> w c))

liftWriteError2 f_ = do
  let mf_ = liftA2_ $: f_
  key :: Maybe (SK cyc) <- lookupKey
  case key of
    Just sk -> return $ lam $ lam $ after_
               $: lam (tell_ $: (cons_ $: (errorRate sk $: v0) $: nil_))
               $: (mf_ $: v1 $: v0)
    Nothing -> return mf_
-}

instance (MonadWriter ErrorRateLog w, MonadReader Keys k,
          ct ~ (CT m zp (Cyc t m' zq)), z ~ (LiftOf zp), Typeable (Cyc t m' z),
          List_ expr, MonadWriter_ expr,
          Add expr ct, ErrorRate expr, ErrorRateCtx expr ct z)
         => Add (ErrorRateWriter expr k w) ct where

  add_ = ERW $ do               -- in k monad
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    let madd_ = liftA2_ $: add_
    case key of
      Just sk -> return $ lam $ lam $ after_
                 $: lam (tell_ $: (cons_ $: (errorRate sk $: v0) $: nil_))
                 $: (madd_ $: v1 $: v0)
      Nothing -> return madd_

  -- don't log error because it doesn't grow
  neg_ = ERW $ pure $ liftA_ $: neg_

instance (MonadWriter ErrorRateLog w, MonadReader Keys k,
          ct ~ (CT m zp (Cyc t m zq)),
          -- needed because PreMul could take some crazy form
          Monadify w (PreMul expr ct) ~ w (PreMul expr ct),
          ErrorRate expr, Applicative_ expr, Mul expr ct)
         => Mul (ErrorRateWriter expr k w) ct where

  type PreMul (ErrorRateWriter expr k w) ct = PreMul expr ct

  mul_ = ERW $ pure $ liftA2_ $: mul_
