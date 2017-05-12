{-# LANGUAGE ConstraintKinds            #-}
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
import Crypto.Lol.Applications.SymmSHE (CT, SK)

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
  z                             -- | (phantom) integral type for secret keys
  k                             -- | (reader) monad that supplies the
                                -- keys for extracting error
  w                             -- | (writer) monad for logging error rates
  e                             -- | environment
  a                             -- | represented type
  = ERW { unERW :: k (expr (Monadify w e) (Monadify w a)) }

type family Monadify w a where
  Monadify w (a,b) = (Monadify w a, Monadify w b)
  Monadify w (a -> b) = Monadify w a -> Monadify w b
  Monadify w a = w a

-- CJP: could generalize to (String, Double) to allow messages, but
-- then we need pairs in the object language!  (We already need lists
-- though...)
type ErrorRateLog = [Double]

-- | Transform an expression into (a monadic) one that logs error
-- rates, where the needed keys are obtained from the monad.
writeErrorRates :: ErrorRateWriter expr z k w e a
                -> k (expr (Monadify w e) (Monadify w a))
writeErrorRates = unERW

-- | Perform the action, then perform the action given by the result,
-- and return the (first) result.
after_ :: (Monad_ expr, Monad m) => expr e ((a -> m ()) -> m a -> m a)
after_ = lam $ lam $ bind_ $: v0 $:
         lam (bind_ $: (v2 $: v0) $:
               lam (return_ $: v1))

tellError :: (MonadWriter ErrorRateLog mon,
              List expr, MonadWriter_ expr, ErrorRate expr,
              ErrorRateCtx expr (CT m zp (Cyc t m' zq)) z) =>
  SK (Cyc t m' z) -> expr e (CT m zp (Cyc t m' zq) -> mon ())
tellError sk = lam (tell_ $: (cons_ $: (errorRate_ sk $: v0) $: nil_))

-- | Convert an object-language function to a (monadic) one that
-- writes the error rate of its ciphertext output.
liftWriteError ::
  (MonadWriter ErrorRateLog w, List expr, MonadWriter_ expr, ErrorRate expr,
   ct ~ (CT m zp (Cyc t m' zq)), ErrorRateCtx expr ct z)
  => SK (Cyc t m' z)            -- | the secret key
  -> expr e (a -> ct)           -- | the function to lift
  -> expr e (w a -> w ct)
liftWriteError sk f_ =
  let mf_ = liftA_ $: s f_
  in lam $ after_ $: tellError sk $: (mf_ $: v0)

type WriteErrorCtx expr z k w ct t m m' zp zq =
  (MonadWriter ErrorRateLog w, MonadReader Keys k, Typeable (SK (Cyc t m' z)),
   List expr, MonadWriter_ expr, ErrorRate expr,
   ct ~ (CT m zp (Cyc t m' zq)), ErrorRateCtx expr ct z)

liftWriteError2 :: forall w k t m' z expr ct m zp zq a b e .
  (WriteErrorCtx expr z k w ct t m m' zp zq)
  => SK (Cyc t m' z)            -- | the secret key
  -> expr e (a -> b -> ct)      -- | the function to lift
  -> k (expr e (w a -> w b -> w ct))

-- CJP: would prefer for this function to be monadic and look up the
-- secret key itself, but GHC fails to find the Typeable constraints
-- that I put right there in the signature.
liftWriteError2 sk f_ =
  let mf_ = liftA2_ $: s (s f_)
  in do
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    case key of
      Just sk -> return $ lam $ lam $ after_ $: tellError sk $: (mf_ $: v1 $: v0)
      Nothing -> return $ liftA2_ $: f_



{-

instance (WriteErrorCtx expr z k w ct t m m' zp zq,
          Add expr ct) => Add (ErrorRateWriter expr z k w) ct where

  add_ = ERW $ do
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    case key of
      Just sk -> return $ liftWriteError2 sk add_
      Nothing -> return $ liftA2_ $: add_

  -- don't log error because it doesn't grow
  neg_ = ERW $ pure $ liftA_ $: neg_

instance (WriteErrorCtx expr z k w ct t m m' zp zq,
          Mul expr ct,
          -- needed because PreMul could take some crazy form
          Monadify w (PreMul expr ct) ~ w (PreMul expr ct))
         => Mul (ErrorRateWriter expr z k w) ct where

  type PreMul (ErrorRateWriter expr z k w) ct = PreMul expr ct

  mul_ = ERW $ do
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    case key of
      Just sk -> return $ liftWriteError2 sk mul_
      Nothing -> return $ liftA2_ $: mul_

instance (WriteErrorCtx expr z k w ct t m m' zp zq,
          AddLit expr ct) => AddLit (ErrorRateWriter expr z k w) ct where
  addLit_ a = ERW $ do
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    case key of
      Just sk -> return $ liftWriteError sk $ addLit_ a
      Nothing -> return $ liftA_ $: addLit_ a

instance (WriteErrorCtx expr z k w ct t m m' zp zq,
          MulLit expr ct) => MulLit (ErrorRateWriter expr z k w) ct where
  mulLit_ a = ERW $ do
    key :: Maybe (SK (Cyc t m' z)) <- lookupKey
    case key of
      Just sk -> return $ liftWriteError sk $ mulLit_ a
      Nothing -> return $ liftA_ $: mulLit_ a

-}


----- TRIVIAL WRAPPER INSTANCES -----

instance (Lambda expr, Applicative k)
  => Lambda (ErrorRateWriter expr z k w) where

  lam f  = ERW $ lam <$> unERW f
  f $: a = ERW $ ($:) <$> unERW f <*> unERW a
  v0     = ERW $ pure v0
  s a    = ERW $ s <$> unERW a

instance (SHE expr, Applicative_ expr, Applicative k, Applicative w) =>
  SHE (ErrorRateWriter expr z k w) where

  type ModSwitchPTCtx   (ErrorRateWriter expr z k w) ct zp' = ModSwitchPTCtx expr ct zp'
  type RescaleLinearCtx (ErrorRateWriter expr z k w) ct zq' = RescaleLinearCtx expr ct zq'
  type AddPublicCtx     (ErrorRateWriter expr z k w) ct     = AddPublicCtx expr ct
  type MulPublicCtx     (ErrorRateWriter expr z k w) ct     = MulPublicCtx expr ct
  type KeySwitchQuadCtx (ErrorRateWriter expr z k w) ct gad = KeySwitchQuadCtx expr ct gad
  type TunnelCtx
    (ErrorRateWriter expr z k w) t e r s e' r' s' zp zq gad = TunnelCtx expr t e r s e' r' s' zp zq gad

  modSwitchPT_     = ERW $ pure $ liftA_ $: modSwitchPT_
  rescaleLinear_   = ERW $ pure $ liftA_ $: rescaleLinear_
  addPublic_     p = ERW $ pure $ liftA_ $: addPublic_ p
  mulPublic_     p = ERW $ pure $ liftA_ $: mulPublic_ p
  keySwitchQuad_ h = ERW $ pure $ liftA_ $: keySwitchQuad_ h
  tunnel_        h = ERW $ pure $ liftA_ $: tunnel_ h

instance (ErrorRate expr, Applicative_ expr, Applicative k, Applicative w) =>
  ErrorRate (ErrorRateWriter expr z k w) where

  type ErrorRateCtx (ErrorRateWriter expr z' k w) ct z = ErrorRateCtx expr ct z

  errorRate_  sk = ERW $ pure $ liftA_ $: errorRate_ sk
