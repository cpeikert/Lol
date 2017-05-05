{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Functions for looking up/generating keys and key-switch hints.
module Crypto.Alchemy.Interpreter.KeysHints
( --KeysHintsAccumT, runKeysHintsAccumT, evalKeysHintsAccumT
  Keys, Hints, lookupKey, lookupHint,
  getKey, getQuadCircHint, getTunnelHint,
  runEnvironT, evalEnvironT
)
where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Dynamic
import Data.Maybe   (mapMaybe)

import Crypto.Alchemy.MonadAccumulator
import Crypto.Lol hiding (lift)
import Crypto.Lol.Applications.SymmSHE

---- Monad helper functions

-- | Wrapper for a dynamic list of keys.
newtype Keys = Keys { unKeys :: [Dynamic] } deriving (Monoid, Show)

-- | Wrapper for a dynamic list of hints.
newtype Hints = Hints { unHints :: [Dynamic] } deriving (Monoid, Show)

{-
-- EAC: the following code is an overlap-free alternative to using nested StateT directly.
-- | An monad that accumulates (dynamic) keys and hints.
newtype KeysHintsAccumT m a = AccumT (StateT Keys (StateT Hints m) a) deriving (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadAccumulator Keys)

instance MonadTrans KeysHintsAccumT where
  lift = AccumT . lift . lift

instance (Monad m) => MonadAccumulator Hints (KeysHintsAccumT m) where
  append = AccumT . lift . append
  accumulate = AccumT . lift . accumulate

-- | Unwrap  a KeysHintsAccum computation as a (result, keys, hints) triple
runKeysHintsAccumT :: (Functor m) => KeysHintsAccumT m a -> m (a, Keys, Hints)
runKeysHintsAccumT (AccumT a) = (\((b,c),d) -> (b,c,d)) <$> (runAccumulatorT $ runAccumulatorT a)

-- | Unwrap  a KeysHintsAccum computation, discarding the accumulated result.
evalKeysHintsAccumT :: (Monad m) => KeysHintsAccumT m a -> m a
evalKeysHintsAccumT (AccumT a) = evalAccumulatorT $ evalAccumulatorT a

runEnvironT :: (Functor m) => v -> ReaderT v (KeysHintsAccumT m) a -> m (a, Keys, Hints)
runEnvironT v = runKeysHintsAccumT . flip runReaderT v

-- | Output the output of the computation, discarding the accumulated result.
evalEnvironT :: (Monad m) => v -> ReaderT v (KeysHintsAccumT m) a -> m a
evalEnvironT v = evalKeysHintsAccumT . flip runReaderT v
-}

-- EAC: could define these in PT2CT, since the point is that they provide exactly what those instances need

-- | Type-restricted version of runAccumulatorT for the
runEnvironT :: (Functor m) => v -> StateT Keys (StateT Hints (ReaderT v m)) a -> m (a, Keys, Hints)
runEnvironT v = ((\((a,b),c) -> (a,b,c)) <$>) . flip runReaderT v . runAccumulatorT . runAccumulatorT

-- | Output the output of the computation, discarding the accumulated result.
evalEnvironT :: (Functor m) => v -> StateT Keys (StateT Hints (ReaderT v m)) a -> m a
evalEnvironT v = ((\(a,_,_) -> a) <$>) . runEnvironT v

lookupDyn :: (Typeable a) => [Dynamic] -> Maybe a
lookupDyn ds = case mapMaybe fromDynamic ds of
                 []    -> Nothing
                 (x:_) -> Just x

-- | Look up a key of the desired type, if it exists.
lookupKey :: (MonadReader Keys m, Typeable a) => m (Maybe a)
lookupKey = (lookupDyn . unKeys) <$> ask

-- | Look up a hint of the desired type, if it exists.
lookupHint :: (MonadReader Hints m, Typeable a) => m (Maybe a)
lookupHint = (lookupDyn . unHints) <$> ask

-- | Append a key to the internal state.
appendKey :: (MonadAccumulator Keys m, Typeable (Cyc t m' z))
  => SK (Cyc t m' z) -> m ()
appendKey a = append $ Keys [toDyn a]

-- | Append a hint to the internal state.
appendHint :: (MonadAccumulator Hints m, Typeable a) => a -> m ()
appendHint a = append $ Hints [toDyn a]

-- | Perform the action, then perform the action given by the result,
-- and return the (first) result.
(>=<) :: (Monad m) => (a -> m ()) -> m a -> m a
f >=< a = do
  a' <- a
  f a'
  return a'

-- | Lookup a key, generating one if it doesn't exist, and return it.
getKey :: (MonadReader v mon, MonadAccumulator Keys mon,
           MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z))
  => mon (SK (Cyc t m' z))
getKey = readerToAccumulator lookupKey >>= \case
  (Just t) -> return t
  -- generate and save a key (using the variance from the monad)
  Nothing -> appendKey >=< (ask >>= genSK)

-- | Lookup a (quadratic, circular) key-switch hint, generating one
-- (and the underlying key if necessary) if it doesn't exist, and
-- return it.
getQuadCircHint :: forall v mon t z gad m' zq zq' kszq .
  (-- constraints for getKey
   MonadReader v mon, MonadAccumulator Keys mon, MonadAccumulator Hints mon,
   MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z),
   -- constraints for lookup
   Typeable (KSQuadCircHint gad (Cyc t m' zq')),
   -- constraints for ksQuadCircHint
   KSHintCtx gad t m' z zq', zq' ~ (kszq, zq))
  => Proxy z -> mon (KSQuadCircHint gad (Cyc t m' zq'))
getQuadCircHint _ = readerToAccumulator lookupHint >>= \case
  (Just h) -> return h
  Nothing -> do
    sk :: SK (Cyc t m' z) <- getKey
    appendHint >=< ksQuadCircHint sk

-- not memoized right now, but could be if we also store the linear
-- function as part of the lookup key

-- EAC: https://ghc.haskell.org/trac/ghc/ticket/13490
-- | Generate a hint for tunneling. The result is /not/ memoized.
getTunnelHint :: forall gad zq mon t e r s e' r' s' z zp v .
  (MonadReader v mon, MonadAccumulator Keys mon, MonadRandom mon,
   GenSKCtx t r' z v, Typeable (Cyc t r' (LiftOf zp)),
   GenSKCtx t s' z v, Typeable (Cyc t s' (LiftOf zp)),
   TunnelHintCtx t e r s e' r' s' z zp zq gad,
   z ~ LiftOf zp)
  => Linear t zp e r s -> mon (TunnelHint gad t e r s e' r' s' zp zq)
getTunnelHint linf = do
  skout <- getKey @_ @_ @_ @_ @z
  skin <- getKey @_ @_ @_ @_ @z
  tunnelHint linf skout skin
