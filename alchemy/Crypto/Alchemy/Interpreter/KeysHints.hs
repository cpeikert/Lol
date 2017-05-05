{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Functions for looking up/generating keys and key-switch hints.
module Crypto.Alchemy.Interpreter.KeysHints
( Keys, Hints, lookupKey, lookupHint
, getKey, getQuadCircHint, getTunnelHint
, runKeysHints, evalKeysHints
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

-- | Convenience function.
runKeysHints :: (Functor m)
  => v -> StateT Keys (StateT Hints (ReaderT v m)) a -> m (a, Keys, Hints)
runKeysHints v = ((\((a,b),c) -> (a,b,c)) <$>) . 
  flip runReaderT v . runAccumulatorT . runAccumulatorT

-- | Output the output of the computation, discarding the accumulated result.
evalKeysHints :: (Functor m) => v -> StateT Keys (StateT Hints (ReaderT v m)) a -> m a
evalKeysHints v = ((\(a,_,_) -> a) <$>) . runKeysHints v

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
   GenSKCtx t r' z v, Typeable (Cyc t r' z),
   GenSKCtx t s' z v, Typeable (Cyc t s' z),
   TunnelHintCtx t e r s e' r' s' z zp zq gad)
  => Proxy z -> Linear t zp e r s
  -> mon (TunnelHint gad t e r s e' r' s' zp zq)
getTunnelHint _ linf = do
  skout <- getKey @_ @_ @_ @_ @z
  skin <- getKey @_ @_ @_ @_ @z
  tunnelHint linf skout skin
