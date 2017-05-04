{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Internal helper functions for PT2CT for looking up/generating
-- keys and hints during compilation

module Crypto.Alchemy.Interpreter.PT2CT.Environment
( KeysHintsAccum, runKeysHintsAccum,
  Keys, Hints, lookupKey, lookupHint,
  getKey, getQuadCircHint, getTunnelHint
)
where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Dynamic
import Data.Maybe   (mapMaybe)

import Crypto.Alchemy.MonadAccumulator
import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

---- Monad helper functions

-- | Wrapper for a dynamic list of keys.
newtype Keys = Keys { unKeys :: [Dynamic] } deriving (Monoid, Show)

-- | Wrapper for a dynamic list of hints.
newtype Hints = Hints { unHints :: [Dynamic] } deriving (Monoid, Show)

-- | An monad that accumulates (dynamic) keys and hints.
newtype KeysHintsAccum a = Accum (State (Keys, Hints) a) deriving (Functor, Applicative, Monad)

-- | Unwrap  a KeysHintsAccum computation as a (result, keys, hints) triple
runKeysHintsAccum :: KeysHintsAccum a -> (Keys, Hints, a)
runKeysHintsAccum (Accum a) = map (\(b,(c,d)) -> (b,c,d)) $ runState a (mempty, mempty)

-- | Unwrap  a KeysHintsAccum computation, discarding the accumulated result.
evalKeysHintsAccum :: KeysHintsAccum a -> a
evalKeysHintsAccum (Accum a) = evalState a (mempty, mempty)

instance MonadAccumulator Keys KeysHintsAccum where
  append key = Accum $ do -- in State monad
    (keys, hints) <- get
    put (keys `mappend` key, hints)

  accumulate f = Accum $ state $ \(keys, hints) ->
    let (a,keys') = f keys
    in (a, (keys `mappend` keys', hints))

instance MonadAccumulator Hints KeysHintsAccum where
  append hint = Accum $ do -- in State monad
    (keys, hints) <- get
    put (keys, hints `mappend` hint)

  accumulate f = Accum $ state $ \(keys, hints) ->
    let (a,hints') = f hints
    in (a, (keys, hints `mappend` hints'))




-- | Look up a value of the desired type, if it exists.
lookupKey :: (MonadReader Keys mon, Typeable a) => mon (Maybe a)
lookupKey = (dynLookup . unKeys) <$> ask
  where dynLookup :: (Typeable a) => [Dynamic] -> Maybe a
        dynLookup ds = case mapMaybe fromDynamic ds of
          []    -> Nothing
          (x:_) -> Just x

-- | Look up a value of the desired type, if it exists.
lookupHint :: (MonadReader Hints mon, Typeable a) => mon (Maybe a)
lookupHint = (dynLookup . unHints) <$> ask
  where dynLookup :: (Typeable a) => [Dynamic] -> Maybe a
        dynLookup ds = case mapMaybe fromDynamic ds of
          []    -> Nothing
          (x:_) -> Just x

-- | Append a value to the internal state.
appendKey :: (MonadAccumulator Keys m, Typeable (Cyc t m' z)) => SK (Cyc t m' z) -> m ()
appendKey a = append $ Keys [toDyn a]

appendHint :: (MonadAccumulator Hints m, Typeable a) => a -> m ()
appendHint a = append $ Hints [toDyn a]

-- | Sequence a, then pass the result to f and return a
(>=<) :: (Monad m) => (a -> m ()) -> m a -> m a
f >=< a = do
  a' <- a
  f a'
  return a'

-- | Lookup a key, generating one if it doesn't exist, and return it.
getKey :: (MonadReader v mon, MonadAccumulator Keys mon,
           MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z))
  => mon (SK (Cyc t m' z))
getKey = embedReader lookupKey >>= \case
  (Just t) -> return t
  -- generate a key with the variance stored in the Reader monad
  Nothing -> do
    v <- ask
    appendKey >=< genSK v

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
getQuadCircHint _ = embedReader lookupHint >>= \case
  (Just h) -> return h
  Nothing -> do
    sk :: SK (Cyc t m' z) <- getKey
    appendHint >=< ksQuadCircHint sk

-- not memoized right now, but could be if we also store the linear function as part of the lookup key
-- EAC: https://ghc.haskell.org/trac/ghc/ticket/13490
-- | Generate auxilliary data needed for tunneling. Note that the results are /not/ stored or reused later.
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
