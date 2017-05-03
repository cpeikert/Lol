{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Internal helper functions for PT2CT for looking up/generating
-- keys and hints during compilation

module Crypto.Alchemy.Interpreter.PT2CT.Environment
( P2CState
, newP2CState
, tunnelHint
, getKSHint
, lookupKey
, lookupHint
, (>=<))
where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Dynamic
import Data.Maybe (mapMaybe)

import Crypto.Alchemy.MonadAccumulator
import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

---- Monad helper functions

-- | Holds keys and hints generated during the compilation process.
data P2CState = St { keys :: [Dynamic], hints :: [Dynamic] } deriving (Show)

newP2CState :: P2CState
newP2CState = St [] []

-- retrieve a key from the state, or generate one if it doesn't exist
getKey :: forall z v mon t m' . (MonadReader v mon, MonadAccumulator P2CState mon,
           MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z))
  => mon (SK (Cyc t m' z))
getKey = lookupKey >>= \case
  (Just t) -> return t
  -- generate a key with the variance stored in the Reader monad
  Nothing -> do
    v <- ask
    append =<< genSK (v :: v)

-- not memoized right now, but could be if we also store the linear function as part of the lookup key
-- EAC: https://ghc.haskell.org/trac/ghc/ticket/13490
tunnelHint :: forall gad zq mon t e r s e' r' s' z zp v .
  (MonadReader v mon, MonadAccumulator P2CState mon, MonadRandom mon,
   GenSKCtx t r' z v, Typeable (Cyc t r' (LiftOf zp)),
   GenSKCtx t s' z v, Typeable (Cyc t s' (LiftOf zp)),
   GenTunnelInfoCtx t e r s e' r' s' z zp zq gad,
   z ~ LiftOf zp)
  => Linear t zp e r s -> mon (TunnelInfo gad t e r s e' r' s' zp zq)
tunnelHint linf = do
  skout <- getKey @z
  sk <- getKey @z
  tunnelInfo linf skout sk

-- retrieve a key-switch hint from the state, or generate a new one otherwise
getKSHint :: forall v mon t z gad m' zq zq' ksmod .
  (-- constraints for getKey
   MonadReader v mon, MonadAccumulator P2CState mon,
   MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z),
   -- constraints for lookupHint
   Typeable (KSQuadCircHint gad (Cyc t m' zq')),
   -- constraints for ksQuadCircHint
   KSHintCtx gad t m' z zq', zq' ~ (ksmod, zq)) -- EAC: Note that order matches the optimized RescaleCyc instance
  => Proxy ksmod -> Proxy z -> Proxy zq -> mon (KSQuadCircHint gad (Cyc t m' zq'))
getKSHint _ _ _ = lookupHint >>= \case
  (Just h) -> return h
  Nothing -> do
    sk :: SK (Cyc t m' z) <- getKey
    append =<< ksQuadCircHint sk

lookupKey :: (Typeable a, MonadReader P2CState mon) => mon (Maybe a)
lookupKey = (dynLookup . keys) <$> ask

lookupHint :: (Typeable a, MonadReader P2CState mon) => mon (Maybe a)
lookupHint = (dynLookup . hints) <$> ask

-- lookup an item in a list of dynamics
dynLookup :: (Typeable a) => [Dynamic] -> Maybe a
dynLookup ds = case mapMaybe fromDynamic ds of
  []    -> Nothing
  (x:_) -> Just x
