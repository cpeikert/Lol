{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Contains internal helper functions for PT2CT for looking up/generating
-- keys and hints during compilation

module Crypto.Alchemy.Interpreter.Compiler.Environment (P2CState, newP2CState, genTunnHint, getKSHint, keyLookup, hintLookup) where

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State

import Data.Dynamic
import Data.Maybe (mapMaybe)

import Crypto.Lol
import Crypto.Lol.Applications.SymmSHE

---- Monad helper functions

data P2CState = St {keys :: [Dynamic], hints :: [Dynamic]} deriving (Show)

newP2CState :: P2CState
newP2CState = St [] []

-- retrieve the scaled variance parameter from the Reader
getSvar :: (MonadReader v mon) => mon v
getSvar = ask

-- retrieve a key from the state, or generate a new one otherwise
getKey :: forall z v mon t m' . (MonadReader v mon, MonadState P2CState mon,
           MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z))
  => mon (SK (Cyc t m' z))
getKey = keyLookup >>= \case
  (Just t) -> return t
  -- generate a key with the variance stored in the Reader monad
  Nothing -> do
    v <- getSvar
    putKey >=< genSK v

putKey :: (MonadState P2CState m, Typeable r') => SK r' -> m ()
putKey sk = modify' $ \St{..} -> St {keys=toDyn sk : keys,..}

putHint :: (MonadState P2CState m, Typeable a) => a -> m ()
putHint h = modify' $ \St{..} -> St {hints=toDyn h : hints,..}

-- sequences the first action and returns the value of the second
(>=<) :: (Monad m) => (a -> m ()) -> m a -> m a
a >=< b = do
  b' <- b
  a b'
  return b'

-- not memoized right now, but could be if we also store the linear function as part of the lookup key
-- EAC: https://ghc.haskell.org/trac/ghc/ticket/13490
genTunnHint :: forall gad zq mon t e r s e' r' s' z zp v .
  (MonadReader v mon, MonadState P2CState mon, MonadRandom mon,
   GenSKCtx t r' z v, Typeable (Cyc t r' (LiftOf zp)),
   GenSKCtx t s' z v, Typeable (Cyc t s' (LiftOf zp)),
   GenTunnelInfoCtx t e r s e' r' s' z zp zq gad,
   z ~ LiftOf zp)
  => Linear t zp e r s -> mon (TunnelInfo gad t e r s e' r' s' zp zq)
genTunnHint linf = do
  skout <- getKey @z
  sk <- getKey @z
  tunnelInfo linf skout sk

-- retrieve a key-switch hint from the state, or generate a new one otherwise
getKSHint :: forall v mon t z gad m' zq zq' ksmod .
  (-- constraints for getKey
   MonadReader v mon, MonadState P2CState mon,
   MonadRandom mon, GenSKCtx t m' z v, Typeable (Cyc t m' z),
   -- constraints for hintLookup
   Typeable (KSQuadCircHint gad (Cyc t m' zq')),
   -- constraints for ksQuadCircHint
   KSHintCtx gad t m' z zq', zq' ~ (ksmod, zq)) -- EAC: Note that order matches the optimized RescaleCyc instance
  => Proxy ksmod -> Proxy z -> Proxy zq -> mon (KSQuadCircHint gad (Cyc t m' zq'))
getKSHint _ _ _ = hintLookup >>= \case
  (Just h) -> return h
  Nothing -> do
    sk :: SK (Cyc t m' z) <- getKey
    putHint >=< ksQuadCircHint sk

-- lookup a key in the state
keyLookup :: (Typeable a, MonadState P2CState mon) => mon (Maybe a)
keyLookup = (dynLookup . keys) <$> get

-- lookup a hint in the state
hintLookup :: (Typeable a, MonadState P2CState mon) => mon (Maybe a)
hintLookup = (dynLookup . hints) <$> get

-- lookup an item in a dynamic list
dynLookup :: (Typeable a) => [Dynamic] -> Maybe a
dynLookup ds = case mapMaybe fromDynamic ds of
  [] -> Nothing
  [x] -> Just x