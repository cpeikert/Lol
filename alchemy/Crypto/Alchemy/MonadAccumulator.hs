{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.MonadAccumulator where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)

-- | An append-only state monad.

class (Monoid w, Monad m) => MonadAccumulator w m where
  -- | Append the given value to the state inside the monad.
  append :: w -> m ()

  -- | Embed a simple append action into the monad.  The given
  -- function should output a value and the state /to be appended/ to
  -- the previous state.
  accumulate  :: (w -> (a,w)) -> m a

instance (MonadAccumulator w m) => MonadAccumulator w (StateT w' m) where
  append = lift . append
  accumulate = lift . accumulate

-- EAC: (Monad m) *should* be implied by (MonadState w m), but GHC can't figure that out...
-- EAC: See Environment.hs for a way to avoid overlapping instances (just remove the pragma and the instance above this comment)
instance {-# OVERLAPPING #-} (Monoid w, Monad m) => MonadAccumulator w (StateT w m) where
  append w = modify (`mappend` w)
  -- EAC: check this
  accumulate f = StateT $ \w -> let (a,z) = f w in return (a, w `mappend` z)

instance (MonadAccumulator w m) => MonadAccumulator w (ReaderT s m) where
  append = lift . append
  accumulate = lift . accumulate

-- EAC: Per Chris's comment, we'll probably also need an instance for RandT

-- | Output the output of the computation as well as the accumulated result.
runAccumulatorT :: (Monoid w) => StateT w m a -> m (a, w)
runAccumulatorT = flip runStateT mempty

-- | Output the output of the computation, discarding the accumulated result.
evalAccumulatorT :: (Monoid w, Functor m) => StateT w m a -> m a
evalAccumulatorT = (fst <$>) . runAccumulatorT

-- | Turn a computation that only requires a `MonadReader` constraint into
-- one that works with a `MonadAccumulator` constraint.
embedReader :: (MonadAccumulator s m) => Reader s a -> m a
embedReader x = accumulate $ \s -> (runReader x s, s)