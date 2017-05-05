{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.MonadAccumulator where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans  (lift)
import Control.Monad.Writer

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

instance {-# OVERLAPPING #-} (Monoid w, Monad m) =>
  MonadAccumulator w (StateT w m) where

  append w = modify (`mappend` w)
  accumulate f = StateT $ \w -> let (a,z) = f w in return (a, w `mappend` z)

instance (MonadAccumulator w m) => MonadAccumulator w (ReaderT s m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m) => MonadAccumulator w (WriterT w m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m) => MonadAccumulator w (RandT g m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m, Error e) => MonadAccumulator w (ErrorT e m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m) => MonadAccumulator w (ExceptT e m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m) => MonadAccumulator w (ListT m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m) => MonadAccumulator w (ContT r m) where
  append = lift . append
  accumulate = lift . accumulate

instance (MonadAccumulator w m, Monoid t) => MonadAccumulator w (RWST r t s m) where
  append = lift . append
  accumulate = lift . accumulate


-- | Perform the action and output the result along with the
-- accumulated state.
runAccumulatorT :: (Monoid w) => StateT w m a -> m (a, w)
runAccumulatorT = flip runStateT mempty

-- | Perform the action and the result, discarding the accumulated
-- state.
evalAccumulatorT :: (Monoid w, Functor m) => StateT w m a -> m a
evalAccumulatorT = (fst <$>) . runAccumulatorT

-- | Embed a computation that only requires a 'Reader' into one that
-- works with a 'MonadAccumulator'.
readerToAccumulator :: (MonadAccumulator r m) => Reader r a -> m a
readerToAccumulator x = accumulate $ \r -> (runReader x r, r)
