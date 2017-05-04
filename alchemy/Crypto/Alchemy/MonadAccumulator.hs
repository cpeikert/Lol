{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.MonadAccumulator where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans  (lift)

-- | An append-only state monad.

class (Monoid w, Monad m) => MonadAccumulator w m where
  -- | Append the given value to the state inside the monad.
  append :: w -> m ()

  -- | Embed a simple append action into the monad.  The given
  -- function should output a value and the state /to be appended/ to
  -- the previous state.
  accumulate  :: (w -> (a,w)) -> m a

-- CJP: why do these specialized instances instead of the (admittedly
-- too-general) one with head 'w m', and MonadState constraint?

instance (Monoid w, Monad m) => MonadAccumulator w (StateT w m) where
  append w = modify (`mappend` w)
  -- EAC: check this
  accumulate f = StateT $ \w -> let (a,z) = f w in return (a, w `mappend` z)

instance (MonadAccumulator w m) => MonadAccumulator w (ReaderT w m) where
  append = lift . append
  accumulate = lift . accumulate

-- EAC: Per Chris's comment, we'll probably also need an instance for RandT
-- CJP: and many more -- essentially one for every existing state transformer...

-- | Output the output of the computation as well as the accumulated result.
runAccumulator :: (Monoid w) => StateT w m a -> m (a, w)
runAccumulator = flip runStateT mempty

-- | Output the output of the computation, discarding the accumulated result.
evalAccumulator :: (Monoid w, Functor m) => StateT w m a -> m a
evalAccumulator = (fst <$>) . runAccumulator

-- | Embed a computation that only requires a 'Reader' into one that
-- works with a 'MonadAccumulator'.
embedReader :: (MonadAccumulator r m) => Reader r a -> m a
embedReader x = accumulate $ \r -> (runReader x r, r)
