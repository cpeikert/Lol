{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Crypto.Alchemy.MonadAccumulator where

import Control.Monad.Reader
import Control.Monad.State

-- | An append-only state monad.

class (Monoid w, Monad m) => MonadAccumulator w m where
  -- | Append the given value to the state inside the monad.
  append :: w -> m ()

  -- | Embed a simple append action into the monad.  The given
  -- function should output a value and the state /to be appended/ to
  -- the previous state.
  accumulate  :: (w -> (a,w)) -> m a

-- EAC: (Monad m) *should* be implied by (MonadState w m), but GHC can't figure that out...
instance (Monad m, MonadState w m, Monoid w) => MonadAccumulator w m where
  append w = modify (`mappend` w)
  accumulate f = state $ \w -> let (a,z) = f w in (a, w `mappend` z)

runAccumulator :: (Monoid w) => State w a -> (a, w)
runAccumulator = flip runState mempty

embedReader :: (MonadAccumulator s m) => Reader s a -> m a
embedReader x = accumulate $ \s -> (runReader x s, s)