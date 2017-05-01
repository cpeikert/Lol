
module Crypto.Alchemy.Language.Monad where

-- | Symantics for functors: promotes any 'Functor' to the object
-- language. (Instances should obey the monad laws.)

class Functor_ expr where
  -- | Object-language analogue of 'fmap'.
  fmap_ :: (Functor f) => expr e (a -> b) -> expr e (f a) -> expr e (f b)

-- | Infix synonym for 'fmap_'.
(<$:>) :: (Functor_ expr, Functor f) =>
          expr e (a -> b) -> expr e (f a) -> expr e (f b)
(<$:>) = fmap_

-- | Symantics for applicative monads: promotes any 'Applicative' to
-- the object language. (Instances should obey the monad laws.)

class (Functor_ expr) => Applicative_ expr where
  -- | Object-language analogue of 'pure'.
  pure_  :: (Applicative f) => expr e a -> expr e (f a)

  -- | Object-language analogue of '(<*>)'.
  (<*:>) :: (Applicative f) => expr e (f (a -> b)) -> expr e (f a) -> expr e (f b)

-- | Symantics for monads: promotes any 'Monad' to the object
-- language. (Instances should obey the monad laws.)

class (Applicative_ expr) => Monad_ expr where
  -- | Object-language analogue of '(>>=)'.
  (>>=:) :: (Monad m) => expr e (m a) -> expr e (a -> m b) -> expr e (m b)

return_ :: (Monad_ expr, Monad m) => expr e a -> expr e (m a)
return_ = pure_
