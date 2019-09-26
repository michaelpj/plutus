{-# LANGUAGE NoImplicitPrelude #-}
module Language.PlutusTx.Applicative where

import           Language.PlutusTx.Functor
import           Prelude                   (Maybe (..))

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
    {-# MINIMAL pure, (<*>) #-}
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    --
    -- A few functors support an implementation of '<*>' that is more
    -- efficient than the default one.
    (<*>) :: f (a -> b) -> f a -> f b

{-# INLINABLE liftA2 #-}
-- | Lift a binary function to actions.
--
-- Some functors support an implementation of 'liftA2' that is more
-- efficient than the default one. In particular, if 'fmap' is an
-- expensive operation, it is likely better to use 'liftA2' than to
-- 'fmap' over the structure and then use '<*>'.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x = (<*>) (fmap f x)

{-# INLINABLE (*>) #-}
-- | Sequence actions, discarding the value of the first argument.
(*>) :: Applicative f => f a -> f b -> f b
a1 *> a2 = (id <$ a1) <*> a2

{-# INLINABLE (<*) #-}
-- | Sequence actions, discarding the value of the second argument.
(<*) :: Applicative f => f a -> f b -> f a
(<*) = liftA2 const

instance Applicative Maybe where
    pure = Just
    Just f <*> Just x = Just (f x)
    _ <*> _ = Nothing
