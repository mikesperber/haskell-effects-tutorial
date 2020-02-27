{-# LANGUAGE GADTs, DeriveFunctor, DeriveAnyClass #-}

module Free where

data It i a =
    Done a
  | Get (i -> It i a)
  deriving (Functor, Applicative)

ask::It  i i
ask = Get Done

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

instance Monad (It i) where
    return = Done

    Done x >>= k = k x
    Get k' >>= k = Get (k' >>> k)

data Free f a =
     Pure a
   | Impure (f (Free f a))
   deriving (Functor, Applicative)

instance Functor f => Monad (Free f) where
    return = Pure
    
    (Pure a) >>= k = k a
    (Impure f) >>= k = Impure (fmap (>>= k) f)

data Reader i x = FGet (i -> x)
  deriving Functor

type FIit i a = Free (Reader i) a

data FFree f a where
    FPure :: a -> FFree f a
    FImpure :: f a -> (a -> FFree f b) -> FFree f b

instance Functor (FFree f) where

instance Applicative (FFree f) where

instance Monad (FFree f) where
  return = FPure

  FImpure fx k' >>= k = FImpure fx (k' >>> k)

