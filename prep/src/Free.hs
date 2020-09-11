{-# LANGUAGE GADTs, DeriveFunctor, DeriveAnyClass #-}

module Free where

import Control.Monad.Identity

data Reader1 env a =
    Done a
  | Get (env -> Reader1 env a)
  deriving (Functor, Applicative)

ask1 :: Reader1 env env
ask1 = Get Done

(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

instance Monad (Reader1 env) where
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


data Reader' env knot = FGet (env -> knot)
  deriving Functor

type Reader env a = Free (Reader' env) a

ask :: Reader env env
ask = Impure (FGet Pure)

runReader :: env -> Reader env a -> a
runReader _env (Pure result) = result
runReader env (Impure (FGet f)) = runReader env (f env)

runFree :: Monad m => (f (Free f a) -> (Free f a -> m a) -> m a) -> Free f a -> m a
runFree _ (Pure result) = return result
runFree rrr (Impure command) = rrr command (runFree rrr)

runReader' :: env -> Reader env a -> Identity a
runReader' env m = runFree (\ (FGet cont) r -> r (cont env)) m

data FFree f a where
    FPure :: a -> FFree f a
    FImpure :: f a -> (a -> FFree f b) -> FFree f b

data FReader i x where
  FFGet :: FReader i i

type IT i a = FFree (FReader i) a

instance Functor (FFree f) where

instance Applicative (FFree f) where

instance Monad (FFree f) where
  return = FPure

  FImpure fx k' >>= k = FImpure fx (k' >>> k)

