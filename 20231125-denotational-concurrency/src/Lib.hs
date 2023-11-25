{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.Async
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.State (StateT (..))
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Compose (ComposeT (..))
import Control.Monad.Writer (WriterT (..))
import List.Transformer (ListT (..), Step (..))

class (Monad m) => MonadConc m where
  concurrently :: m a -> m b -> m (a, b)

instance MonadConc IO where
  concurrently = Control.Concurrent.Async.concurrently

newtype ConcT t m a = ConcT {getConcT :: ComposeT t ListT m a}

runConcT :: ConcT t m a -> t (ListT m) a
runConcT (ConcT tma) = getComposeT tma

collectListT :: (Monad m) => ListT m a -> m [a]
collectListT (ListT ma) = do
  step <- ma
  case step of
    Nil -> pure []
    Cons a mas -> (a :) <$> collectListT mas

liftT :: (MFunctor t, Monad m) => t m a -> ConcT t m a
liftT tma = ConcT (ComposeT $ hoist lift tma)

instance (MonadTrans t, Monad m) => Functor (ConcT t m) where
  fmap f (ConcT ma) = ConcT (fmap f ma)

instance (MonadTrans t, Monad m) => Applicative (ConcT t m) where
  pure = ConcT . pure
  (<*>) (ConcT mf) (ConcT ma) = ConcT (mf <*> ma)

instance (MonadTrans t, Monad m) => Monad (ConcT t m) where
  (>>=) (ConcT ma) f = ConcT $ ma >>= getConcT . f

instance (MApplicative t, Monad m) => MonadConc (ConcT t m) where
  concurrently (ConcT ma) (ConcT mb) =
    ConcT
      . ComposeT
      $ mapply
        (<|>)
        ( getComposeT $ do
            a <- ma
            b <- mb
            pure (a, b)
        )
        ( getComposeT $ do
            b <- mb
            a <- ma
            pure (a, b)
        )

instance (MonadTrans t, MFunctor t) => MonadTrans (ConcT t) where
  lift = ConcT . lift

{-
s -> m (s, a) ~~~ s -> [(s, a)]
Either e a    ~~~ [Either e a]
(w, a)        ~~~ [(w, a)]
r -> a        ~~~ r -> [a]
-}

class (MFunctor t, MonadTrans t) => MApplicative t where
  mapply :: (forall x. m x -> n x -> o x) -> t m a -> t n a -> t o a

instance MApplicative (StateT s) where
  mapply f (StateT ma) (StateT mb) = StateT (\s -> f (ma s) (mb s))

instance MApplicative (ExceptT e) where
  mapply f (ExceptT ma) (ExceptT mb) = ExceptT (f ma mb)

instance (Monoid w) => MApplicative (WriterT w) where
  mapply f (WriterT ma) (WriterT mb) = WriterT (f ma mb)

instance (MApplicative t, MApplicative u) => MApplicative (ComposeT t u) where
  mapply f (ComposeT ma) (ComposeT mb) = ComposeT (mapply (mapply f) ma mb)