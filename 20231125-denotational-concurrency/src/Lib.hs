{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.Async
import Control.Monad.Except (ExceptT (..))
import Control.Monad.Morph (MFunctor, hoist)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.STM (STM)
import qualified Control.Monad.STM
import Control.Monad.State (StateT (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Compose (ComposeT (..))
import Control.Monad.Writer (WriterT (..))
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Monoid.Commutative (CommutativeMonoid)
import Data.Semigroup.Commutative (CommutativeSemigroup)
import GHC.Conc (par)
import List.Transformer (ListT (..), Step (..))

class (Monad m) => MonadConc m where
  concurrently :: m a -> m b -> m (a, b)

instance MonadConc IO where
  concurrently = Control.Concurrent.Async.concurrently

instance MonadConc Identity where
  concurrently (Identity a) (Identity b) = Identity (a `par` b `par` (a, b))

instance (MonadConc m) => MonadConc (ReaderT r m) where
  concurrently (ReaderT ma) (ReaderT mb) =
    ReaderT $ \r -> concurrently (ma r) (mb r)

instance (CommutativeSemigroup s, MonadConc m) => MonadConc (StateT s m) where
  concurrently (StateT ma) (StateT mb) =
    StateT $ \s -> (\((a, sa), (b, sb)) -> ((a, b), sa <> sb)) <$> concurrently (ma s) (mb s)

instance (CommutativeMonoid w, MonadConc m) => MonadConc (WriterT w m) where
  concurrently (WriterT ma) (WriterT mb) =
    WriterT $ (\((a, sa), (b, sb)) -> ((a, b), sa <> sb)) <$> concurrently ma mb

instance (CommutativeSemigroup e, MonadConc m) => MonadConc (ExceptT e m) where
  concurrently (ExceptT ma) (ExceptT mb) =
    ExceptT $ concurrently ma mb <&> \case
      (Left ea, Left eb) -> Left (ea <> eb)
      (Left ea, Right _) -> Left ea
      (Right _, Left eb) -> Left eb
      (Right a, Right b) -> Right (a, b)

newtype Concurrently m a = Concurrently (m a)
  deriving (Functor)

instance (MonadConc m) => Applicative (Concurrently m) where
  pure = Concurrently . pure
  Concurrently mf <*> Concurrently ma = Concurrently (uncurry ($) <$> concurrently mf ma)

class (MonadConc m, Monad (Transaction m)) => MonadTransaction m where
  type Transaction m :: Type -> Type
  atomically :: Transaction m a -> m a

instance MonadTransaction IO where
  type Transaction IO = STM
  atomically = Control.Monad.STM.atomically

instance MonadTransaction Identity where
  type Transaction Identity = AtomicIdentity
  atomically = getAtomicIdentity

newtype AtomicIdentity a = AtomicIdentity {getAtomicIdentity :: Identity a}
  deriving (Functor, Applicative, Monad)

instance (CommutativeSemigroup s, MonadTransaction m) => MonadTransaction (StateT s m) where
  type Transaction (StateT s m) = AtomicStateT s (Transaction m)
  atomically (AtomicStateT (StateT ma)) = StateT $ \s -> atomically (ma s)

newtype AtomicStateT s m a = AtomicStateT (StateT s m a)
  deriving (Functor, Applicative, Monad, MonadState s, MonadTrans)

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