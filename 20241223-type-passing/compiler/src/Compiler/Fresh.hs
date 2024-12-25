{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Compiler.Fresh where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Writer.CPS (WriterT)
import Control.Monad.Writer.Class (MonadWriter)

class Monad m => MonadFresh m where
  fresh :: m Word
  default fresh :: (m ~ t n, MonadTrans t, MonadFresh n) => m Word
  fresh = lift fresh

instance MonadFresh m => MonadFresh (ReaderT r m)
instance MonadFresh m => MonadFresh (ExceptT e m)
instance MonadFresh m => MonadFresh (WriterT w m)
instance MonadFresh m => MonadFresh (StateT s m)

newtype FreshT m a = FreshT (StateT Word m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadReader r, MonadError e, MonadWriter w)

instance MonadState s m => MonadState s (FreshT m) where
  get = FreshT $ lift get
  put = FreshT . lift . put

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (FreshT ma) = evalStateT ma 0

instance Monad m => MonadFresh (FreshT m) where
  fresh = FreshT $ do
    n <- get
    put $ n + 1
    pure n
