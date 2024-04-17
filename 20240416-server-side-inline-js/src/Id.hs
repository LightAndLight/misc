{-# language DefaultSignatures #-}
module Id where

import Control.Monad.Writer.CPS (WriterT)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Functor.Identity (Identity (..))
import Control.Monad.State.Class (get, put)

class Monad m => MonadFresh m where
  freshId :: m Int
  default freshId :: (m ~ t n, MonadTrans t, MonadFresh n) => m Int
  freshId = lift freshId

instance (MonadFresh m, Monoid w) => MonadFresh (WriterT w m)

newtype FreshT m a = FreshT (StateT Int m a)
  deriving (Functor, Applicative, Monad)

instance MonadTrans FreshT where
  lift = FreshT . lift

instance Monad m => MonadFresh (FreshT m) where
  freshId = FreshT $ do
    n <- get
    put $! n + 1
    pure n

runFresh :: FreshT Identity a -> a
runFresh (FreshT ma) = runIdentity (evalStateT ma 0)
