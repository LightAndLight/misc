{-# LANGUAGE DerivingVia #-}

module SPWA.Session (ConnectionId (..), Session (..), SessionEnv (..), forkSession) where

import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (TQueue, TVar, atomically, modifyTVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader)
import qualified Data.ByteString.Lazy as Lazy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.UUID (UUID)

newtype Session a = Session {runSession :: SessionEnv -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SessionEnv) via (ReaderT SessionEnv IO)

data SessionEnv = SessionEnv
  { connectionId :: ConnectionId
  , connections :: TVar (Map ConnectionId (TQueue Lazy.ByteString))
  , threads :: TVar (Map ConnectionId [ThreadId])
  }

newtype ConnectionId = ConnectionId UUID
  deriving (Eq, Ord, Show)

forkSession :: Session () -> Session ()
forkSession (Session ma) = Session $ \env -> do
  threadId <- forkIO $ ma env
  atomically $ modifyTVar (threads env) (Map.insertWith (\new old -> new <> old) (connectionId env) [threadId])