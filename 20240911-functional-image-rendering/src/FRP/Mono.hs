{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module FRP.Mono
  ( MonoFRP, runMonoFRP, MonoEvent, MonoReactive, mkEvent

  -- * Internals
  , atB
  , traceE
  )
where

import FRP (FRP(..), Behaviour (..), Fun (..))
import Data.IORef (readIORef, newIORef, writeIORef, atomicModifyIORef)
import qualified Data.IntMap as IntMap
import Data.Foldable (traverse_)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM.TMVar (putTMVar, newEmptyTMVarIO, takeTMVar)
import Control.Applicative ((<|>))
import Control.Monad.Fix (MonadFix)

data Env = Env{ _start :: UTCTime, _eventQueue :: TQueue (IO ()) }

newtype MonoFRP a = MonoFRP (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadFix)

runMonoFRP :: MonoFRP (MonoEvent (), a) -> (a -> IO ()) -> IO ()
runMonoFRP (MonoFRP m) k = do
  start <- getCurrentTime
  eventQueue <- newTQueueIO
  
  quitVar <- newEmptyTMVarIO
  let quit = atomically $ putTMVar quitVar ()

  let
    worker = do
      mAction <-
        atomically $
          Just <$> readTQueue eventQueue <|>
          Nothing <$ takeTMVar quitVar

      case mAction of
        Nothing -> pure ()
        Just action -> do
          action
          worker
  
  withAsync worker $ \workerAsync -> do
    (eQuit, a) <- runReaderT m (Env start eventQueue)
    _subscription <- subscribe eQuit (\_t () -> quit)
    k a
    wait workerAsync
    

newtype MonoEvent a = MonoEvent{ _subscribe :: (Double -> a -> IO ()) -> IO Subscription }

newtype Subscription = Subscription{ _unsubscribe :: IO () }

subscribe :: MonoEvent a -> (Double -> a -> IO ()) -> IO Subscription
subscribe (MonoEvent m) = m

unsubscribe :: Subscription -> IO ()
unsubscribe (Subscription m) = m

instance Functor MonoEvent where
  fmap f e = MonoEvent $ \cb -> subscribe e (\t -> cb t . f)

newtype MonoReactive a = MonoReactive (IO a)
  deriving (Functor, Applicative, Monad)

atR :: MonoReactive a -> IO a
atR (MonoReactive m) = m

atB :: Behaviour MonoFRP a -> Double -> IO a
atB (Behaviour rfun) t = do
  fun <- atR rfun
  case fun of
    K a -> pure a
    Fun f -> pure $ f t

instance FRP MonoFRP where
  type Event MonoFRP = MonoEvent
  never = MonoEvent{ _subscribe = \_ -> pure Subscription{ _unsubscribe = pure () } }

  accumE a ef = MonoFRP $ do
    ref <- liftIO $ newIORef a
    pure
      MonoEvent{ _subscribe = \cb ->
        subscribe ef (\t f -> do
          x' <- atomicModifyIORef ref (\x -> let x' = f x in (x', x'))
          cb t x'
        )
      }
  
  type Reactive MonoFRP = MonoReactive
  
  stepperR a ea = MonoFRP $ do
    ref <- liftIO $ newIORef a
    _subscription <- liftIO $ subscribe ea (\_t -> writeIORef ref)
    pure $ MonoReactive (readIORef ref)
  
  accumR a ef = MonoFRP $ do
    ref <- liftIO $ newIORef a
    _subscription <- liftIO $ subscribe ef (\_t f -> atomicModifyIORef ref (\x -> (f x, ())))
    pure $ MonoReactive (readIORef ref)

  tagR ra eb =
    MonoEvent{ _subscribe = \cb ->
      subscribe eb (\t b -> do
        a <- atR ra
        cb t (a, b))
    }
  
  tagB ba eb =
    MonoEvent{ _subscribe = \cb ->
      subscribe eb (\t b -> do
        a <- atB ba t
        cb t (a, b))
    }

mkEvent :: MonoFRP (a -> IO (), MonoEvent a)
mkEvent = MonoFRP $ do
  start <- asks _start
  eventQueue <- asks _eventQueue
  nextKeyRef <- liftIO $ newIORef 0
  subscribersRef <- liftIO $ newIORef IntMap.empty
  pure
    ( \value -> do
        atomically . writeTQueue eventQueue $ do
          now <- getCurrentTime
          let !t = realToFrac @NominalDiffTime @Double $ now `diffUTCTime` start
          readIORef subscribersRef >>= traverse_ (\cb -> cb t value)
    , MonoEvent $ \cb -> do
        key <- atomicModifyIORef nextKeyRef (\x -> (x + 1, x))
        atomicModifyIORef subscribersRef (\x -> (IntMap.insert key cb x, ()))
        pure Subscription{ _unsubscribe = atomicModifyIORef subscribersRef (\x -> (IntMap.delete key x, ())) }
    )

traceE :: Show a => MonoEvent a -> MonoEvent a
traceE ea =
  MonoEvent{ _subscribe = \cb -> do
    subscribe ea (\t a -> print (t, a) *> cb t a)
  }
