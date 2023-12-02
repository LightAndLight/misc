{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
  Event,
  never,
  attach,
  Behavior,
  switcherM,
  stepperM,
  fromIO,
  FRP,
  frpMain,
  stdio,
  triggerEvent,
  perform,
  perform_,

  -- * Re-exports
  Semialign (..),
  These (..),
) where

import Control.Concurrent.STM (TMVar, TQueue, atomically, newTMVarIO, newTQueueIO, newTVarIO, putTMVar, readTVar, takeTMVar, tryReadTQueue, writeTQueue, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Coerce (coerce)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Semialign (Semialign (..))
import Data.These (These (..), these)
import Data.Unique.Tag (RealWorld, Tag, newTag)
import System.IO.Unsafe (unsafePerformIO)

-- | @[[ Behavior a ]] = Time -> a@
newtype Behavior a = Behavior {unBehavior :: Internal a}

-- | @[[ fmap f b ]] = f . [[ b ]]@
instance Functor Behavior where
  fmap f (Behavior ba) = Behavior (fmap f ba)

{- | @[[ pure a ]] = const a@
@[[ bf <*> ba]] = [[ bf ]] <*> [[ ba ]]
-}
instance Applicative Behavior where
  pure = Behavior . pure
  Behavior bf <*> Behavior ba = Behavior (bf <*> ba)

{- | When lifting an IO action into a 'Behavior', such as in 'fromIO' or 'switcherM',
we have to arrange for sampling of the behavior to return the same value at the
same logical time.

The result of the IO action can be continually varying, such as the current time.
When the behavior is first sampled at a particular logical time, the value returned
by the IO action is cached until the next logical time.
-}
cachedBehavior :: (MonadIO m) => Internal a -> m (Behavior a)
cachedBehavior action = do
  cacheRef <- liftIO $ newIORef Nothing
  pure . Behavior $ do
    mCached <- liftIO $ readIORef cacheRef
    case mCached of
      Nothing -> do
        a <- action
        liftIO . writeIORef cacheRef $ Just a
        do
          queue <- asks writeQueue
          liftIO $ atomically $ writeTQueue queue (writeIORef cacheRef Nothing)
        pure a
      Just cached ->
        pure cached

{-# NOINLINE fromIO #-}
fromIO :: IO a -> Behavior a
fromIO action = unsafePerformIO $ cachedBehavior (liftIO action)

-- | @[[ switcher b e ]] = \\t -> last ([[ b ]] : before [[ e ]] t) t@
switcherM :: Behavior a -> Event (Behavior a) -> FRP (Behavior a)
switcherM ba eba =
  liftInternal $ do
    ref <- liftIO $ newIORef ba
    subscribe
      eba
      ( \ba' -> do
          queue <- asks writeQueue
          liftIO . atomically . writeTQueue queue $ writeIORef ref ba'
      )
    cachedBehavior
      ( do
          Behavior action <- liftIO $ readIORef ref
          action
      )

-- | @[[ stepper a e ]] = [[ switcherB (pure a) (fmap pure e) ]]@
stepperM :: a -> Event a -> FRP (Behavior a)
stepperM a ea =
  liftInternal $ do
    ref <- liftIO $ newIORef a
    subscribe
      ea
      ( \a' -> do
          queue <- asks writeQueue
          liftIO . atomically . writeTQueue queue $ writeIORef ref a'
      )
    pure . Behavior . liftIO $ readIORef ref

-- | @[[ Event a ]] = [(Time, a)]@
data Event a
  = Never
  | Event {unEvent :: Internal (Tag RealWorld a)}

-- | @[[ fmap f e ]] = [ (t, f a) | (t, a) <- [[ e ]] ]@
instance Functor Event where
  fmap f ea =
    Event $ do
      tag <- newEventTag
      subscribe ea (notify tag . f)
      pure tag

instance Semialign Event where
  align ea eb =
    Event $ do
      tag <- newEventTag
      ref <- liftIO $ newIORef Nothing
      propagateQueued <- liftIO $ newTVarIO False
      let
        propagate = do
          mValue <- liftIO $ readIORef ref
          for_ mValue (notify tag)
      let
        queuePropagation = do
          env <- ask
          liftIO . atomically $ do
            b <- readTVar propagateQueued
            unless b $ do
              writeTQueue (writeQueue env) $ do
                writeIORef ref Nothing
                atomically (writeTVar propagateQueued False)
              writeTQueue (notifyQueue env) propagate
              writeTVar propagateQueued True
      subscribe ea $ \a -> do
        queuePropagation
        liftIO
          $ atomicModifyIORef
            ref
            ( maybe
                (Just $ This a, ())
                ( these
                    (error "duplicate write to align left - This")
                    (\b -> (Just $ These a b, ()))
                    (error "duplicate write to align left - These")
                )
            )
      subscribe eb $ \b -> do
        queuePropagation
        liftIO
          $ atomicModifyIORef
            ref
            ( maybe
                (Just $ That b, ())
                ( these
                    (\a -> (Just $ These a b, ()))
                    (error "duplicate write to align right - That")
                    (error "duplicate write to align right - These")
                )
            )
      pure tag

-- | @[[ never ]] = [(+inf, _|_)]@
never :: Event a
never = Never

attach :: Event a -> Behavior b -> Event (a, b)
attach ea (Behavior mb) =
  Event $ do
    tag <- newEventTag
    subscribe ea (\a -> do b <- mb; notify tag (a, b))
    pure tag

data Env = Env
  { events :: IORef (DMap (Tag RealWorld) EventSubscriptions)
  , propagationLock :: TMVar ()
  , writeQueue :: TQueue (IO ())
  , notifyQueue :: TQueue (Internal ())
  }

newtype EventSubscriptions a = EventSubscriptions [a -> Internal ()]

newtype FRP a = FRP (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newtype Internal a = Internal (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadFix)

runInternal :: Env -> Internal a -> IO a
runInternal env (Internal ma) = runReaderT ma env

liftInternal :: Internal a -> FRP a
liftInternal = coerce

newEventTag :: Internal (Tag RealWorld a)
newEventTag = do
  eventsRef <- asks events
  tag <- liftIO newTag
  liftIO . modifyIORef eventsRef $ DMap.insert tag (EventSubscriptions mempty)
  pure tag

flushWriteQueue :: Internal ()
flushWriteQueue = do
  queue <- asks writeQueue
  mAction <- liftIO . atomically $ tryReadTQueue queue
  case mAction of
    Nothing ->
      pure ()
    Just action -> do
      liftIO action
      flushWriteQueue

flushNotifyQueue :: Internal ()
flushNotifyQueue = do
  queue <- asks notifyQueue
  mAction <- liftIO . atomically $ tryReadTQueue queue
  case mAction of
    Nothing ->
      pure ()
    Just action -> do
      action
      flushNotifyQueue

triggerEvent :: FRP (a -> IO (), Event a)
triggerEvent =
  liftInternal $ do
    lock <- asks propagationLock
    tag <- newEventTag
    env <- ask
    pure
      ( \a -> bracket (atomically $ takeTMVar lock) (atomically . putTMVar lock) $ \() ->
          runInternal env $ do
            notify tag a
            flushNotifyQueue
            flushWriteQueue
      , Event (pure tag)
      )

perform :: forall a. Event (IO a) -> FRP (Event a)
perform eioa = do
  (triggera, ea) <- triggerEvent
  liftInternal $ subscribe eioa (\ma -> liftIO $ ma >>= triggera)
  pure ea

perform_ :: forall a. Event (IO a) -> FRP ()
perform_ eioa =
  liftInternal $ subscribe eioa (liftIO . void)

subscribe :: Event a -> (a -> Internal ()) -> Internal ()
subscribe Never _ = pure ()
subscribe (Event getTag) f = do
  eventsRef <- asks events
  tag <- getTag
  liftIO
    . modifyIORef eventsRef
    $ DMap.update (\(EventSubscriptions old) -> Just . EventSubscriptions $ old <> [f]) tag

notify :: Tag RealWorld a -> a -> Internal ()
notify tag a = do
  eventSubscriptions <- liftIO . readIORef =<< asks events
  case DMap.lookup tag eventSubscriptions of
    Nothing -> undefined
    Just (EventSubscriptions subs) ->
      traverse_ ($ a) subs

stdio :: (Event String -> FRP (Event String)) -> FRP ()
stdio program = do
  (sendIn, eIn) <- triggerEvent
  eOut <- program eIn
  perform_ $ putStrLn <$> eOut
  liftIO . forever $ do
    line <- getLine
    sendIn line

frpMain :: FRP a -> IO a
frpMain (FRP ma) = do
  eventsRef <- newIORef mempty
  lock <- newTMVarIO ()
  wQueue <- newTQueueIO
  nQueue <- newTQueueIO
  runReaderT
    ma
    Env
      { events = eventsRef
      , propagationLock = lock
      , writeQueue = wQueue
      , notifyQueue = nQueue
      }