{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (
  Event,
  never,
  attach,
  switcherE,
  Behavior,
  switcherB,
  stepperB,
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
  Filterable (..),
) where

import Control.Concurrent.STM (TMVar, TQueue, atomically, newTMVarIO, newTQueueIO, newTVarIO, putTMVar, readTVar, takeTMVar, tryReadTQueue, writeTQueue, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forever, join, unless)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Coerce (coerce)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.IORef (IORef, atomicModifyIORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semialign (Semialign (..))
import Data.These (These (..), these)
import Data.Unique.Tag (GCompare, GEq, RealWorld, Tag, newTag)
import System.IO.Unsafe (unsafePerformIO)
import Witherable (Filterable (..))

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

{- | When lifting an IO action into a 'Behavior', such as in 'fromIO' or 'switcherB',
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
        queueWrite (liftIO $ writeIORef cacheRef Nothing)
        pure a
      Just cached ->
        pure cached

{-# NOINLINE fromIO #-}
fromIO :: IO a -> Behavior a
fromIO action = unsafePerformIO $ cachedBehavior (liftIO action)

-- | @[[ switcherB b e ]] = \\t -> last ([[ b ]] : before [[ e ]] t) t@
switcherB :: Behavior a -> Event (Behavior a) -> FRP (Behavior a)
switcherB ba eba =
  liftInternal $ do
    ref <- liftIO $ newIORef ba
    _unsub_eba <- subscribe eba $ \ba' -> queueWrite (liftIO $ writeIORef ref ba')
    cachedBehavior
      ( do
          Behavior action <- liftIO $ readIORef ref
          action
      )

-- | @[[ stepperB a e ]] = [[ switcherB (pure a) (fmap pure e) ]]@
stepperB :: a -> Event a -> FRP (Behavior a)
stepperB a ea =
  liftInternal $ do
    ref <- liftIO $ newIORef a
    _unsub_ea <- subscribe ea $ \a' -> queueWrite (liftIO $ writeIORef ref a')
    pure . Behavior . liftIO $ readIORef ref

-- | @[[ Event a ]] = [(Time, a)]@
data Event a
  = Never
  | Event {unEvent :: Internal (EventKey a)}

-- | @[[ fmap f e ]] = [ (t, f a) | (t, a) <- [[ e ]] ]@
instance Functor Event where
  fmap f ea =
    Event $ do
      key <- newEventKey
      _unsub_ea <- subscribe ea (notify key . f)
      pure key

instance Semialign Event where
  {-
  align Never Never = Never
  align ea Never = fmap This ea
  align Never eb = fmap That eb
  align ea@Event{} eb@Event{} =
  -}
  align ea eb =
    Event $ do
      key <- newEventKey
      ref <- liftIO $ newIORef Nothing
      propagateQueued <- liftIO $ newTVarIO False
      let
        propagate = do
          mValue <- liftIO $ readIORef ref
          for_ mValue (notify key)
      let
        queuePropagation = do
          env <- ask
          liftIO . atomically $ do
            b <- readTVar propagateQueued
            unless b $ do
              {-
              When `ref` contains a one-sided value (`This` or `That`) how do I
              know whether it's because the other side doesn't fire for this logical
              time, or it's because the other side just hasn't fired *yet*? (because I
              handle notifications fairly synchronously)

              I put an action onto a queue, and everything on that queue is
              run after this "round" of propagation for the logical time. If neither
              input event depends on the output event, then by the time the queued action
              is run it's a fair measure of whether the input events succeeded.

              The final question is whether there are any valid `align` events where one of the
              inputs depends on the output. Maybe not?
              -}
              writeTQueue (notifyQueue env) propagate
              writeTVar propagateQueued True
              -- Reset internal state after propagation has finished.
              writeTQueue
                (writeQueue env)
                ( liftIO $ do
                    writeIORef ref Nothing
                    atomically (writeTVar propagateQueued False)
                )
      _unsub_ea <- subscribe ea $ \a -> do
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
      _unsub_eb <- subscribe eb $ \b -> do
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
      pure key

instance Filterable Event where
  mapMaybe _ Never = Never
  mapMaybe f ea =
    Event $ do
      key <- newEventKey
      _unsub_ea <- subscribe ea $ traverse_ (notify key) . f
      pure key

-- | @[[ never ]] = [(+inf, _|_)]@
never :: Event a
never = Never

-- | @[[ attach e b ]] = [ (t, (a, [[ b ]] t)) | (t, a) <- [[ e ]] ]@
attach :: Event a -> Behavior b -> Event (a, b)
attach ea (Behavior mb) =
  Event $ do
    key <- newEventKey
    _unsub_eb <- subscribe ea (\a -> do b <- mb; notify key (a, b))
    pure key

-- @[[ switcherE ea eea ]] = [ if t <= t' then (t, a) else (t'', a') | (t, a) <- [[ ea ]], (t', ea') <- [[ eea ]], (t'', a') <- [[ switcherE ea' eea ]] ]@
switcherE :: Event a -> Event (Event a) -> Event a
switcherE ea eea =
  Event $ do
    key <- newEventKey
    unsub_ea <- subscribe ea $ notify key
    unsubscribeRef <- liftIO $ newIORef unsub_ea
    _unsub_eea <- subscribe eea $ \ea' -> do
      queueWrite $ do
        join $ liftIO (readIORef unsubscribeRef)
        unsub_ea' <- subscribe ea' $ notify key
        liftIO $ writeIORef unsubscribeRef unsub_ea'
    pure key

newtype EventKey a = EventKey (Tag RealWorld a)
  deriving (GEq, GCompare)

data Env = Env
  { events :: IORef (DMap EventKey EventSubscriptions)
  , propagationLock :: TMVar ()
  , writeQueue :: TQueue (Internal ())
  , notifyQueue :: TQueue (Internal ())
  }

newtype SubscriptionKey = SubscriptionKey Int
  deriving (Eq, Ord)

data EventSubscriptions a = EventSubscriptions !Int (Map SubscriptionKey (a -> Internal ()))

newtype FRP a = FRP (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newtype Internal a = Internal (ReaderT Env IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadFix)

runInternal :: Env -> Internal a -> IO a
runInternal env (Internal ma) = runReaderT ma env

liftInternal :: Internal a -> FRP a
liftInternal = coerce

newEventKey :: Internal (EventKey a)
newEventKey = do
  eventsRef <- asks events
  key <- liftIO $ EventKey <$> newTag
  liftIO . modifyIORef eventsRef $ DMap.insert key (EventSubscriptions 0 mempty)
  pure key

flushWriteQueue :: Internal ()
flushWriteQueue = do
  queue <- asks writeQueue
  mAction <- liftIO . atomically $ tryReadTQueue queue
  case mAction of
    Nothing ->
      pure ()
    Just action -> do
      action
      flushWriteQueue

queueWrite :: Internal () -> Internal ()
queueWrite action = do
  queue <- asks writeQueue
  liftIO $ atomically $ writeTQueue queue action

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
    key <- newEventKey
    env <- ask
    pure
      ( \a -> bracket (atomically $ takeTMVar lock) (atomically . putTMVar lock) $ \() ->
          runInternal env $ do
            notify key a
            flushNotifyQueue
            flushWriteQueue
      , Event (pure key)
      )

perform :: forall a. Event (IO a) -> FRP (Event a)
perform eioa = do
  (triggera, ea) <- triggerEvent
  liftInternal . void $ subscribe eioa (\ma -> liftIO $ ma >>= triggera)
  pure ea

perform_ :: forall a. Event (IO a) -> FRP ()
perform_ eioa =
  liftInternal . void $ subscribe eioa (liftIO . void)

subscribe :: Event a -> (a -> Internal ()) -> Internal (Internal ())
subscribe Never _ = pure (pure ())
subscribe (Event getEventKey) f = do
  eventsRef <- asks events
  eventKey <- getEventKey
  subKey <-
    liftIO
      . atomicModifyIORef eventsRef
      $ ( \es ->
            let EventSubscriptions nextSubKey old = es DMap.! eventKey
                subKey = SubscriptionKey nextSubKey
             in ( DMap.insert
                    eventKey
                    (EventSubscriptions (nextSubKey + 1) $ Map.insert subKey f old)
                    es
                , subKey
                )
        )
  pure
    ( liftIO
        . modifyIORef eventsRef
        $ DMap.adjust
          (\(EventSubscriptions nextSubKey old) -> EventSubscriptions nextSubKey $ Map.delete subKey old)
          eventKey
    )

notify :: EventKey a -> a -> Internal ()
notify tag a = do
  eventSubscriptions <- liftIO . readIORef =<< asks events
  case DMap.lookup tag eventSubscriptions of
    Nothing -> undefined
    Just (EventSubscriptions _ subs) ->
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