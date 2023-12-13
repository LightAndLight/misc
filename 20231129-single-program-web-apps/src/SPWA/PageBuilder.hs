{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module SPWA.PageBuilder (
  PageBuilder (..),
  runPageBuilder,
  PageBuilderEnv (..),
  PageBuilderState (..),
  ReactiveInfo (..),
  Event (..),
  EventKey (..),
  TriggerId (..),
  Behavior (..),
  Reactive (..),
  ReactiveKey (..),
  appendPostScript,
  mkReactive,
  memoEventWith,
  initBehavior,
  initReactive,
  newEvent,
  newBehavior,
  notify,
  subscribe,
  queueAction,
) where

import Compiler.Plugin.Interface (Quoted (..), quote)
import qualified Compiler.Plugin.Interface as Ctx (Ctx (..))
import qualified Compiler.Plugin.Interface as Expr (compose)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState, gets, modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple
import qualified Data.UUID as UUID
import qualified Data.UUID.V4
import Data.Unique.Tag (GCompare, GEq, RealWorld, Tag, newTag)
import SPWA.DomEvent (DomEvent)
import SPWA.ExprToJavascript (exprToJavascript)
import SPWA.Js (Js (..))
import qualified SPWA.Js as Js
import SPWA.MemoRef (MemoRef, newMemoRef, readMemoRef, setMemoRef, unsafeNewMemoRef)
import SPWA.RPC (RPC)
import SPWA.Send (Send, encodeSend)
import SPWA.Supply (HasSupply (..), freshId)
import SPWA.Template (Template (..))

data PageBuilderEnv = PageBuilderEnv
  { pbe_writeQueueName :: String
  , pbe_connectionIdName :: String
  }

data PageBuilderState = PageBuilderState
  { pbs_supply :: Int
  , pbs_postScript :: Template IO Js
  , pbs_domEventSubscriptions :: Map (String, DomEvent) (String -> Js)
  , pbs_onLoadSubscriptions :: String -> Js
  , pbs_triggerSubscriptions :: String -> Js
  , pbs_hasTrigger :: Bool
  , pbs_rpcs :: Map ByteString RPC
  , pbs_reactives :: DMap ReactiveKey ReactiveInfo
  }

instance HasSupply PageBuilderState where
  getSupply = pbs_supply
  setSupply n s = s{pbs_supply = n}

data ReactiveInfo a where
  ReactiveInfo ::
    (Send a) =>
    { ri_initial :: IO a
    , ri_event :: PageBuilder EventKey
    , ri_behavior :: PageBuilder String
    } ->
    ReactiveInfo a

newtype PageBuilder a = PageBuilder {unPageBuilder :: ReaderT PageBuilderEnv (StateT PageBuilderState IO) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState PageBuilderState
    , MonadReader PageBuilderEnv
    , MonadFix
    , MonadIO
    )

data Behavior a where
  Behavior :: String -> Behavior a
  Current :: (Send a) => Reactive a -> Behavior a
  FmapBehavior :: Quoted (a -> b) -> Behavior a -> Behavior b
  PureBehavior :: Quoted a -> Behavior a
  ApBehavior :: Behavior (a -> b) -> Behavior a -> Behavior b

instance Functor Behavior where
  {-# INLINE fmap #-}
  fmap f = FmapBehavior (quote f)

instance Applicative Behavior where
  {-# INLINE pure #-}
  pure a = PureBehavior (quote a)

  {-# INLINE (<*>) #-} -- so that `quote`s can be replaced
  (<*>) = ApBehavior

data Event :: Type -> Type where
  FromDomEvent :: String -> DomEvent -> Event ()
  Event :: PageBuilder EventKey -> Event a
  FmapEvent :: MemoRef EventKey -> Quoted (a -> b) -> Event a -> Event b

instance Functor Event where
  {-# INLINE fmap #-}
  fmap f a = let !memoRef = unsafeNewMemoRef a in FmapEvent memoRef (quote f) a

data EventKey
  = EventKey_Derived String
  | EventKey_DomEvent String DomEvent
  | EventKey_Trigger TriggerId

newtype TriggerId = TriggerId {getTriggerId :: String}
  deriving (Show)

data Reactive a where
  FmapReactive :: Quoted (a -> b) -> Reactive a -> Reactive b
  Reactive :: ReactiveKey a -> Reactive a

instance Functor Reactive where
  {-# INLINE fmap #-}
  fmap f = FmapReactive (quote f)

newtype ReactiveKey a = ReactiveKey (Tag RealWorld a)
  deriving (GEq, GCompare)

runPageBuilder :: PageBuilder a -> IO a
runPageBuilder ma =
  evalStateT
    ( flip runReaderT PageBuilderEnv{pbe_writeQueueName = "/* unset */", pbe_connectionIdName = "/* unset */"} $ do
        name <- ("writeQueue_" <>) <$> freshId
        connectionIdVar <- ("connectionId_" <>) <$> freshId

        local (\e -> e{pbe_writeQueueName = name, pbe_connectionIdName = connectionIdVar}) . unPageBuilder $ do
          appendPostScript . pure $ Js ["var " <> name <> " = [];"]
          appendPostScript . liftIO $ (\uuid -> Js.line $ "const " <> connectionIdVar <> " = \"" <> UUID.toString uuid <> "\";") <$> Data.UUID.V4.nextRandom
          ma
    )
    PageBuilderState
      { pbs_supply = 0
      , pbs_postScript = mempty
      , pbs_domEventSubscriptions = mempty
      , pbs_onLoadSubscriptions = mempty
      , pbs_triggerSubscriptions = mempty
      , pbs_rpcs = mempty
      , pbs_reactives = mempty
      , pbs_hasTrigger = False
      }

appendPostScript :: Template IO Js -> PageBuilder ()
appendPostScript js = modify $ \s -> s{pbs_postScript = pbs_postScript s <> js}

mkReactive :: (Send a) => IO a -> Event a -> PageBuilder (ReactiveKey a)
mkReactive initial_ eUpdate = do
  key <- liftIO $ ReactiveKey <$> newTag
  modify $ \s ->
    s
      { pbs_reactives =
          DMap.insert
            key
            (ReactiveInfo initial_ (reactiveInitEvent key eUpdate) (reactiveInitBehavior key))
            (pbs_reactives s)
      }
  pure key
 where
  reactiveInitEvent :: ReactiveKey a -> Event a -> PageBuilder EventKey
  reactiveInitEvent reactiveKey event = do
    eventKey <- initEvent event
    modify $ \s ->
      s
        { pbs_reactives =
            DMap.adjust
              (\info -> info{ri_event = pure eventKey})
              reactiveKey
              (pbs_reactives s)
        }
    pure eventKey

  reactiveInitBehavior :: ReactiveKey a -> PageBuilder String
  reactiveInitBehavior reactiveKey = do
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives

    (b, bState) <- newBehavior (ByteString.Lazy.Char8.unpack . encodeSend <$> initial)
    modify $ \s ->
      s
        { pbs_reactives =
            DMap.adjust
              (\info -> info{ri_behavior = pure b})
              reactiveKey
              (pbs_reactives s)
        }

    eventKey <- mkEvent
    writeQueueName <- asks pbe_writeQueueName
    subscribe (Event $ pure eventKey) $ \value -> queueAction writeQueueName (Js [bState <> " = " <> value <> ";"])

    pure b

queueAction :: String -> Js -> Js
queueAction queueName action =
  Js [queueName <> ".push(() => {"]
    <> Js.indent 2 action
    <> Js ["});"]

initEvent :: Event a -> PageBuilder EventKey
initEvent e =
  case e of
    Event mkEvent ->
      mkEvent
    FmapEvent memoRef (Quoted fExpr _) e' ->
      memoEventWith memoRef $ \name -> do
        fJs <- exprToJavascript Ctx.Nil fExpr
        temp <- ("temp_" <>) <$> freshId
        notifyJs <- notify name
        subscribe e' $ \value ->
          Tuple.fst fJs
            <> Js ["const " <> temp <> " = " <> Tuple.snd fJs <> "(" <> value <> ");"]
            <> notifyJs temp
    FromDomEvent elId de ->
      pure $ EventKey_DomEvent elId de

newEvent :: PageBuilder String
newEvent = do
  name <- ("event_" <>) <$> freshId
  appendPostScript . pure $ Js ["const " <> name <> " = { subscribers: [] };"]
  pure name

newBehavior :: IO String -> PageBuilder (String, String)
newBehavior initial = do
  state <- ("state_" <>) <$> freshId
  name <- ("behavior_" <>) <$> freshId
  appendPostScript
    . Template
    $ ( \i ->
          Js
            [ "var " <> state <> " = " <> i <> ";"
            , "const " <> name <> " = () => " <> state <> ";"
            ]
      )
    <$> initial
  pure (name, state)

subscribe :: Event a -> (String -> Js) -> PageBuilder ()
subscribe ea callback = do
  key <- initEvent ea
  case key of
    EventKey_Derived name -> do
      arg <- ("arg_" <>) <$> freshId
      appendPostScript . pure $ Js [name <> ".subscribers.push((" <> arg <> ") => {"] <> Js.indent 2 (callback arg) <> Js ["});"]
    EventKey_DomEvent elId de -> do
      subscribeDomEvent elId de callback
    EventKey_Trigger triggerId -> do
      subscribeTrigger triggerId callback

subscribeDomEvent :: String -> DomEvent -> (String -> Js) -> PageBuilder ()
subscribeDomEvent elId de callback =
  modify $ \s ->
    s
      { pbs_domEventSubscriptions =
          Map.insertWith
            (\new old -> old <> new)
            (elId, de)
            callback
            (pbs_domEventSubscriptions s)
      }

subscribeTrigger :: TriggerId -> (String -> Js) -> PageBuilder ()
subscribeTrigger triggerId callback =
  modify $ \s ->
    s{pbs_triggerSubscriptions = callback <> pbs_triggerSubscriptions s}

memoEventWith :: MemoRef EventKey -> (String -> PageBuilder ()) -> PageBuilder EventKey
memoEventWith memoRef f = do
  mEventKey <- liftIO $ readMemoRef memoRef
  case mEventKey of
    Nothing -> do
      name <- newEvent

      let eventKey = EventKey_Derived name
      liftIO $ setMemoRef memoRef eventKey

      f name

      pure eventKey
    Just eventKey ->
      pure eventKey

notify :: String -> PageBuilder (String -> Js)
notify name = do
  f <- ("f_" <>) <$> freshId
  pure
    $ \value ->
      Js
        [ "for ("
            <> f
            <> " of "
            <> name
            <> ".subscribers) {"
        ]
        <> Js.indent 2 (Js [f <> "(" <> value <> ");"])
        <> Js ["}"]

initReactive :: (Send a) => Reactive a -> PageBuilder (ReactiveKey a)
initReactive (Reactive key) = pure key
initReactive (FmapReactive f (Reactive key)) = do
  ReactiveInfo initial mkEvent _ <- gets $ (DMap.! key) . pbs_reactives
  memoRef <- liftIO newMemoRef
  mkReactive (quotedValue f <$> initial) (FmapEvent memoRef f $ Event mkEvent)
initReactive (FmapReactive f' r'@FmapReactive{}) = go f' r'
 where
  go :: (Send x) => Quoted (a -> x) -> Reactive a -> PageBuilder (ReactiveKey x)
  go f (FmapReactive g r) = go (f `Expr.compose` g) r
  go f (Reactive key) = do
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! key) . pbs_reactives
    memoRef <- liftIO newMemoRef
    mkReactive (quotedValue f <$> initial) (FmapEvent memoRef f $ Event mkEvent)

initBehavior :: Behavior a -> PageBuilder String
initBehavior (Behavior var) = pure var
initBehavior (Current ra) = do
  reactiveKey <- initReactive ra
  ReactiveInfo _ _ mkBehavior <- gets $ (DMap.! reactiveKey) . pbs_reactives
  mkBehavior
initBehavior (FmapBehavior f ba) = do
  parentName <- initBehavior ba
  name <- ("behavior_" <>) <$> freshId
  (fJs, value) <- exprToJavascript Ctx.Nil (quotedCode f)
  appendPostScript
    . pure
    $ Js ["const " <> name <> " = () => {"]
    <> Js.indent
      2
      ( fJs
          <> Js ["return " <> value <> "(" <> parentName <> "());"]
      )
    <> Js ["}"]
  pure name
initBehavior (PureBehavior a) = do
  name <- ("behavior_" <>) <$> freshId
  (aJs, aValue) <- exprToJavascript Ctx.Nil (quotedCode a)
  appendPostScript . pure $ aJs <> Js ["const " <> name <> " = () => " <> aValue <> ";"]
  pure name
initBehavior (ApBehavior bf ba) = do
  fName <- initBehavior bf
  aName <- initBehavior ba
  name <- ("behavior_" <>) <$> freshId
  appendPostScript . pure $ Js ["const " <> name <> " = () => " <> fName <> "()(" <> aName <> "());"]
  pure name