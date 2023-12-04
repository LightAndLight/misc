{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Lib (
  App,
  Path,
  serve,
  page,
  pageM,
  Interact,
  Event,
  Behavior,
  Reactive,
  Element,
  html,
  element,
  textInput,
  domEvent,
  sample,
  current,
  stepper,
  stepperM,
  perform,
  request,
  toString,
  Expr.isEmpty,
  Html (..),
  href,
  DomEvent (..),
  module Network.HTTP.Types.Method,
) where

import Compiler.Plugin.Interface (Product (..), Quote' (..), Quoted (..), Sum (..), quote, toString)
import qualified Compiler.Plugin.Interface as Expr
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Class (MonadReader, asks, local)
import Control.Monad.State.Class (MonadState, gets, modify)
import Control.Monad.State.Lazy (StateT, evalStateT)
import Control.Monad.Trans
import Control.Monad.Writer.Lazy (WriterT, runWriterT)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Json
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Fix (Fix)
import Data.Foldable (fold)
import Data.Functor.Classes (Show1)
import Data.Functor.Const (Const (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any (..))
import Data.String (IsString (..))
import qualified Data.Text
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import Data.Traversable (for)
import qualified Data.Tuple as Tuple
import Data.Type.Equality ((:~:) (..))
import Data.Unique.Tag (RealWorld, Tag, newTag)
import GHC.Generics (Generic)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (badRequest400, notFound404, ok200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn)
import qualified System.IO
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

newtype Path = Path [String]
  deriving (Eq, Ord, Semigroup, Monoid, Show)

href :: Path -> (Data.Text.Text, Data.Text.Text)
href (Path segments) = ("href", Data.Text.pack $ intercalate "/" segments)

instance IsString Path where
  fromString = Path . pure

data Interact :: Type -> Type where
  Pure :: a -> Interact a
  Apply :: Interact (a -> b) -> Interact a -> Interact b
  Bind :: Interact a -> (a -> Interact b) -> Interact b
  TextInput :: Interact (Behavior Data.Text.Text, Element)
  Element :: Html -> Interact Element
  DomEvent :: DomEvent -> Element -> Interact (Event ())
  Perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
  Request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
  Stepper :: (Send a) => a -> Event a -> Interact (Reactive a)
  StepperM :: (Send a) => IO a -> Event a -> Interact (Reactive a)
  MFix :: (a -> Interact a) -> Interact a

instance Functor Interact where
  fmap f m = Pure f `Apply` m

instance Applicative Interact where
  pure = Pure
  (<*>) = Apply

instance Monad Interact where
  (>>=) = Bind

instance MonadFix Interact where
  mfix = MFix

data Element = MkElement Data.Text.Text Html

html :: Element -> Html
html (MkElement _ a) = a

element :: Html -> Interact Element
element = Element

textInput :: Interact (Behavior Data.Text.Text, Element)
textInput = TextInput

newtype ReactiveKey a = ReactiveKey (Tag RealWorld a)
  deriving (GEq, GCompare)

data ReactiveInfo a where
  ReactiveInfo ::
    (Send a) =>
    { ri_initial :: a
    , ri_event :: PageBuilder EventKey
    , ri_behavior :: PageBuilder String
    } ->
    ReactiveInfo a

data PageBuilderEnv = PageBuilderEnv
  { pbe_writeQueueName :: String
  , pbe_mkNodeName :: String
  }

data PageBuilderState = PageBuilderState
  { pbs_supply :: Int
  , pbs_postScript :: Js
  , pbs_domEventSubscriptions :: Map (Data.Text.Text, DomEvent) (String -> Js)
  , pbs_rpcs :: Map ByteString RPC
  , pbs_reactives :: DMap ReactiveKey ReactiveInfo
  }

newtype PageBuilder a = PageBuilder {unPageBuilder :: ReaderT PageBuilderEnv (StateT PageBuilderState IO) a}
  deriving (Functor, Applicative, Monad, MonadState PageBuilderState, MonadReader PageBuilderEnv, MonadFix, MonadIO)

instance HasSupply PageBuilderState where
  getSupply = pbs_supply
  setSupply n s = s{pbs_supply = n}

runPageBuilder :: PageBuilder a -> IO a
runPageBuilder ma =
  evalStateT
    ( flip
        runReaderT
        PageBuilderEnv
          { pbe_writeQueueName = "/* unset */"
          , pbe_mkNodeName = "/* unset */"
          }
        $ do
          writeQueueName <- ("writeQueue_" <>) <$> freshId
          mkNodeName <- ("mkNode_" <>) <$> freshId

          local (\e -> e{pbe_writeQueueName = writeQueueName, pbe_mkNodeName = mkNodeName}) . unPageBuilder $ do
            appendPostScript $ Js ["var " <> writeQueueName <> " = [];"]
            appendPostScript
              $ Js
                [ "function " <> mkNodeName <> "(node) {"
                , "}"
                ]
            ma
    )
    PageBuilderState
      { pbs_supply = 0
      , pbs_postScript = mempty
      , pbs_domEventSubscriptions = mempty
      , pbs_rpcs = mempty
      , pbs_reactives = mempty
      }

appendPostScript :: Js -> PageBuilder ()
appendPostScript js = modify $ \s -> s{pbs_postScript = pbs_postScript s <> js}

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

queueAction :: String -> Js -> Js
queueAction queueName action =
  Js [queueName <> ".push(() => {"]
    <> indent 2 action
    <> Js ["});"]

reactiveInitBehavior :: ReactiveKey a -> PageBuilder String
reactiveInitBehavior reactiveKey = do
  ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives

  b <- newBehavior (ByteString.Lazy.Char8.unpack $ encodeSend initial)
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
  subscribe (Event $ pure eventKey) $ \value -> queueAction writeQueueName (Js [b <> " = " <> value <> ";"])

  pure b

data EventKey
  = EventKey_Derived String
  | EventKey_DomEvent Data.Text.Text DomEvent

newtype MemoRef a = MemoRef (IORef (MemoMaybe a))

data MemoMaybe a where
  MemoNothing :: x -> MemoMaybe a
  MemoJust :: a -> MemoMaybe a

{-# NOINLINE unsafeNewMemoRef #-}
unsafeNewMemoRef ::
  -- | This argument isn't used by the function; it's here to stop what would
  -- be @unsafePerformIO newMemoRef@ getting let-floated.
  a ->
  MemoRef EventKey
unsafeNewMemoRef a = MemoRef (unsafePerformIO $ newIORef (MemoNothing a))

{- | Do not use @unsafePerformIO newMemoRef@.
It will create a single IORef in global scope due to let-floating.
Use @unsafeNewMemoRef@ instead.
-}
newMemoRef :: IO (MemoRef EventKey)
newMemoRef = MemoRef <$> newIORef (MemoNothing undefined)

readMemoRef :: MemoRef a -> IO (Maybe a)
readMemoRef (MemoRef ref) = do
  val <- readIORef ref
  case val of
    MemoNothing _ ->
      pure Nothing
    MemoJust a ->
      pure $ Just a

-- | Don't call this twice.
setMemoRef :: MemoRef a -> a -> IO ()
setMemoRef (MemoRef ref) a = writeIORef ref (MemoJust a)

data Event :: Type -> Type where
  FromDomEvent :: Data.Text.Text -> DomEvent -> Event ()
  Event :: PageBuilder EventKey -> Event a
  FmapEvent :: MemoRef EventKey -> Quoted (a -> b) -> Event a -> Event b

instance Functor Event where
  {-# INLINE fmap #-}
  fmap f = let !memoRef = unsafeNewMemoRef f in FmapEvent memoRef (quote f)

data Behavior a where
  Behavior :: String -> Behavior a
  Current :: (Send a) => Reactive a -> Behavior a

domEvent :: DomEvent -> Element -> Event ()
domEvent de (MkElement elId _) = FromDomEvent elId de

subscribe :: Event a -> (String -> Js) -> PageBuilder ()
subscribe ea callback = do
  key <- initEvent ea
  case key of
    EventKey_Derived name -> do
      arg <- ("arg_" <>) <$> freshId
      appendPostScript $ Js [name <> ".subscribers.push((" <> arg <> ") => {"] <> indent 2 (callback arg) <> Js ["});"]
    EventKey_DomEvent elId de -> do
      subscribeDomEvent elId de callback

subscribeDomEvent :: Data.Text.Text -> DomEvent -> (String -> Js) -> PageBuilder ()
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
        <> indent 2 (Js [f <> "(" <> value <> ");"])
        <> Js ["}"]

newEvent :: PageBuilder String
newEvent = do
  name <- ("event_" <>) <$> freshId
  appendPostScript $ Js ["const " <> name <> " = { subscribers: [] };"]
  pure name

newBehavior :: String -> PageBuilder String
newBehavior initial = do
  name <- ("behavior_" <>) <$> freshId
  appendPostScript $ Js ["var " <> name <> " = " <> initial]
  pure name

{- |
For example, in this code

@
do
  rec r <- stepper initial $ sample e (current r)
  let r' = fmap f r
  ...
@

there's only one call to @sample e (current r)@. But without memoization,
two @sample e (current r)@ events will be compiled: one for the event that
updates to @current r@, and one for the event behind @fmap f r@. As a result,
@e@ will have two subscribers instead of one. This is okay semantically, but
it increases code size and duplicates work.

Functions that call 'memoEvent' should be marked as @NOINLINE@, so that each
call-site is assigned a single memoized event.
-}
memoEvent :: (String -> PageBuilder ()) -> Event a
memoEvent f = let !memoRef = unsafeNewMemoRef f in Event $ memoEventWith memoRef f

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

{-# NOINLINE sample #-}
sample :: Event a -> Behavior b -> Event (a, b)
sample ea bb =
  memoEvent $ \name -> do
    b <- initBehavior bb
    temp <- ("temp_" <>) <$> freshId
    notifyJs <- notify name
    subscribe ea $ \value ->
      Js ["const " <> temp <> " = { fst: " <> value <> ", snd: " <> b <> " };"]
        <> notifyJs temp

current :: (Send a) => Reactive a -> Behavior a
current = Current

initReactive :: (Send a) => Reactive a -> PageBuilder (ReactiveKey a)
initReactive (Reactive key) = pure key
initReactive (FmapReactive f (Reactive key)) = do
  ReactiveInfo initial mkEvent _ <- gets $ (DMap.! key) . pbs_reactives
  memoRef <- liftIO newMemoRef
  mkReactive (Expr.quotedValue f initial) (FmapEvent memoRef f $ Event mkEvent)
initReactive (FmapReactive f' r'@FmapReactive{}) = go f' r'
 where
  go :: (Send x) => Quoted (a -> x) -> Reactive a -> PageBuilder (ReactiveKey x)
  go f (FmapReactive g r) = go (f `Expr.compose` g) r
  go f (Reactive key) = do
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! key) . pbs_reactives
    memoRef <- liftIO newMemoRef
    mkReactive (Expr.quotedValue f initial) (FmapEvent memoRef f $ Event mkEvent)

mkReactive :: (Send a) => a -> Event a -> PageBuilder (ReactiveKey a)
mkReactive initial eUpdate = do
  key <- liftIO $ ReactiveKey <$> newTag
  modify $ \s ->
    s
      { pbs_reactives =
          DMap.insert
            key
            (ReactiveInfo initial (reactiveInitEvent key eUpdate) (reactiveInitBehavior key))
            (pbs_reactives s)
      }
  pure key

initBehavior :: Behavior a -> PageBuilder String
initBehavior (Behavior var) = pure var
initBehavior (Current ra) = do
  reactiveKey <- initReactive ra
  ReactiveInfo _ _ mkBehavior <- gets $ (DMap.! reactiveKey) . pbs_reactives
  mkBehavior

initEvent :: Event a -> PageBuilder EventKey
initEvent e =
  case e of
    Event mkEvent ->
      mkEvent
    FmapEvent memoRef (Quoted fExpr _) e' ->
      memoEventWith memoRef $ \name -> do
        fJs <- exprToJavascript Expr.Nil fExpr
        temp <- ("temp_" <>) <$> freshId
        notifyJs <- notify name
        subscribe e' $ \value ->
          Tuple.fst fJs
            <> Js ["const " <> temp <> " = " <> Tuple.snd fJs <> "(" <> value <> ");"]
            <> notifyJs temp
    FromDomEvent elId de ->
      pure $ EventKey_DomEvent elId de

newtype Addr a = Addr Int
  deriving (Show, Eq, Ord)

instance GEq Addr where
  geq (Addr a) (Addr b) = if a == b then Just (unsafeCoerce Refl) else Nothing

instance GCompare Addr where
  gcompare (Addr a) (Addr b) =
    case compare a b of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

data Reactive a where
  FmapReactive :: Quoted (a -> b) -> Reactive a -> Reactive b
  Reactive :: ReactiveKey a -> Reactive a

instance Functor Reactive where
  {-# INLINE fmap #-}
  fmap f = FmapReactive (quote f)

stepper :: (Send a) => a -> Event a -> Interact (Reactive a)
stepper = Stepper

stepperM :: (Send a) => IO a -> Event a -> Interact (Reactive a)
stepperM = StepperM

class (FromJSON (SendTy a), ToJSON (SendTy a)) => Send a where
  type SendTy a :: Type
  fromSendTy :: SendTy a -> a
  toSendTy :: a -> SendTy a

instance Send () where
  type SendTy () = ()
  fromSendTy = id
  toSendTy = id

instance Send Int where
  type SendTy Int = Int
  fromSendTy = id
  toSendTy = id

instance Send Data.Text.Text where
  type SendTy Data.Text.Text = Data.Text.Text
  fromSendTy = id
  toSendTy = id

instance (Send a, Send b) => Send (a, b) where
  type SendTy (a, b) = SendPair (SendTy a) (SendTy b)
  fromSendTy (SendPair a b) = (fromSendTy a, fromSendTy b)
  toSendTy (a, b) = SendPair (toSendTy a) (toSendTy b)

instance (Send a) => Send [a] where
  type SendTy [a] = [SendTy a]
  fromSendTy = fmap fromSendTy
  toSendTy = fmap toSendTy

data SendPair a b = SendPair {fst :: a, snd :: b}
  deriving (Generic)
instance (FromJSON a, FromJSON b) => FromJSON (SendPair a b)
instance (ToJSON a, ToJSON b) => ToJSON (SendPair a b)

newtype SendHtml = SendHtml Html

instance Send Html where
  type SendTy Html = SendHtml
  fromSendTy (SendHtml h) = h
  toSendTy = SendHtml

instance ToJSON SendHtml where
  toJSON (SendHtml h) =
    case h of
      Node name attrs children ->
        Json.object
          [ "tag" Json..= ("Node" :: Data.Text.Text)
          , "arg"
              Json..= [ Json.toJSON name
                      , Json.toJSON attrs
                      , Json.toJSON $ fmap toSendTy children
                      ]
          ]
      _ ->
        error "TODO: toEncoding for SendHtml"

instance FromJSON SendHtml where
  parseJSON = Json.withObject "SendHtml" $ \obj -> do
    tag :: Data.Text.Text <- obj Json..: "tag"
    case tag of
      "Node" -> do
        arg :: [Json.Value] <- obj Json..: "arg"
        fmap SendHtml
          $ Node
          <$> Json.parseJSON (arg !! 0)
          <*> Json.parseJSON (arg !! 1)
          <*> fmap fromSendTy (Json.parseJSON (arg !! 2))
      _ -> fail $ "invalid tag: " <> Data.Text.unpack tag

decodeSend :: (Send a) => Lazy.ByteString -> Either String a
decodeSend = fmap fromSendTy . Json.eitherDecode'

encodeSend :: (Send a) => a -> Lazy.ByteString
encodeSend = Json.encode . toSendTy

perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
perform = Perform

request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
request = Request

data Page
  = Page Html
  | PageM (Interact Html)

newtype RPC = RPC (forall a. Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a)

renderPage :: Path -> Page -> PageBuilder Builder
renderPage path (Page h) = renderHtml path h
renderPage path (PageM h) = renderInteractHtml path h

class HasSupply s where
  getSupply :: s -> Int
  setSupply :: Int -> s -> s

freshId :: (MonadState s m, HasSupply s) => m String
freshId = do
  n <- gets getSupply
  modify $ setSupply (n + 1)
  pure $ show n

setId :: Data.Text.Text -> Html -> Html
setId i (Node name attrs children) = Node name (("id", i) : attrs) children
setId _ a = a

newtype Js = Js {getJs :: [String]}
  deriving (Show, Semigroup, Monoid)

indent :: Int -> Js -> Js
indent n (Js ls) = Js (fmap (replicate n ' ' <>) ls)

renderInteractHtml :: Path -> Interact Html -> PageBuilder Builder
renderInteractHtml path x = do
  h <- go x
  renderHtml path h
 where
  go :: Interact a -> PageBuilder a
  go (Pure h) = pure h
  go (Apply mf ma) = go mf <*> go ma
  go (Bind ma f) = go ma >>= go . f
  go TextInput = do
    elId <- ("element_" <>) <$> freshId
    behaviorId <- ("behavior_" <>) <$> freshId
    pure
      ( Behavior behaviorId
      , MkElement (fromString elId)
          $ Node "input" [("id", fromString elId), ("type", "text")] []
          `WithScript` unlines
            [ "const " <> elId <> " = document.getElementById(\"" <> elId <> "\");"
            , "var " <> behaviorId <> " = \"\";"
            , elId <> ".addEventListener("
            , "  \"change\","
            , "  (event) => {"
            , "    " <> behaviorId <> " = " <> elId <> ".value;"
            , "  }"
            , ");"
            ]
      )
  go (Element h) = do
    elId <- ("element_" <>) <$> freshId
    pure
      . MkElement (fromString elId)
      $ setId (fromString elId) h
      `WithScript` ("const " <> elId <> " = document.getElementById(\"" <> elId <> "\");")
  go (DomEvent de (MkElement elId _)) =
    pure $ FromDomEvent elId de
  go (Perform ea f) = do
    let
      run :: Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
      run input resp =
        case decodeSend input of
          Left err -> do
            hPutStrLn System.IO.stderr $ show err
            undefined
          Right a -> do
            b <- f a
            resp (encodeSend b)

    fnId <- ("function_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert (ByteString.Char8.pack fnId) (RPC run) (pbs_rpcs s)}

    subscribe ea $ \value -> Js (fetch path fnId value)
  go (Request ea f) = do
    let
      run :: Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
      run input resp =
        case decodeSend input of
          Left err -> do
            hPutStrLn System.IO.stderr $ show err
            undefined
          Right a -> do
            b <- f a
            resp (encodeSend b)

    fnId <- ("function_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert (ByteString.Char8.pack fnId) (RPC run) (pbs_rpcs s)}

    name <- newEvent
    response <- ("response_" <>) <$> freshId
    arg <- ("arg_" <>) <$> freshId
    notifyJs <- notify name
    subscribe ea $ \value ->
      Js
        (fetch path fnId value)
        <> Js [".then((" <> response <> ") => {"]
        <> indent
          2
          ( Js [response <> ".json().then((" <> arg <> ") => {"]
              <> indent 2 (notifyJs arg)
              <> Js ["});"]
          )
        <> Js ["});"]

    pure $ Event (pure $ EventKey_Derived name)
  go (StepperM getInitial eUpdate) = do
    initial <- liftIO getInitial
    Reactive <$> mkReactive initial eUpdate
  go (Stepper initial eUpdate) = do
    Reactive <$> mkReactive initial eUpdate
  go (MFix f) = mfix (go . f)

exprToJavascript :: (MonadState s m, HasSupply s) => Expr.Ctx (Const String) ctx -> Expr.Expr ctx a -> m (Js, String)
exprToJavascript = go id
 where
  go :: (MonadState s m, HasSupply s) => (forall x. Expr.Index ctx' x -> Expr.Index ctx x) -> Expr.Ctx (Const String) ctx -> Expr.Expr ctx' a -> m (Js, String)
  go weaken ctx expr =
    case expr of
      Expr.Var v -> do
        let Const v' = Expr.getCtx (weaken v) ctx
        pure (mempty, v')
      Expr.Lam (body :: Expr.Expr (a ': ctx) b) -> do
        arg <- ("arg_" <>) <$> freshId
        (ls, body') <- go (\case Expr.Z -> Expr.Z; Expr.S ix -> Expr.S (weaken ix)) (Expr.Cons (Const arg :: Const String a) ctx) body
        temp <- ("temp_" <>) <$> freshId
        pure
          ( Js ["const " <> temp <> " = (" <> arg <> ") => {"]
              <> indent 2 (ls <> Js ["return " <> body' <> ";"])
              <> Js ["};"]
          , temp
          )
      Expr.App f x -> do
        (ls, f') <- go weaken ctx f
        (ls', x') <- go weaken ctx x
        pure (ls <> ls', f' <> "(" <> x' <> ")")
      Expr.Int i -> pure (mempty, show i)
      Expr.Add a b -> do
        (ls, a') <- go weaken ctx a
        (ls', b') <- go weaken ctx b
        pure (ls <> ls', "(" <> a' <> " + " <> b' <> ")")
      Expr.Bool b ->
        if b then pure (mempty, "true") else pure (mempty, "false")
      Expr.IfThenElse cond t e -> do
        (ls, cond') <- go weaken ctx cond
        (ls', t') <- go weaken ctx t
        (ls'', e') <- go weaken ctx e
        pure (ls <> ls' <> ls'', "(" <> cond' <> " ? " <> t' <> " : " <> e' <> ")")
      Expr.Lt a b -> do
        (ls, a') <- go weaken ctx a
        (ls', b') <- go weaken ctx b
        pure (ls <> ls', "(" <> a' <> " < " <> b' <> ")")
      Expr.Case a branches -> do
        value <- ("value_" <>) <$> freshId
        (ls, a') <- go weaken ctx a
        result <- ("result_" <>) <$> freshId
        (branches', Any tagged) <- runWriterT $ traverse (branchToJavascript value result weaken ctx) branches
        pure
          ( ls
              <> Js ["const " <> value <> " = " <> a' <> ";"]
              <> Js ["var " <> result <> ";"]
              <> Js ["switch (" <> value <> (if tagged then ".tag" else "") <> ") {"]
              <> indent 2 (fold branches')
              <> Js ["}"]
          , result
          )
       where
        branchToJavascript ::
          (MonadState s m, HasSupply s) =>
          String ->
          String ->
          (forall x. Expr.Index ctx' x -> Expr.Index ctx x) ->
          Expr.Ctx (Const String) ctx ->
          Expr.Branch ctx' a b ->
          WriterT Any m Js
        branchToJavascript value result weaken ctx (Expr.Branch pattern body) =
          case pattern of
            Expr.PDefault -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["default:"]
                <> indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PInt i -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["case " <> show i <> ":"]
                <> indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PUnit -> do
              (ls, body') <- lift $ go weaken ctx body
              pure
                $ Js ["default:"]
                <> indent
                  2
                  ( ls
                      <> Js
                        [ result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
            Expr.PPair @ctx @a @b -> do
              (ls, body') <-
                lift
                  $ go
                    ( \case
                        Expr.Z -> Expr.Z
                        Expr.S n ->
                          Expr.S $ case n of
                            Expr.Z -> Expr.Z
                            Expr.S n' -> Expr.S (weaken n')
                    )
                    (Expr.Cons (Const (value <> ".snd") :: Const String b) $ Expr.Cons (Const (value <> ".fst") :: Const String a) ctx)
                    body
              pure
                $ Js ["default:"]
                <> indent
                  2
                  ( ls
                      <> Js
                        [ "" <> result <> " = " <> body' <> ";"
                        , "break;"
                        ]
                  )
      Expr.Char c -> do
        pure (mempty, show c)
      Expr.ToString -> do
        pure (mempty, "JSON.stringify")
      Expr.Append -> do
        pure (mempty, "((a, b) => [...a, ...b])")
      Expr.Weaken x -> go (weaken . Expr.S) ctx x

data Html
  = Html [Html]
  | Node Data.Text.Text [(Data.Text.Text, Data.Text.Text)] [Html]
  | WithScript Html String
  | Text Data.Text.Text
  | ReactiveText (Reactive Data.Text.Text)
  | ReactiveHtml (Reactive Html)
  | OnEvent Html (DomEvent, IO ())

instance Quote' Data.Text.Text where
  type QuoteTy' Data.Text.Text = Data.Text.Text
  {-# INLINE quote' #-}
  quote' = Expr.String

instance (Quote' a) => Quote' [a] where
  type QuoteTy' [a] = [QuoteTy' a]
  {-# INLINE quote' #-}
  quote' = Expr.List . fmap quote'

instance (Quote' a, Quote' b) => Quote' (a, b) where
  type QuoteTy' (a, b) = Product '[ '("fst", Expr.Expr '[] (QuoteTy' a)), '("snd", Expr.Expr '[] (QuoteTy' b))]
  {-# INLINE quote' #-}
  quote' (a, b) =
    Expr.Product
      ( Product_Cons @"fst" (quote' a)
          $ Product_Cons @"snd" (quote' b)
          $ Product_Nil
      )

newtype QuotedHtml ctx
  = QuotedHtml
      ( Expr.Expr
          ctx
          ( Sum
              '[ '("Html", QuoteTy' [Html])
               , '( "Node"
                  , Product
                      '[ '("name", QuoteTy' Data.Text.Text)
                       , '("attrs", QuoteTy' [(Data.Text.Text, Data.Text.Text)])
                       , '("children", QuoteTy' [Html])
                       ]
                  )
               ]
          )
      )
  deriving (Show)

instance Quote' Html where
  type QuoteTy' Html = QuotedHtml
  {-# INLINE quote' #-}
  quote' (Html children) =
    Expr.Coerced (QuotedHtml $ Sum_Z @"Html" $ quote' children)
  quote' (Node name attrs children) =
    Expr.Coerced
      ( QuotedHtml
          . Sum_S
          . Sum_Z
          . Expr.Product
          $ Product_Cons (quote' name)
          $ Product_Cons (quote' attrs)
          $ Product_Cons (quote' children)
          $ Product_Nil
      )

instance Quote' (a -> b) where
  type QuoteTy' (a -> b) = (a -> b)
  {-# INLINE quote' #-}
  quote' f = quotedCode (quote f)

data DomEvent
  = Click
  | Change
  deriving (Eq, Ord)

renderDomEvent :: (IsString s) => DomEvent -> s
renderDomEvent Click = "\"click\""
renderDomEvent Change = "\"change\""

renderPath :: (IsString s, Monoid s) => Path -> s
renderPath (Path segments) = go segments
 where
  go [] = mempty
  go [b] = fromString b
  go (b : bs) =
    fromString b <> "/" <> go bs

fetch :: (IsString s, Monoid s) => Path -> s -> s -> [s]
fetch path fnId a =
  [ "fetch("
  , "  \"" <> renderPath path <> "\","
  , "  { method: \"POST\", body: JSON.stringify({ fn: \"" <> fnId <> "\", arg: " <> a <> " })}"
  , ")"
  ]

flushQueue :: String -> PageBuilder Js
flushQueue queueName = do
  temp <- ("temp_" <>) <$> freshId
  pure
    $ Js ["while (" <> queueName <> ".length > 0) {"]
    <> indent
      2
      ( Js
          [ "const " <> temp <> " = " <> queueName <> ".shift();"
          , temp <> "();"
          ]
      )
    <> Js ["}"]

renderHtml :: Path -> Html -> PageBuilder Builder
renderHtml path h = do
  go Nothing h
 where
  go :: Maybe String -> Html -> PageBuilder Builder
  go _ (Html children) = do
    writeQueueName <- asks pbe_writeQueueName
    children' <- fold <$> traverse (go Nothing) children
    postScript <- gets pbs_postScript
    domEventSubscriptions <- gets pbs_domEventSubscriptions
    flushQueueJs <- flushQueue writeQueueName
    domEventSubscriptionsJs <- fmap fold . for (Map.toList domEventSubscriptions) $ \((elId, de), callback) -> do
      arg <- ("arg_" <>) <$> freshId
      pure
        $ Js
          [Data.Text.unpack elId <> ".addEventListener("]
        <> indent
          2
          ( Js
              [ renderDomEvent de <> ","
              , "(" <> arg <> ") => {"
              ]
              <> indent
                2
                ( callback arg
                    <> flushQueueJs
                )
              <> Js ["}"]
          )
        <> Js [");"]
    pure
      $ "<!doctype html>\n"
      <> "<html>\n"
      <> children'
      <> "<script>\n"
      <> Builder.byteString
        ( ByteString.Char8.pack
            . foldMap (<> "\n")
            $ getJs (postScript <> domEventSubscriptionsJs)
        )
      <> "</script>"
      <> "</html>\n"
  go mId (Node name attrs children) = do
    let nameBytes = Data.Text.Encoding.encodeUtf8 name
    children' <- fold <$> traverse (go Nothing) children
    pure
      $ "<"
      <> Builder.byteString nameBytes
      <> foldMap
        (\(attrName, attrValue) -> " " <> attrName <> "=\"" <> attrValue <> "\"")
        ( maybe [] (pure . (,) "id" . fromString) mId
            <> fmap
              ( bimap
                  (Builder.byteString . Data.Text.Encoding.encodeUtf8)
                  (Builder.byteString . Data.Text.Encoding.encodeUtf8)
              )
              attrs
        )
      <> ">"
      <> (case children of [] -> ""; [Text{}] -> ""; _ -> "\n")
      <> children'
      <> "</"
      <> Builder.byteString nameBytes
      <> ">\n"
  go _ (Text t) =
    -- TODO: escape text
    pure $ Builder.byteString (Data.Text.Encoding.encodeUtf8 t)
  go mId (OnEvent element (event, action)) = do
    elId <- maybe (("element_" <>) <$> freshId) pure mId
    fnId <- ByteString.Char8.pack . ("function_" <>) <$> freshId
    arg <- ("arg_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert fnId (RPC $ \_ resp -> action *> resp "") (pbs_rpcs s)}
    element' <- go (Just elId) element
    pure
      . (element' <>)
      . foldMap (<> "\n")
      $ [ "<script>"
        , "const " <> fromString elId <> " = document.getElementById(\"" <> fromString elId <> "\");"
        , fromString elId <> ".addEventListener("
        , "  " <> renderDomEvent event <> ", "
        , "  (" <> fromString arg <> ") => { "
        ]
      <> fmap ("  " <>) (fetch path (Builder.byteString fnId) "{}")
      <> [ "  }"
         , ");"
         , "</script>"
         ]
  go mId (WithScript el script) = do
    el' <- go mId el
    pure $ el' <> "<script>\n" <> Builder.byteString (ByteString.Char8.pack script) <> "</script>\n"
  go _mId (ReactiveText ra) = do
    elId <- ("element_" <>) <$> freshId
    reactiveKey <- initReactive ra
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives
    event <- mkEvent
    subscribe (Event $ pure event) $ \value -> Js [elId <> ".textContent = " <> value <> ";"]
    pure $ "<span id=\"" <> fromString elId <> "\">" <> Builder.byteString (Data.Text.Encoding.encodeUtf8 initial) <> "</span>"
  go _mId (ReactiveHtml rhtml) = do
    elId <- ("element_" <>) <$> freshId
    reactiveKey <- initReactive rhtml
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives
    event <- mkEvent
    var <- ("var_" <>) <$> freshId
    mkNodeJs <- asks pbe_mkNodeName
    subscribe (Event $ pure event) $ \value ->
      Js
        [ "const " <> var <> " = document.getElementById(\"" <> elId <> "\");"
        , var <> ".replaceWith(" <> mkNodeJs <> "(" <> value <> "));"
        ]
    go (Just elId) initial

-- | [[ App ]] = Path -> Maybe Page
newtype App = App (Map Path Page)
  deriving (Semigroup, Monoid)

page :: Path -> Html -> App
page p v = App (Map.singleton p $ Page v)

pageM :: Path -> Interact Html -> App
pageM p v = App (Map.singleton p $ PageM v)

data ActionRequest = ActionRequest {fn :: String, arg :: Json.Value}
  deriving (Generic)

instance FromJSON ActionRequest

compile :: App -> IO Wai.Application
compile (App paths) = do
  actionsAndPages <-
    Map.traverseWithKey
      ( \path p -> do
          (actions, content) <- runPageBuilder $ do
            p' <- renderPage ("" <> path) p
            rpcs <- gets pbs_rpcs
            pure (rpcs, p')
          pure
            $ if Map.null actions
              then Map.singleton methodGet (Right content)
              else Map.insert methodGet (Right content) $ Map.singleton methodPost (Left actions)
      )
      paths
  pure $ \request respond -> do
    let
      requestPath :: Path
      requestPath = Path . fmap Text.unpack $ Wai.pathInfo request

    case Map.lookup requestPath actionsAndPages of
      Nothing ->
        respond $ Wai.responseLBS notFound404 [] "not found"
      Just methods ->
        let method = Wai.requestMethod request
         in case Map.lookup method methods of
              Nothing ->
                respond . Wai.responseLBS badRequest400 [] $ "invalid method: " <> ByteString.Lazy.fromStrict method
              Just actionOrPage ->
                case actionOrPage of
                  Left pathActions -> do
                    result <- decodeJsonBody @ActionRequest request
                    case result of
                      Left err -> do
                        hPutStrLn System.IO.stderr $ show err
                        respond $ Wai.responseBuilder badRequest400 [] "invalid action request"
                      Right (ActionRequest fn' arg) -> do
                        case Map.lookup (ByteString.Char8.pack fn') pathActions of
                          Nothing ->
                            respond $ Wai.responseLBS badRequest400 [] "invalid action request"
                          Just (RPC action) -> do
                            action (Json.encode arg) (respond . Wai.responseBuilder ok200 [] . Builder.lazyByteString)
                  Right contents ->
                    respond $ Wai.responseBuilder ok200 [] contents
 where
  decodeJsonBody :: (FromJSON a) => Wai.Request -> IO (Either String a)
  decodeJsonBody request = go mempty
   where
    go acc = do
      bs <- Wai.getRequestBodyChunk request
      if ByteString.null bs
        then pure . Json.eitherDecode' $ Builder.toLazyByteString acc
        else go (acc <> Builder.byteString bs)

serve :: App -> IO ()
serve app = do
  let port = 8000
  putStrLn $ "app running on port " <> show port
  Warp.run port =<< compile app