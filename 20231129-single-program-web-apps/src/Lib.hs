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
  Html (..),
  href,
  DomEvent (..),
  module Network.HTTP.Types.Method,
) where

import Compiler.Plugin.Interface (Quoted (..), quote, toString)
import qualified Compiler.Plugin.Interface as Expr
import Control.Monad.Fix (MonadFix (..))
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
import Data.Foldable (fold)
import Data.Functor (void)
import Data.Functor.Const (Const (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any (..))
import Data.String (IsString (..))
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Type.Equality ((:~:) (..))
import Data.Unique.Tag (RealWorld, Tag, newTag)
import GHC.Generics (Generic)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (badRequest400, notFound404, ok200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn)
import qualified System.IO
import Unsafe.Coerce (unsafeCoerce)

newtype Path = Path [String]
  deriving (Eq, Ord, Semigroup, Monoid, Show)

href :: Path -> (String, String)
href (Path segments) = ("href", intercalate "/" segments)

instance IsString Path where
  fromString = Path . pure

data Interact :: Type -> Type where
  Pure :: a -> Interact a
  Apply :: Interact (a -> b) -> Interact a -> Interact b
  Bind :: Interact a -> (a -> Interact b) -> Interact b
  TextInput :: Interact (Behavior String, Element)
  Element :: Html -> Interact Element
  DomEvent :: DomEvent -> Element -> Interact (Event ())
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

data Element = MkElement String Html

html :: Element -> Html
html (MkElement _ a) = a

element :: Html -> Interact Element
element = Element

textInput :: Interact (Behavior String, Element)
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

data PageBuilderState = PageBuilderState
  { pbs_supply :: Int
  , pbs_postScript :: Js
  , pbs_domEventSubscriptions :: Map (String, DomEvent) (String -> Js)
  , pbs_rpcs :: Map ByteString RPC
  , pbs_reactives :: DMap ReactiveKey ReactiveInfo
  }

newtype PageBuilder a = PageBuilder (StateT PageBuilderState IO a)
  deriving (Functor, Applicative, Monad, MonadState PageBuilderState, MonadFix, MonadIO)

instance HasSupply PageBuilderState where
  getSupply = pbs_supply
  setSupply n s = s{pbs_supply = n}

runPageBuilder :: PageBuilder a -> IO a
runPageBuilder (PageBuilder ma) =
  evalStateT
    ma
    PageBuilderState
      { pbs_supply = 0
      , pbs_postScript = mempty
      , pbs_domEventSubscriptions = mempty
      , pbs_rpcs = mempty
      , pbs_reactives = mempty
      }

appendPostScript :: Js -> PageBuilder ()
appendPostScript js = modify $ \s -> s{pbs_postScript = pbs_postScript s <> js}

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
  subscribe (Event $ pure eventKey) $ \value -> Js [b <> " = " <> value <> ";"]

  pure b

data EventKey
  = EventKey_Derived String
  | EventKey_DomEvent String DomEvent

data Event :: Type -> Type where
  FromDomEvent :: String -> DomEvent -> Event ()
  Event :: PageBuilder EventKey -> Event a
  FmapEvent :: Quoted (a -> b) -> Event a -> Event b

instance Functor Event where
  {-# INLINE fmap #-}
  fmap f a = FmapEvent (quote f) a

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
      appendPostScript $ Js [name <> ".subscribers.push((" <> arg <> ") => {"] <> callback arg <> Js ["})"]
    EventKey_DomEvent elId de -> do
      subscribeDomEvent elId de callback

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
        , f <> "(" <> value <> ");"
        , "}"
        ]

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

sample :: Event a -> Behavior b -> Event (a, b)
sample ea bb =
  Event $ do
    name <- newEvent
    b <- initBehavior bb
    temp <- ("temp_" <>) <$> freshId
    notifyJs <- notify name
    subscribe ea $ \value ->
      Js ["const " <> temp <> " = { fst: " <> value <> ", snd: " <> b <> " };"]
        <> notifyJs temp
    pure $ EventKey_Derived name

current :: (Send a) => Reactive a -> Behavior a
current = Current

initReactive :: (Send a) => Reactive a -> PageBuilder (ReactiveKey a)
initReactive (Reactive key) = pure key
initReactive r' = go (Expr.Quoted (Expr.Lam $ Expr.Var Expr.Z) id) r'
 where
  go :: (Send x) => Quoted (a -> x) -> Reactive a -> PageBuilder (ReactiveKey x)
  go f (FmapReactive g r) = go (f `Expr.compose` g) r
  go f (Reactive key) = do
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! key) . pbs_reactives
    mkReactive (Expr.quotedValue f initial) (FmapEvent f $ Event mkEvent)

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
    FmapEvent (Quoted fExpr _) e' -> do
      name <- newEvent
      fJs <- exprToJavascript Expr.Nil fExpr
      temp <- ("temp_" <>) <$> freshId
      notifyJs <- notify name
      subscribe e' $ \value ->
        Js ["const " <> temp <> " = (" <> fJs <> ")(" <> value <> ");"]
          <> notifyJs temp
      pure $ EventKey_Derived name
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

instance Send String where
  type SendTy String = String
  fromSendTy = id
  toSendTy = id

instance (Send a, Send b) => Send (a, b) where
  type SendTy (a, b) = SendPair (SendTy a) (SendTy b)
  fromSendTy (SendPair a b) = (fromSendTy a, fromSendTy b)
  toSendTy (a, b) = SendPair (toSendTy a) (toSendTy b)

data SendPair a b = SendPair {fst :: a, snd :: b}
  deriving (Generic)
instance (FromJSON a, FromJSON b) => FromJSON (SendPair a b)
instance (ToJSON a, ToJSON b) => ToJSON (SendPair a b)

decodeSend :: (Send a) => Lazy.ByteString -> Either String a
decodeSend = fmap fromSendTy . Json.eitherDecode'

encodeSend :: (Send a) => a -> Lazy.ByteString
encodeSend = Json.encode . toSendTy

perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
perform e f = void $ request e (void . f)

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

setId :: String -> Html -> Html
setId i (Node name attrs children) = Node name (("id", i) : attrs) children
setId _ a = a

newtype Js = Js {getJs :: [String]}
  deriving (Semigroup, Monoid)

renderInteractHtml :: Path -> Interact Html -> PageBuilder Builder
renderInteractHtml path x = do
  h <- go x
  content <- renderHtml path h
  pure content
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
      , MkElement elId
          $ Node "input" [("id", elId), ("type", "text")] []
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
    pure $ MkElement elId $ setId elId h `WithScript` ("const " <> elId <> " = document.getElementById(\"" <> elId <> "\");")
  go (DomEvent de (MkElement elId _)) =
    pure $ FromDomEvent elId de
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
        <> Js
          [ ".then((" <> response <> ") => {"
          , response <> ".json().then((" <> arg <> ") => {"
          ]
        <> notifyJs arg
        <> Js
          [ "});"
          , "});"
          ]

    pure $ Event (pure $ EventKey_Derived name)
  go (StepperM getInitial eUpdate) = do
    initial <- liftIO getInitial
    Reactive <$> mkReactive initial eUpdate
  go (Stepper initial eUpdate) = do
    Reactive <$> mkReactive initial eUpdate
  go (MFix f) = mfix (go . f)

exprToJavascript :: (MonadState s m, HasSupply s) => Expr.Ctx (Const String) ctx -> Expr.Expr ctx a -> m String
exprToJavascript = go id
 where
  go :: (MonadState s m, HasSupply s) => (forall x. Expr.Index ctx' x -> Expr.Index ctx x) -> Expr.Ctx (Const String) ctx -> Expr.Expr ctx' a -> m String
  go weaken ctx expr =
    case expr of
      Expr.Var v -> do
        let Const v' = Expr.getCtx (weaken v) ctx
        pure v'
      Expr.Lam (body :: Expr.Expr (a ': ctx) b) -> do
        arg <- ("arg_" <>) <$> freshId
        body' <- go (\case Expr.Z -> Expr.Z; Expr.S ix -> Expr.S (weaken ix)) (Expr.Cons (Const arg :: Const String a) ctx) body
        pure $ "((" <> arg <> ") => " <> body' <> ")"
      Expr.App f x -> do
        f' <- go weaken ctx f
        x' <- go weaken ctx x
        pure $ f' <> "(" <> x' <> ")"
      Expr.Int i -> pure $ show i
      Expr.Add a b -> do
        a' <- go weaken ctx a
        b' <- go weaken ctx b
        pure $ "(" <> a' <> " + " <> b' <> ")"
      Expr.Bool b ->
        if b then pure "true" else pure "false"
      Expr.IfThenElse cond t e -> do
        cond' <- go weaken ctx cond
        t' <- go weaken ctx t
        e' <- go weaken ctx e
        pure $ "(" <> cond' <> " ? " <> t' <> " : " <> e' <> ")"
      Expr.Lt a b -> do
        a' <- go weaken ctx a
        b' <- go weaken ctx b
        pure $ "(" <> a' <> " < " <> b' <> ")"
      Expr.Case a branches -> do
        value <- ("value_" <>) <$> freshId
        a' <- go weaken ctx a
        result <- ("result_" <>) <$> freshId
        (branches', Any tagged) <- runWriterT $ traverse (branchToJavascript value result weaken ctx) branches
        pure
          . unlines
          $ [ "((" <> value <> (if tagged then ".tag" else "") <> ") => {"
            , "var " <> result <> ";"
            , "switch (" <> value <> ") {"
            ]
          <> branches'
          <> [ "};"
             , "return " <> result <> ";"
             ]
          <> ["})(" <> a' <> ")"]
       where
        branchToJavascript ::
          (MonadState s m, HasSupply s) =>
          String ->
          String ->
          (forall x. Expr.Index ctx' x -> Expr.Index ctx x) ->
          Expr.Ctx (Const String) ctx ->
          Expr.Branch ctx' a b ->
          WriterT Any m (String)
        branchToJavascript value result weaken ctx (Expr.Branch pattern body) =
          case pattern of
            Expr.PDefault -> do
              body' <- lift $ go weaken ctx body
              pure
                $ unlines
                  [ "default:"
                  , "  " <> result <> " = " <> body' <> ";"
                  , "  break;"
                  ]
            Expr.PInt i -> do
              body' <- lift $ go weaken ctx body
              pure
                $ unlines
                  [ "case " <> show i <> ":"
                  , "  " <> result <> " = " <> body' <> ";"
                  , "  break;"
                  ]
            Expr.PUnit -> do
              body' <- lift $ go weaken ctx body
              pure
                $ unlines
                  [ "default:"
                  , "  " <> result <> " = " <> body' <> ";"
                  , "  break;"
                  ]
            Expr.PPair @ctx @a @b -> do
              body' <-
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
                $ unlines
                  [ "default:"
                  , "  " <> result <> " = " <> body' <> ";"
                  , "  break;"
                  ]
      Expr.Char c -> do
        pure $ show c
      Expr.ToString -> do
        arg <- ("arg_" <>) <$> freshId
        pure $ "((" <> arg <> ") => JSON.stringify(" <> arg <> "))"
      Expr.Weaken x -> go (weaken . Expr.S) ctx x

data Html
  = Html [Html]
  | Node String [(String, String)] [Html]
  | WithScript Html String
  | Text String
  | ReactiveText (Reactive String)
  | OnEvent Html (DomEvent, IO ())

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
  [ "    fetch("
  , "      \"" <> renderPath path <> "\","
  , "      { method: \"POST\", body: JSON.stringify({ fn: \"" <> fnId <> "\", arg: " <> a <> " })}"
  , "    )"
  ]

renderHtml :: Path -> Html -> PageBuilder Builder
renderHtml path h = do
  go Nothing h
 where
  go :: Maybe String -> Html -> PageBuilder Builder
  go _ (Html children) = do
    children' <- fold <$> traverse (go Nothing) children
    postScript <- gets pbs_postScript
    domEventSubscriptions <- gets pbs_domEventSubscriptions
    domEventSubscriptionsJs <- fmap fold . for (Map.toList domEventSubscriptions) $ \((elId, de), callback) -> do
      arg <- ("arg_" <>) <$> freshId
      pure
        $ Js
          [ elId <> ".addEventListener("
          , renderDomEvent de <> ","
          , "(" <> arg <> ") => {"
          ]
        <> callback arg
        <> Js ["});"]
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
    let nameBytes = ByteString.Char8.pack name
    children' <- fold <$> traverse (go Nothing) children
    pure
      $ "<"
      <> Builder.byteString nameBytes
      <> foldMap
        (\(attrName, attrValue) -> " " <> attrName <> "=\"" <> attrValue <> "\"")
        ( maybe [] (pure . (,) "id" . fromString) mId
            <> fmap
              ( bimap
                  (Builder.byteString . ByteString.Char8.pack)
                  (Builder.byteString . ByteString.Char8.pack)
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
    pure $ Builder.byteString (ByteString.Char8.pack t)
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
      <> fetch path (Builder.byteString fnId) "{}"
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
    pure $ "<span id=\"" <> fromString elId <> "\">" <> fromString initial <> "</span>"

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