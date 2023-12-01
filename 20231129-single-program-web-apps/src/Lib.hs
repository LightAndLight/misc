{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (
  App,
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
  stepper,
  stepperM,
  perform,
  request,
  toString,
  Html (..),
  DomEvent (..),
  module Network.HTTP.Types.Method,
) where

import Compiler.Plugin.Interface (Quoted (..), quote, toString)
import qualified Compiler.Plugin.Interface as Expr
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State (evalStateT, runStateT)
import Control.Monad.State.Class (MonadState, get, gets, modify, put)
import Control.Monad.Trans
import Control.Monad.Writer (WriterT, runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
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
import Data.Foldable (fold)
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any (..))
import Data.String (IsString (..))
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status (badRequest400, notFound404, ok200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.IO (hPutStrLn)
import qualified System.IO

newtype Path = Path [String]
  deriving (Eq, Ord, Semigroup, Monoid, Show)

instance IsString Path where
  fromString = Path . pure

data Interact :: Type -> Type where
  Pure :: a -> Interact a
  Apply :: Interact (a -> b) -> Interact a -> Interact b
  Bind :: Interact a -> (a -> Interact b) -> Interact b
  TextInput :: Interact (Behavior String, Element)
  Element :: Html -> Interact Element
  DomEvent :: DomEvent -> Element -> Interact (Event ())
  Perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
  Request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
  Stepper :: a -> Event a -> Interact (Reactive a)
  StepperM :: IO a -> Event a -> Interact (Reactive a)

instance Functor Interact where
  fmap f m = Pure f `Apply` m

instance Applicative Interact where
  pure = Pure
  (<*>) = Apply

instance Monad Interact where
  (>>=) = Bind

data Element = MkElement String Html

html :: Element -> Html
html (MkElement _ a) = a

element :: Html -> Interact Element
element = Element

textInput :: Interact (Behavior String, Element)
textInput = TextInput

data Event :: Type -> Type where
  FromDomEvent :: String -> DomEvent -> Event ()
  Sample :: Event a -> Behavior b -> Event (a, b)
  FromRequest :: Event a -> String -> Event b
  FmapEvent :: Quoted (a -> b) -> Event a -> Event b

instance Functor Event where
  {-# INLINE fmap #-}
  fmap f a = FmapEvent (quote f) a

data Behavior a
  = Behavior String

domEvent :: DomEvent -> Element -> Event ()
domEvent de (MkElement elId _) = FromDomEvent elId de

sample :: Event a -> Behavior b -> Event (a, b)
sample = Sample

data Reactive a = FromStepper a (Event a)

stepper :: a -> Event a -> Interact (Reactive a)
stepper = Stepper

stepperM :: IO a -> Event a -> Interact (Reactive a)
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
perform = Perform

request :: (Send a, Send b) => Event a -> (a -> IO b) -> Interact (Event b)
request = Request

data Page
  = Page Html
  | PageM (Interact Html)

newtype RPC = RPC (forall a. Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a)

renderPage :: (MonadState Int m, MonadIO m, MonadFix m) => Path -> Page -> m (Map ByteString RPC, Builder)
renderPage path (Page h) = renderHtml path (Js ["<!-- no script -->"]) h
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

data Subscribers = Subscribers
  { fetchers :: Map String (Path, String, String, Subscribers)
  , others :: [(String -> Js, String, Subscribers)]
  }

instance Semigroup Subscribers where
  Subscribers a b <> Subscribers a' b' = Subscribers (Map.unionWith (\(_, _, _, subs) (x, y, z, subs') -> (x, y, z, subs <> subs')) a a') (b <> b')

instance Monoid Subscribers where
  mempty = Subscribers mempty mempty

hasSubscribers :: Subscribers -> Bool
hasSubscribers (Subscribers a b) = not (Map.null a && null b)

subscribersJs :: Subscribers -> String -> Js
subscribersJs (Subscribers fs os) input =
  Map.foldrWithKey
    ( \fnId (path, result, temp, subscribers) rest ->
        Js
          [ "    fetch("
          , "      \"" <> renderPath path <> "\","
          , "      { method: \"POST\", body: JSON.stringify({ fn: \"" <> fnId <> "\", arg: " <> input <> " })}"
          , "    )"
          ]
          <> ( if hasSubscribers subscribers
                then
                  Js
                    [ "    .then("
                    , "      (" <> result <> ") => {"
                    , "        " <> result <> ".json().then((" <> temp <> ") => {"
                    ]
                    <> subscribersJs subscribers temp
                    <> Js
                      [ "        });"
                      , "      }"
                      , "    );"
                      ]
                else Js ["    ;"]
             )
          <> rest
    )
    ( foldMap
        ( \(code, result, subscribers) ->
            code input <> subscribersJs subscribers result
        )
        os
    )
    fs

data RenderState = RenderState
  { supply :: Int
  , eventListeners :: Map (String, DomEvent) Subscribers
  }

instance HasSupply RenderState where
  getSupply = supply
  setSupply s r = r{supply = s}

attachEventListeners :: Map (String, DomEvent) Subscribers -> Js
attachEventListeners =
  Map.foldrWithKey
    ( \(elId, de) v rest ->
        Js
          [ elId <> ".addEventListener("
          , renderDomEvent de <> ","
          , "(__ignore) => {"
          ]
          <> subscribersJs v "{}"
          <> Js
            [ "}"
            , ");"
            ]
          <> rest
    )
    mempty

renderInteractHtml :: (MonadState Int m, MonadIO m, MonadFix m) => Path -> Interact Html -> m (Map ByteString RPC, Builder)
renderInteractHtml path x = do
  s <- get
  ((h, (fns, script)), s') <- flip runStateT RenderState{supply = s, eventListeners = mempty} . runWriterT $ go x
  put $ getSupply s'
  let script' = script <> attachEventListeners (eventListeners s')
  (fns', content) <- renderHtml path script' h
  pure (fns <> fns', content)
 where
  go :: (MonadState RenderState m, MonadWriter (Map ByteString RPC, Js) m, MonadIO m, MonadFix m) => Interact a -> m a
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
  go (Perform ea f) = do
    let
      run :: Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
      run input resp =
        case decodeSend input of
          Left err -> do
            hPutStrLn System.IO.stderr $ show err
            undefined
          Right a -> do
            f a
            resp ""
    fnId <- ("function_" <>) <$> freshId
    subscribe <- performEventScript path ea
    result <- ("result_" <>) <$> freshId
    temp <- ("temp_" <>) <$> freshId
    subscribe $ Subscribers (Map.singleton fnId (path, result, temp, mempty)) mempty
    tell (Map.singleton (ByteString.Char8.pack fnId) (RPC run), mempty)
    pure ()
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
    tell (Map.singleton (ByteString.Char8.pack fnId) (RPC run), mempty)
    pure $ FromRequest ea fnId
  go (StepperM getInitial eUpdate) = do
    initial <- liftIO getInitial
    pure $ FromStepper initial eUpdate
  go (Stepper initial eUpdate) = do
    pure $ FromStepper initial eUpdate

performEventScript :: (MonadState RenderState m) => Path -> Event a -> m (Subscribers -> m ())
performEventScript path e =
  case e of
    FromDomEvent elId de -> do
      pure $ \subs -> modify $ \s -> s{eventListeners = Map.insertWith (\new old -> old <> new) (elId, de) subs (eventListeners s)}
    FromRequest e' fnId -> do
      result <- ("result_" <>) <$> freshId
      temp <- ("temp_" <>) <$> freshId
      subscribe <- performEventScript path e'
      pure $ \subs -> subscribe $ Subscribers (Map.singleton fnId (path, result, temp, subs)) mempty
    Sample e' (Behavior var) -> do
      temp <- ("temp_" <>) <$> freshId
      subscribe <- performEventScript path e'
      pure $ \subs ->
        subscribe
          $ Subscribers
            mempty
            [
              ( \target ->
                  Js ["const " <> temp <> " = { fst: " <> target <> ", snd: " <> var <> "};"]
              , temp
              , subs
              )
            ]
    FmapEvent (Quoted expr _) e' -> do
      temp <- ("temp_" <>) <$> freshId
      expr' <- exprToJavascript Expr.Nil expr
      subscribe <- performEventScript path e'
      pure $ \subs ->
        subscribe
          $ Subscribers
            mempty
            [
              ( \target ->
                  Js ["const " <> temp <> " = " <> expr' <> "(" <> target <> ");"]
              , temp
              , subs
              )
            ]

{-
performEventScript :: (MonadState RenderState m) => Path -> Event a -> (String -> Js) -> m ()
performEventScript path e code =
  case e of
    FromDomEvent elId de ->
      modify $ \s -> s{eventListeners = Map.insertWith (\new old -> old <> new) (elId, de) code (eventListeners s)}
    Sample e' (Behavior var) -> do
      temp <- ("temp_" <>) <$> freshId
      performEventScript
        path
        e'
        ( \target ->
            Js ["const " <> temp <> " = { fst: " <> target <> ", snd: " <> var <> "};"] <> code temp
        )
    FromRequest e' fnId -> do
      result <- ("result_" <>) <$> freshId
      temp <- ("temp_" <>) <$> freshId
      performEventScript
        path
        e'
        ( \target ->
            Js
              [ "    fetch("
              , "      \"" <> renderPath path <> "\","
              , "      { method: \"POST\", body: JSON.stringify({ fn: \"" <> fnId <> "\", arg: " <> target <> " })}"
              , "    ).then("
              , "      (" <> result <> ") => {"
              , "        " <> result <> ".json().then((" <> temp <> ") => {"
              ]
              <> code temp
              <> Js
                [ "        });"
                , "      }"
                , "    );"
                ]
        )
    FmapEvent (Quoted expr _) e' -> do
      temp <- ("temp_" <>) <$> freshId
      expr' <- exprToJavascript Expr.Nil expr
      performEventScript
        path
        e'
        ( \target ->
            Js ["const " <> temp <> " = " <> expr' <> "(" <> target <> ");"]
              <> code temp
        )
-}

exprToJavascript :: (MonadState s m, HasSupply s) => Expr.Ctx (Const String) ctx -> Expr.Expr ctx a -> m String
exprToJavascript ctx expr =
  case expr of
    Expr.Var v -> do
      let Const v' = Expr.getCtx v ctx
      pure v'
    Expr.Lam body -> do
      arg <- ("arg_" <>) <$> freshId
      body' <- exprToJavascript (Expr.Cons (Const arg) ctx) body
      pure $ "((" <> arg <> ") => " <> body' <> ")"
    Expr.App f x -> do
      f' <- exprToJavascript ctx f
      x' <- exprToJavascript ctx x
      pure $ f' <> "(" <> x' <> ")"
    Expr.Int i -> pure $ show i
    Expr.Add a b -> do
      a' <- exprToJavascript ctx a
      b' <- exprToJavascript ctx b
      pure $ "(" <> a' <> " + " <> b' <> ")"
    Expr.Bool b ->
      if b then pure "true" else pure "false"
    Expr.IfThenElse cond t e -> do
      cond' <- exprToJavascript ctx cond
      t' <- exprToJavascript ctx t
      e' <- exprToJavascript ctx e
      pure $ "(" <> cond' <> " ? " <> t' <> " : " <> e' <> ")"
    Expr.Lt a b -> do
      a' <- exprToJavascript ctx a
      b' <- exprToJavascript ctx b
      pure $ "(" <> a' <> " < " <> b' <> ")"
    Expr.Case a branches -> do
      value <- ("value_" <>) <$> freshId
      a' <- exprToJavascript ctx a
      result <- ("result_" <>) <$> freshId
      (branches', Any tagged) <- runWriterT $ traverse (branchToJavascript value result ctx) branches
      pure
        . unlines
        $ [ "((" <> value <> (if tagged then ".tag" else "") <> ") => {"
          , "switch (" <> value <> ") {"
          ]
        <> branches'
        <> [ "};"
           , "return " <> result <> ";"
           ]
        <> ["})(" <> a' <> ")"]
     where
      branchToJavascript :: (MonadState s m, HasSupply s) => String -> String -> Expr.Ctx (Const String) ctx -> Expr.Branch ctx a b -> WriterT Any m (String)
      branchToJavascript value result ctx (Expr.Branch pattern body) =
        case pattern of
          Expr.PDefault -> do
            body' <- lift $ exprToJavascript ctx body
            pure
              $ unlines
                [ "default:"
                , "  result = " <> body' <> ";"
                , "  break;"
                ]
          Expr.PInt i -> do
            body' <- lift $ exprToJavascript ctx body
            pure
              $ unlines
                [ "case " <> show i <> ":"
                , "  result = " <> body' <> ";"
                , "  break;"
                ]
    Expr.Char c -> do
      pure $ show c
    Expr.ToString -> do
      arg <- ("arg_" <>) <$> freshId
      pure $ "((" <> arg <> ") => JSON.stringify(" <> arg <> "))"

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
  , "    );"
  ]

renderHtml :: (MonadState Int m) => Path -> Js -> Html -> m (Map ByteString RPC, Builder)
renderHtml path postScript h = do
  s <- get
  (((h', _script), rpcs), s') <- flip runStateT RenderState{supply = s, eventListeners = mempty} . runWriterT . runWriterT $ go Nothing h
  put $ getSupply s'
  pure (rpcs, h')
 where
  go :: (MonadState RenderState m, MonadWriter (Map ByteString RPC) m) => Maybe String -> Html -> WriterT String m Builder
  go _ (Html children) = do
    (children', postScript') <- lift . runWriterT $ fold <$> traverse (go Nothing) children
    els <- gets eventListeners
    pure
      $ "<!doctype html>\n"
      <> "<html>\n"
      <> children'
      <> "<script>\n"
      <> Builder.byteString (ByteString.Char8.pack $ postScript' <> foldMap (<> "\n") (getJs $ postScript <> attachEventListeners els))
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
    lift . tell $ Map.singleton fnId (RPC $ \_ resp -> action *> resp "")
    element' <- go (Just elId) element
    pure
      . (element' <>)
      . foldMap (<> "\n")
      $ [ "<script>"
        , "const " <> fromString elId <> " = document.getElementById(\"" <> fromString elId <> "\");"
        , fromString elId <> ".addEventListener("
        , "  " <> renderDomEvent event <> ", "
        , "  (event) => { "
        ]
      <> fetch path (Builder.byteString fnId) "{}"
      <> [ "  }"
         , ");"
         , "</script>"
         ]
  go mId (WithScript el script) = do
    el' <- go mId el
    pure $ el' <> "<script>\n" <> Builder.byteString (ByteString.Char8.pack script) <> "</script>\n"
  go mId (ReactiveText (FromStepper initial eString)) = do
    textId <- maybe (("text_" <>) <$> freshId) pure mId
    subscribe <- performEventScript path eString
    subscribe $ Subscribers mempty [(\target -> Js [textId <> ".textContent = " <> target <> ";"], mempty, mempty)]
    pure
      $ ("<span id=\"" <> fromString textId <> "\">" <> fromString initial <> "</span>\n")
      <> ("<script>const " <> fromString textId <> " = " <> "document.getElementById(\"" <> fromString textId <> "\");</script>\n")

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
          (actions, content) <- flip evalStateT 0 $ renderPage ("" <> path) p
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