{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib (
  App,
  serve,
  page,
  pageM,
  Interact,
  Event,
  Behavior,
  Element,
  html,
  element,
  textInput,
  domEvent,
  sample,
  perform,
  Html (..),
  DomEvent (..),
  module Network.HTTP.Types.Method,
) where

import Control.Monad.State (evalState)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Data.Aeson (FromJSON)
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
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (..))
import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
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
  FromDomEvent :: String -> DomEvent -> Event a
  Sample :: Event a -> Behavior b -> Event (a, b)

data Behavior a = Behavior String

domEvent :: DomEvent -> Element -> Event ()
domEvent de (MkElement elId _) = FromDomEvent elId de

sample :: Event a -> Behavior b -> Event (a, b)
sample = Sample

class (FromJSON (SendTy a)) => Send a where
  type SendTy a :: Type
  fromSendTy :: SendTy a -> a

instance Send () where
  type SendTy () = ()
  fromSendTy = id

instance Send String where
  type SendTy String = String
  fromSendTy = id

instance (Send a, Send b) => Send (a, b) where
  type SendTy (a, b) = SendPair (SendTy a) (SendTy b)
  fromSendTy (SendPair a b) = (fromSendTy a, fromSendTy b)

data SendPair a b = SendPair {fst :: a, snd :: b}
  deriving (Generic)
instance (FromJSON a, FromJSON b) => FromJSON (SendPair a b)

decodeSend :: (Send a) => Lazy.ByteString -> Either String a
decodeSend = fmap fromSendTy . Json.eitherDecode'

perform :: (Send a) => Event a -> (a -> IO ()) -> Interact ()
perform = Perform

data Page
  = Page Html
  | PageM (Interact Html)

renderPage :: (MonadState Int m) => Path -> Page -> m (Map ByteString (Lazy.ByteString -> IO ()), Builder)
renderPage path (Page html) = renderHtml path "<!-- no script -->" html
renderPage path (PageM html) = renderInteractHtml path html

freshId :: (MonadState Int m) => m String
freshId = do
  n <- get
  put $ n + 1
  pure $ show n

setId :: String -> Html -> Html
setId i (Node name attrs children) = Node name (("id", i) : attrs) children
setId _ a = a

renderInteractHtml :: (MonadState Int m) => Path -> Interact Html -> m (Map ByteString (Lazy.ByteString -> IO ()), Builder)
renderInteractHtml path x = do
  (h, (fns, script)) <- runWriterT $ go x
  (fns', content) <- renderHtml path script h
  pure (fns <> fns', content)
 where
  -- uncurry (renderHtml path) . Tuple.swap . second Tuple.snd <=<

  go :: (MonadState Int m, MonadWriter (Map ByteString (Lazy.ByteString -> IO ()), String) m) => Interact a -> m a
  go (Pure html) = pure html
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
      run :: Lazy.ByteString -> IO ()
      run input =
        case decodeSend input of
          Left err ->
            hPutStrLn System.IO.stderr $ show err
          Right a ->
            f a
    fnId <- ("function_" <>) <$> freshId
    script <- foldMap (<> "\n") <$> performEventScript ea (fetch path fnId)
    tell (Map.singleton (ByteString.Char8.pack fnId) run, script)
    pure ()

performEventScript :: (MonadState Int m) => Event a -> (String -> [String]) -> m [String]
performEventScript e code =
  case e of
    FromDomEvent elId de ->
      pure
        $ [ elId <> ".addEventListener("
          , "  " <> renderDomEvent de <> ","
          , "  (event) => {"
          ]
        <> code "{}"
        <> [ "  }"
           , ");"
           ]
    Sample e' (Behavior var) -> do
      temp <- ("temp_" <>) <$> freshId
      performEventScript
        e'
        (\target -> ["const " <> temp <> " = { fst: " <> target <> ", snd: " <> var <> "};"] <> code temp)

data Html
  = Html [Html]
  | Node String [(String, String)] [Html]
  | WithScript Html String
  | Text String
  | OnEvent Html (DomEvent, IO ())

data DomEvent
  = Click
  | Change

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
fetch path fnId arg =
  [ "    fetch("
  , "      \"" <> renderPath path <> "\","
  , "      { method: \"POST\", body: JSON.stringify({ fn: \"" <> fnId <> "\", arg: " <> arg <> " })}"
  , "    );"
  ]

renderHtml :: (MonadState Int m) => Path -> String -> Html -> m (Map ByteString (Lazy.ByteString -> IO ()), Builder)
renderHtml path postScript = fmap Tuple.swap . runWriterT . go Nothing
 where
  go :: (MonadState Int m, MonadWriter (Map ByteString (Lazy.ByteString -> IO ())) m) => Maybe Builder -> Html -> m Builder
  go _ (Html children) = do
    children' <- fold <$> traverse (go Nothing) children
    pure
      $ "<!doctype html>\n"
      <> "<html>\n"
      <> children'
      <> "<script>\n"
      <> Builder.byteString (ByteString.Char8.pack postScript)
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
        ( maybe [] (pure . (,) "id") mId
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
    elId <- maybe (Builder.byteString . ByteString.Char8.pack . ("element_" <>) <$> freshId) pure mId
    fnId <- ByteString.Char8.pack . ("function_" <>) <$> freshId
    tell $ Map.singleton fnId (const action)
    element' <- go (Just elId) element
    pure
      . (element' <>)
      . foldMap (<> "\n")
      $ [ "<script>"
        , "const " <> elId <> " = document.getElementById(\"" <> elId <> "\");"
        , elId <> ".addEventListener("
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

compile :: App -> Wai.Application
compile (App paths) =
  let
    actionsAndPages :: Map Path (Map Method (Either (Map ByteString (Lazy.ByteString -> IO ())) Builder))
    actionsAndPages =
      Map.mapWithKey
        ( \path p ->
            let (actions, content) = flip evalState 0 $ renderPage ("" <> path) p
             in if Map.null actions
                  then Map.singleton methodGet (Right content)
                  else Map.insert methodGet (Right content) $ Map.singleton methodPost (Left actions)
        )
        paths
   in
    \request respond -> do
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
                            Just action -> do
                              action (Json.encode arg)
                              respond $ Wai.responseBuilder ok200 [] mempty
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
  Warp.run port (compile app)