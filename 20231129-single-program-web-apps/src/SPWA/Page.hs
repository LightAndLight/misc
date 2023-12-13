{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module SPWA.Page (Page (..), renderPage) where

import Control.Concurrent.STM
import Control.Monad (when)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.Reader.Class (asks)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans
import Data.Bifunctor (bimap)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import qualified Data.Dependent.Map as DMap
import Data.Foldable (fold)
import qualified Data.Map as Map
import Data.String (IsString (..))
import Data.Traversable (for)
import SPWA.DomEvent (renderDomEvent)
import SPWA.Element (Element (..))
import SPWA.Html (Html (..), setId)
import SPWA.Interact (Interact (..))
import SPWA.Js (Js (..))
import qualified SPWA.Js as Js
import SPWA.MemoRef (newMemoRef, readMemoRef, setMemoRef)
import SPWA.PageBuilder (
  Behavior (..),
  Event (..),
  EventKey (..),
  PageBuilder,
  PageBuilderEnv (..),
  PageBuilderState (..),
  ReactiveInfo (..),
  TriggerId (..),
  appendPostScript,
  initReactive,
  memoEventWith,
  mkReactive,
  newBehavior,
  newEvent,
  notify,
  queueAction,
  subscribe,
 )
import SPWA.Path (Path, renderPath)
import SPWA.RPC (RPC (..))
import SPWA.Reactive (Reactive (..))
import SPWA.Send (decodeSend, encodeSend)
import SPWA.Session (Session (..), SessionEnv (..), runSession)
import SPWA.Supply (freshId)
import SPWA.Template (Template)
import System.IO (hPutStrLn)
import qualified System.IO

data Page
  = Page Html
  | PageM (Interact Html)

renderPage :: Path -> Page -> PageBuilder (Template IO Builder)
renderPage path (Page h) = renderHtml path h
renderPage path (PageM h) = renderInteractHtml path h

fetch :: (IsString s, Monoid s) => Path -> s -> s -> s -> [s]
fetch path conn fnId a =
  [ "fetch("
  , "  \"" <> renderPath path <> "\","
  , "  { method: \"POST\", body: JSON.stringify({ conn: " <> conn <> ", fn: \"" <> fnId <> "\", arg: " <> a <> " })}"
  , ")"
  ]

renderHtml :: Path -> Html -> PageBuilder (Template IO Builder)
renderHtml path h = do
  go Nothing h
 where
  flushQueue :: String -> PageBuilder Js
  flushQueue queueName = do
    temp <- ("temp_" <>) <$> freshId
    pure
      $ Js ["while (" <> queueName <> ".length > 0) {"]
      <> Js.indent
        2
        ( Js
            [ "const " <> temp <> " = " <> queueName <> ".shift();"
            , temp <> "();"
            ]
        )
      <> Js ["}"]

  makeJs :: Js
  makeJs =
    Js
      [ "function make(html) {"
      , "  var el;"
      , "  switch (html.tag) {"
      , "    case \"Node\":"
      , "      const tag = html.args[0];"
      , "      const attrs = html.args[1];"
      , "      const children = html.args[2];"
      , "      el = document.createElement(tag);"
      , "      for (attr of attrs) {"
      , "        el.setAttribute(attr.fst, attr.snd);"
      , "      }"
      , "      for (child of children) {"
      , "        el.appendChild(make(child));"
      , "      }"
      , "      break;"
      , "    case \"Text\":"
      , "      const text = html.args[0];"
      , "      el = document.createTextNode(text);"
      , "      break;"
      , "  }"
      , "  return el;"
      , "}"
      , ""
      , "function make_with_id(id, html) {"
      , "  const el = make(html);"
      , "  el.setAttribute(\"id\", id);"
      , "  return el;"
      , "}"
      ]

  go :: Maybe String -> Html -> PageBuilder (Template IO Builder)
  go _ (Html children) = do
    writeQueueName <- asks pbe_writeQueueName

    children' <- fold <$> traverse (go Nothing) children

    needsMake <- gets pbs_needsMake

    flushQueueJs <- flushQueue writeQueueName

    domEventSubscriptions <- gets pbs_domEventSubscriptions
    domEventSubscriptionsJs <- fmap fold . for (Map.toList domEventSubscriptions) $ \((elId, de), callback) -> do
      arg <- ("arg_" <>) <$> freshId
      pure
        $ Js
          [elId <> ".addEventListener("]
        <> Js.indent
          2
          ( Js
              [ renderDomEvent de <> ","
              , "(" <> arg <> ") => {"
              ]
              <> Js.indent
                2
                ( callback arg
                    <> flushQueueJs
                )
              <> Js ["}"]
          )
        <> Js [");"]

    hasTrigger <- gets pbs_hasTrigger
    socketVar <- ("socket_" <>) <$> freshId
    when hasTrigger . appendPostScript . pure $ Js.line ("var " <> socketVar <> ";")

    onLoadSubscriptions <- gets pbs_onLoadSubscriptions
    triggerSubscriptions <- gets pbs_triggerSubscriptions
    onLoadSubscriptionsJs <- do
      var <- ("var_" <>) <$> freshId
      socketOpenVar <- ("var_" <>) <$> freshId
      socketMessageVar <- ("var_" <>) <$> freshId
      connectionIdName <- asks pbe_connectionIdName
      pure
        $ Js.line "window.addEventListener("
        <> Js.indent
          2
          ( Js.line "\"load\","
              <> Js.line ("(" <> var <> ") => {")
              <> Js.indent
                2
                ( if hasTrigger
                    then
                      Js
                        [ socketVar <> " = new WebSocket(\"ws://localhost:8000" <> renderPath path <> "?connectionId=\" + " <> connectionIdName <> ");"
                        , socketVar <> ".addEventListener("
                        ]
                        <> Js.indent
                          2
                          ( Js
                              [ "\"open\","
                              , "(" <> socketOpenVar <> ") => {"
                              ]
                              <> Js.indent
                                2
                                ( onLoadSubscriptions var
                                    <> Js.line (socketVar <> ".addEventListener(")
                                    <> Js.indent
                                      2
                                      ( Js
                                          [ "\"message\","
                                          , "(" <> socketMessageVar <> ") => {"
                                          ]
                                          <> triggerSubscriptions ("JSON.parse(" <> socketMessageVar <> ".data)")
                                          <> Js.line "}"
                                      )
                                    <> Js.line ");"
                                )
                              <> Js.line "}"
                          )
                        <> Js.line ");"
                    else onLoadSubscriptions var
                )
              <> Js.line "}"
          )
        <> Js.line ");"

    postScript <- gets pbs_postScript
    pure
      $ "<!doctype html>\n"
      <> "<html>\n"
      <> children'
      <> "<script>\n"
      <> fmap (Builder.byteString . ByteString.Char8.pack . foldMap (<> "\n") . getJs) ((if needsMake then pure makeJs else mempty) <> postScript)
      <> pure
        ( Builder.byteString
            $ ByteString.Char8.pack
            . foldMap (<> "\n")
            $ getJs (domEventSubscriptionsJs <> onLoadSubscriptionsJs)
        )
      <> "</script>"
      <> "</html>\n"
  go mId (Node name attrs children) = do
    let nameBytes = ByteString.Char8.pack name
    children' <- fold <$> traverse (go Nothing) children
    pure
      $ "<"
      <> pure (Builder.byteString nameBytes)
      <> foldMap
        (\(attrName, attrValue) -> " " <> attrName <> "=\"" <> attrValue <> "\"")
        ( maybe [] (pure . (,) "id" . fromString) mId
            <> fmap
              ( bimap
                  (pure . Builder.byteString . ByteString.Char8.pack)
                  (pure . Builder.byteString . ByteString.Char8.pack)
              )
              attrs
        )
      <> ">"
      <> (case children of [] -> ""; [Text{}] -> ""; _ -> "\n")
      <> children'
      <> "</"
      <> pure (Builder.byteString nameBytes)
      <> ">\n"
  go mId (Void name attrs) = do
    let nameBytes = ByteString.Char8.pack name
    pure
      $ "<"
      <> pure (Builder.byteString nameBytes)
      <> foldMap
        (\(attrName, attrValue) -> " " <> attrName <> "=\"" <> attrValue <> "\"")
        ( maybe [] (pure . (,) "id" . fromString) mId
            <> fmap
              ( bimap
                  (pure . Builder.byteString . ByteString.Char8.pack)
                  (pure . Builder.byteString . ByteString.Char8.pack)
              )
              attrs
        )
      <> ">"
  go _ (Text t) =
    -- TODO: escape text
    pure . pure $ Builder.byteString (ByteString.Char8.pack t)
  go mId (OnEvent element (event, action)) = do
    elId <- maybe (("element_" <>) <$> freshId) pure mId
    fnId <- ByteString.Char8.pack . ("function_" <>) <$> freshId
    arg <- ("arg_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert fnId (RPC $ \_ _ resp -> action *> resp "") (pbs_rpcs s)}
    element' <- go (Just elId) element
    conn <- asks pbe_connectionIdName
    pure
      . (element' <>)
      . foldMap (<> "\n")
      $ [ "<script>"
        , "const " <> fromString elId <> " = document.getElementById(\"" <> fromString elId <> "\");"
        , fromString elId <> ".addEventListener("
        , "  " <> renderDomEvent event <> ", "
        , "  (" <> fromString arg <> ") => { "
        ]
      <> fmap ("  " <>) (fetch path (fromString conn) (pure $ Builder.byteString fnId) "{}")
      <> [ "  }"
         , ");"
         , "</script>"
         ]
  go mId (WithScript el script) = do
    el' <- go mId el
    pure $ el' <> "<script>\n" <> pure (Builder.byteString $ ByteString.Char8.pack script) <> "</script>\n"
  go _mId (ReactiveText ra) = do
    elId <- ("element_" <>) <$> freshId
    reactiveKey <- initReactive ra
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives
    event <- mkEvent
    subscribe (Event $ pure event) $ \value -> Js [elId <> ".textContent = " <> value <> ";"]
    pure $ "<span id=\"" <> pure (fromString elId) <> "\">" <> liftIO (fmap fromString initial) <> "</span>"
  go _mId (ReactiveHtml rHtml) = do
    modify $ \s -> s{pbs_needsMake = True}
    elId <- ("element_" <>) <$> freshId
    reactiveKey <- initReactive rHtml
    ReactiveInfo initial mkEvent _ <- gets $ (DMap.! reactiveKey) . pbs_reactives
    i <- go (Just elId) =<< liftIO initial
    event <- mkEvent
    subscribe (Event $ pure event) $ \value -> Js [elId <> ".replaceWith(make_with_id(\"" <> elId <> "\", " <> value <> "));"]
    pure i

renderInteractHtml :: Path -> Interact Html -> PageBuilder (Template IO Builder)
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
    stateName <- ("state_" <>) <$> freshId
    behaviorName <- ("behavior_" <>) <$> freshId
    pure
      ( Behavior behaviorName
      , MkElement elId
          $ Node "input" [("id", elId), ("type", "text")] []
          `WithScript` unlines
            [ "const " <> elId <> " = document.getElementById(\"" <> elId <> "\");"
            , "var " <> stateName <> " = \"\";"
            , "const " <> behaviorName <> " = () => " <> stateName <> ";"
            , elId <> ".addEventListener("
            , "  \"change\","
            , "  (event) => {"
            , "    " <> stateName <> " = " <> elId <> ".value;"
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
    let run :: SessionEnv -> Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
        run _ input resp =
          case decodeSend input of
            Left err -> do
              hPutStrLn System.IO.stderr $ show err
              undefined
            Right a -> do
              b <- f a
              resp (encodeSend b)

    fnId <- ("function_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert (ByteString.Char8.pack fnId) (RPC run) (pbs_rpcs s)}

    conn <- asks pbe_connectionIdName
    subscribe ea $ \value -> Js (fetch path conn fnId value)
  go (Request ea f) = do
    memoRef <- liftIO newMemoRef
    pure . Event . memoEventWith memoRef $ \name -> do
      let run :: SessionEnv -> Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
          run _ input resp =
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
      conn <- asks pbe_connectionIdName
      subscribe ea $ \value ->
        Js
          (fetch path conn fnId value)
          <> Js [".then((" <> response <> ") => {"]
          <> Js.indent
            2
            ( Js [response <> ".json().then((" <> arg <> ") => {"]
                <> Js.indent 2 (notifyJs arg)
                <> Js ["});"]
            )
          <> Js ["});"]
  go (StepperRM getInitial eUpdate) =
    Reactive <$> mkReactive getInitial eUpdate
  go (StepperR initial eUpdate) = do
    Reactive <$> mkReactive (pure initial) eUpdate
  go (StepperB initial eUpdate) = do
    memoRef <- liftIO newMemoRef
    pure . Behavior' $ do
      mName <- liftIO $ readMemoRef memoRef
      case mName of
        Just name ->
          pure name
        Nothing -> do
          (name, state) <- newBehavior (pure . ByteString.Lazy.Char8.unpack $ encodeSend initial)
          liftIO $ setMemoRef memoRef name
          writeQueueName <- asks pbe_writeQueueName
          subscribe eUpdate $ \value -> queueAction writeQueueName (Js [state <> " = " <> value <> ";"])
          pure name
  go (MFix f) = mfix (go . f)
  go (OnLoad action) = do
    {- `OnLoad action` is essentially `Perform eOnLoad action` for
    a fictional `eOnLoad` event that fires on page load.

    I'm leaving `eOnLoad` out for now, because it feels too much like
    an implementation detail. `onLoad :: IO () -> Interact ()` says that
    the action is not run as part of describing the page (`Interact Html`),
    but it is run when that page is alive.
    -}
    let run :: SessionEnv -> Lazy.ByteString -> (Lazy.ByteString -> IO a) -> IO a
        run sessionEnv input resp =
          case decodeSend input of
            Left err -> do
              hPutStrLn System.IO.stderr $ show err
              undefined
            Right () -> do
              runSession action sessionEnv
              resp (encodeSend ())

    fnId <- ("function_" <>) <$> freshId
    modify $ \s -> s{pbs_rpcs = Map.insert (ByteString.Char8.pack fnId) (RPC run) (pbs_rpcs s)}

    conn <- asks pbe_connectionIdName
    modify $ \s ->
      s
        { pbs_onLoadSubscriptions =
            pbs_onLoadSubscriptions s
              <> (\value -> Js (fetch path conn fnId value))
        }
  go MkTrigger = do
    modify $ \s -> s{pbs_hasTrigger = True}
    triggerId <- TriggerId . ("trigger_" <>) <$> freshId
    let event = Event . pure $ EventKey_Trigger triggerId
    pure
      ( \value -> Session $ \env -> do
          conns <- atomically $ readTVar (connections env)
          case Map.lookup (connectionId env) conns of
            Nothing -> error $ "connection missing: " <> show (connectionId env)
            Just queue -> atomically $ writeTQueue queue (encodeSend value)
      , event
      )