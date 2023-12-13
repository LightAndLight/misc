{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SPWA.App (App (..), serve, toWaiApplication, page, pageM) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (catch, finally)
import Control.Monad (forever)
import Control.Monad.State.Class (gets)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Json
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.HTTP.Types.Status
import Network.Wai (queryString)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets (
  ConnectionException,
  PendingConnection,
  acceptRequest,
  defaultConnectionOptions,
  receiveDataMessage,
  sendTextData,
 )
import SPWA.Html (Html)
import SPWA.Interact (Interact)
import SPWA.Page (Page (..), renderPage)
import SPWA.PageBuilder (PageBuilderState (..), runPageBuilder)
import SPWA.Path (Path (..))
import SPWA.RPC (RPC (..))
import SPWA.Session (ConnectionId (..), SessionEnv (..))
import SPWA.Template (Template, runTemplate)
import System.IO (hPutStrLn)
import qualified System.IO

-- | [[ App ]] = Path -> Maybe Page
newtype App = App (Map Path Page)
  deriving (Semigroup, Monoid)

page :: Path -> Html -> App
page p v = App (Map.singleton p $ Page v)

pageM :: Path -> Interact Html -> App
pageM p v = App (Map.singleton p $ PageM v)

data PathResponse
  = PathResponse_Page
      (Template IO Builder)
      -- | Whether the page has server-to-client triggers
      Bool
  | PathResponse_Actions (Map ByteString RPC)

data ActionRequest = ActionRequest {conn :: UUID, fn :: String, arg :: Json.Value}
  deriving (Generic)

instance FromJSON ActionRequest

toWaiApplication :: App -> IO Wai.Application
toWaiApplication (App paths) = do
  actionsAndPages <-
    Map.traverseWithKey
      ( \path p -> do
          (actions, content, hasTrigger) <- runPageBuilder $ do
            p' <- renderPage ("" <> path) p
            rpcs <- gets pbs_rpcs
            hasTrigger <- gets pbs_hasTrigger
            pure (rpcs, p', hasTrigger)
          pure
            $ if Map.null actions
              then Map.singleton methodGet (PathResponse_Page content hasTrigger)
              else Map.insert methodGet (PathResponse_Page content hasTrigger) $ Map.singleton methodPost (PathResponse_Actions actions)
      )
      paths

  triggerQueues <- newTVarIO mempty
  connectionThreads <- newTVarIO mempty

  pure $ \request respond -> do
    let requestPath :: Path
        requestPath = Path . fmap Text.unpack $ Wai.pathInfo request

    case Map.lookup requestPath actionsAndPages of
      Nothing ->
        respond $ Wai.responseLBS notFound404 [] "not found"
      Just methods ->
        let method = Wai.requestMethod request
         in case Map.lookup method methods of
              Nothing ->
                respond . Wai.responseLBS badRequest400 [] $ "invalid method: " <> ByteString.Lazy.fromStrict method
              Just pathResponse ->
                case pathResponse of
                  PathResponse_Page mkContents hasTrigger ->
                    if hasTrigger && isWebSocketsReq request
                      then case lookup "connectionId" $ queryString request of
                        Just (Just connectionIdRaw) ->
                          case UUID.fromString $ Data.ByteString.Char8.unpack connectionIdRaw of
                            Just connectionId ->
                              case websocketsApp defaultConnectionOptions (wsServer triggerQueues connectionThreads $ ConnectionId connectionId) request of
                                Just response ->
                                  respond response
                                Nothing ->
                                  respond $ Wai.responseLBS badRequest400 [] "unexpected websocket request"
                            Nothing ->
                              respond $ Wai.responseLBS badRequest400 [] "invalid connectionId value in websocket request"
                        _ ->
                          respond $ Wai.responseLBS badRequest400 [] "invalid connectionId query parameter in websocket request"
                      else do
                        contents <- runTemplate mkContents
                        respond $ Wai.responseBuilder ok200 [] contents
                  PathResponse_Actions pathActions -> do
                    result <- decodeJsonBody @ActionRequest request
                    case result of
                      Left err -> do
                        hPutStrLn System.IO.stderr $ show err
                        respond $ Wai.responseBuilder badRequest400 [] "invalid action request"
                      Right (ActionRequest conn fn' arg) -> do
                        case Map.lookup (ByteString.Char8.pack fn') pathActions of
                          Nothing ->
                            respond $ Wai.responseLBS badRequest400 [] "invalid action request"
                          Just (RPC action) -> do
                            action
                              (SessionEnv (ConnectionId conn) triggerQueues connectionThreads)
                              (Json.encode arg)
                              (respond . Wai.responseBuilder ok200 [] . Builder.lazyByteString)
 where
  decodeJsonBody :: (FromJSON a) => Wai.Request -> IO (Either String a)
  decodeJsonBody request = go mempty
   where
    go acc = do
      bs <- Wai.getRequestBodyChunk request
      if ByteString.null bs
        then pure . Json.eitherDecode' $ Builder.toLazyByteString acc
        else go (acc <> Builder.byteString bs)

  wsServer ::
    TVar (Map ConnectionId (TQueue Lazy.ByteString)) ->
    TVar (Map ConnectionId [ThreadId]) ->
    ConnectionId ->
    PendingConnection ->
    IO ()
  wsServer triggerQueues connectionThreads connectionId pending = do
    putStrLn $ "accepting connection " <> show connectionId
    connection <- acceptRequest pending
    queue <- atomically $ do
      queue <- newTQueue
      modifyTVar triggerQueues $ Map.insert connectionId queue
      pure queue
    closedVar <- newTVarIO False
    void
      . forkIO
      $ forever (void $ receiveDataMessage connection)
      `finally` ( do
                    mThreads <- atomically $ do
                      writeTVar closedVar True
                      Map.lookup connectionId <$> readTVar connectionThreads
                    (traverse_ . traverse_) killThread mThreads
                )
      `catch` (\(e :: ConnectionException) -> putStrLn $ "connection " <> show connectionId <> " terminated: " <> show e)
    let
      loop = do
        mValue <- atomically $ do
          closed <- readTVar closedVar
          if closed then pure Nothing else Just <$> readTQueue queue
        case mValue of
          Nothing ->
            pure ()
          Just value -> do
            sendTextData connection value
            loop
    loop

serve :: Int -> App -> IO ()
serve port app = do
  putStrLn $ "app running on port " <> show port
  Warp.run port =<< toWaiApplication app