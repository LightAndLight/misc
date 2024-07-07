{-# language GADTs #-}
module Web.Wai where

import Barbies (ApplicativeB, TraversableB)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Map as Map
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text
import Network.HTTP.Types.Status (mkStatus)
import qualified Network.Wai as Wai
import Web.Endpoint (Endpoint)
import Web.Route (Route, ActionType(..))
import Web.RouteMap (MethodEndpoints(..), SelectedEndpoint(..), toRouteMapUnchecked, findPath)
import Web.Request (requestContentType, decodeRequest)
import Web.Response (Response(..), encodeResponse)

runSelectedEndpoint :: SelectedEndpoint IO -> Wai.Request -> IO Wai.Response
runSelectedEndpoint (SelectedEndpoint NoBody m) _ = do
  Response status a <- m
  pure $ Wai.responseLBS (mkStatus status "") [] (encodeResponse a)
runSelectedEndpoint (SelectedEndpoint (WithBody @request) m) request =
  case Prelude.lookup "Content-Type" (Wai.requestHeaders request) of
    Nothing ->
      pure $ Wai.responseLBS (mkStatus 400 "bad request") [] "bad request"
    Just providedContentType ->
      if providedContentType == requestContentType (Proxy :: Proxy request)
      then do
        bytes <- fullRequestBody "" request
        case decodeRequest bytes of
          Nothing ->
            -- TODO: better error message
            pure $ Wai.responseLBS (mkStatus 400 "bad request") [] "bad request"
          Just a -> do
            Response status b <- m a
            pure $ Wai.responseLBS (mkStatus status "") [] (encodeResponse b)
      else
        pure $ Wai.responseLBS (mkStatus 415 "unsupported media type") [] "unsupported media type"
  where
    fullRequestBody acc r = do
      chunk <- Wai.getRequestBodyChunk r
      if ByteString.Char8.null chunk
        then pure acc
        else fullRequestBody (acc <> ByteString.Lazy.fromStrict chunk) r

toApplicationUnchecked ::
  (TraversableB b, ApplicativeB b) =>
  b Route ->
  b (Endpoint IO) ->
  Wai.Application
toApplicationUnchecked routes endpoints =
  \request respond ->
    case findPath (requestPath request) routeMap of
      Nothing ->
        respond $ Wai.responseLBS (mkStatus 404 "not found") [] "not found"
      Just (MethodEndpoints methods) ->
        case Map.lookup (requestMethod request) methods of
          Nothing ->
            respond $ Wai.responseLBS (mkStatus 405 "method not allowed") [] "method not allowed"
          Just endpoint -> do
            response <- runSelectedEndpoint endpoint request
            respond response
  where
    routeMap = toRouteMapUnchecked routes endpoints
    
    requestMethod :: Wai.Request -> String
    requestMethod = ByteString.Char8.unpack . Wai.requestMethod
    
    requestPath :: Wai.Request -> [String]
    requestPath = fmap Text.unpack . Wai.pathInfo
