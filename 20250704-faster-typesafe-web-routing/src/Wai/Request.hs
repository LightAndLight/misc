-- | Request-related re-exports from "Network.Wai".
module Wai.Request
  ( Request
      -- ** Request accessors
    , requestMethod
    , httpVersion
    , rawPathInfo
    , rawQueryString
    , requestHeaders
    , isSecure
    , remoteHost
    , pathInfo
    , queryString
    , getRequestBodyChunk
    , requestBody
    , vault
    , RequestBodyLength (..)
    , requestBodyLength
    , requestHeaderHost
    , requestHeaderRange
    , requestHeaderReferer
    , requestHeaderUserAgent
    -- ** Streaming request bodies
    , strictRequestBody
    , consumeRequestBodyStrict
    , lazyRequestBody
    , consumeRequestBodyLazy
      -- ** Request modifiers
    , setRequestBodyChunks
    , mapRequestHeaders
  ) where

import Network.Wai hiding (Request)
import Network.Wai.Internal
