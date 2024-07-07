module Web.Request where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Data.Proxy (Proxy)
import Web.FormUrlEncoded (Form, urlDecodeForm)

class IsRequest a where
  requestContentType :: Proxy a -> ByteString
  decodeRequest :: Lazy.ByteString -> Maybe a

instance IsRequest Form where
  requestContentType _ = "application/x-www-form-urlencoded"
  decodeRequest = either (const Nothing) Just . urlDecodeForm
