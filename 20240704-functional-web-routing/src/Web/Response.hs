module Web.Response where

import qualified Data.ByteString.Lazy as Lazy

data Response a = Response Int a

success200 :: a -> Response a
success200 = Response 200

notFound404 :: a -> Response a
notFound404 = Response 404

class IsResponse a where
  encodeResponse :: a -> Lazy.ByteString

instance IsResponse Lazy.ByteString where
  encodeResponse = id
