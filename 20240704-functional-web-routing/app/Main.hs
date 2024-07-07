{-# language DeriveGeneric #-}
{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}
{-# language GeneralisedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module Main where

import Barbies
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (traverse_)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy (encodeUtf8)
import GHC.Generics (Generic)
import Web.FormUrlEncoded (Form, FromForm, urlDecodeAsForm)

import Web.Body
import Web.Endpoint
import Web.Link
import Web.Param
import Web.Path
import Web.Request
import Web.Response
import Web.Route
import Web.RouteMap
import Web.Warp

newtype Html = Html Lazy.ByteString
  deriving newtype (Semigroup, Monoid)

instance IsString Html where
  fromString = Html . Text.Lazy.encodeUtf8 . Text.Lazy.pack

instance IsResponse Html where
  encodeResponse (Html h) = h

htmlLines :: [Html] -> Html
htmlLines = Html . foldMap (\(Html h) -> h <> "\n")

data CreatePost = CreatePost { createPostTitle :: String, createPostContent :: String }
  deriving (Show, Generic, FromForm)

instance IsRequest CreatePost where
  requestContentType _ = requestContentType (Proxy :: Proxy Form)
  decodeRequest = either (const Nothing) Just . urlDecodeAsForm

data UpdatePost = UpdatePost { updatePostTitle :: Maybe String, updatePostContent :: Maybe String }
  deriving (Show, Generic, FromForm)

instance IsRequest UpdatePost where
  requestContentType _ = requestContentType (Proxy :: Proxy Form)
  decodeRequest = either (const Nothing) Just . urlDecodeAsForm

data Api f
  = Api
  { listPosts :: f (Response Html)
  , newPost :: f (Response Html)
  , getPost :: f (Param Int -> Response Html)
  , editPost :: f (Param Int -> Response Html)
  , createPost :: f (Body CreatePost -> Response Html)
  , updatePost :: f (Param Int -> Body UpdatePost -> Response Html)
  } deriving (Generic, FunctorB, TraversableB, ApplicativeB)

manualApiLinks :: Api Link
manualApiLinks =
  Api
  { listPosts = mkLink ["posts"]
  , newPost = mkLink ["posts", "new"]
  , getPost = mkLink (\postId -> ["posts", show postId])
  , editPost = mkLink (\postId -> ["posts", show postId, "edit"])
  , createPost = mkLink ["posts"]
  , updatePost = mkLink (\postId -> ["posts", show postId])
  }

apiRoutes :: Api Route
apiRoutes =
  Api
  { listPosts =
      route_ "GET" ("posts" // end) (response @Html)
  , newPost =
      route_ "GET" ("posts" // "new" // end) (response @Html)
  , getPost =
      route_ "GET" ("posts" // param "postId" @Int // end) (response @Html)
  , editPost =
      route_ "GET" ("posts" // param "postId" @Int // "edit" // end) (response @Html)
  , createPost =
      route "POST" ("posts" // end) (request @CreatePost) (response @Html)
  , updatePost =
      route "PATCH" ("posts" // param "postId" @Int // end) (request @UpdatePost) (response @Html)
  }

apiEndpoints :: Api (Endpoint IO)
apiEndpoints =
  Api
  { listPosts = mkEndpoint $ do
      putStrLn "listed posts"
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>List posts</title>"
        , "</head>"
        , "<body>"
        , "<p>Listed posts</p>"
        , "</body>"
        , "</html>"
        ]
  , newPost = mkEndpoint $ do
      putStrLn "new post"
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>New post</title>"
        , "</head>"
        , "<body>"
        , "<p>New post</p>"
        , "</body>"
        , "</html>"
        ]
  , getPost = mkEndpoint $ \postId -> do
      putStrLn $ "got post " <> show postId
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>Got post</title>"
        , "</head>"
        , "<body>"
        , "<p>Got post: " <> fromString (show postId) <> "</p>"
        , "</body>"
        , "</html>"
        ]
  , editPost = mkEndpoint $ \postId -> do
      putStrLn $ "editing post " <> show postId
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>Edit post</title>"
        , "</head>"
        , "<body>"
        , "<p>Edit post: " <> fromString (show postId) <> "</p>"
        , "</body>"
        , "</html>"
        ]
  , createPost = mkEndpoint $ \body -> do
      putStrLn $ "created post " <> show body
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>Created post</title>"
        , "</head>"
        , "<body>"
        , "<p>Created post: " <> fromString (show body) <> "</p>"
        , "</body>"
        , "</html>"
        ]
  , updatePost = mkEndpoint $ \postId body -> do
      putStrLn $ "updated post " <> show postId <> " " <> show body
      pure . success200 . htmlLines $
        [ "<!doctype html>"
        , "<html>"
        , "<head>"
        , "<title>Updated post</title>"
        , "</head>"
        , "<body>"
        , "<p>Updated post: " <> fromString (show postId) <> " " <> fromString (show body) <> "</p>"
        , "</body>"
        , "</html>"
        ]
  }

apiRouteMap :: RouteMap (SelectedEndpoint IO)
apiRouteMap = toRouteMapUnchecked apiRoutes apiEndpoints

example1 :: String
example1 = url (getPost manualApiLinks) 1

example2 :: String
example2 =
  routeMethod (getPost apiRoutes) <> " " <>
  url (routeLink $ getPost apiRoutes) 1

main :: IO ()
main = do
  putStrLn "debug"
  print $ debugRouteMap apiRouteMap
  putStrLn "Sitemap"
  traverse_ (putStrLn . ("  " <>)) (sitemap apiRoutes)
  serve 3000 apiRoutes apiEndpoints
