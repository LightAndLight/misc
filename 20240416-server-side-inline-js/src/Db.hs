{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module Db where

import Data.Text (Text)
import Data.String (IsString)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Lib (Server (..))
import Data.IORef (IORef, newIORef)
import Data.Aeson (ToJSON, FromJSON)
import qualified Js
import System.IO.Unsafe (unsafePerformIO)

newtype PostId = PostId Int
  deriving Eq

newtype PostSlug = PostSlug Text
  deriving newtype (Eq, Show, IsString, FromHttpApiData, ToHttpApiData)

data Post f
  = Post
  { id :: f PostId
  , title :: f Text
  , slug :: f PostSlug
  }

{-# NOINLINE allPosts #-}
allPosts :: IORef [Post Server]
allPosts = 
  unsafePerformIO . newIORef $
  [ Post{id = S $ PostId 1, title = "Title 1", slug = "title-1"}
  , Post{id = S $ PostId 2, title = "Title 2", slug = "title-2"}
  , Post{id = S $ PostId 3, title = "Title 3", slug = "title-3"}
  ]

newtype CommentId = CommentId Int
  deriving newtype Eq

newtype CommentSlug = CommentSlug Text
  deriving newtype (Show, Eq, IsString, FromHttpApiData, ToHttpApiData, ToJSON, FromJSON)

instance Js.Debug CommentSlug where
  debug = Js.debug . Js.as @Text

data Comment f
  = Comment
  { id :: f CommentId
  , slug :: f CommentSlug
  , post :: f PostId
  , parent :: f (Maybe CommentId)
  , content :: f Text
  }

{-# NOINLINE allComments #-}
allComments :: IORef [Comment Server]
allComments =
  unsafePerformIO . newIORef $
  [ Comment
      { id = S $ CommentId 1
      , slug = "xxx"
      , parent = S Nothing
      , post = S $ PostId 1
      , content = "Comment 1"
      }
  , Comment
      { id = S $ CommentId 2
      , slug = "yyy"
      , parent = S Nothing
      , post = S $ PostId 1
      , content = "Comment 2"
      }
  , Comment
      { id = S $ CommentId 3
      , slug = "zzz"
      , parent = S Nothing
      , post = S $ PostId 1
      , content = "Comment 3"
      }
  ]
