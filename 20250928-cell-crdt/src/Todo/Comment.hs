{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Comment where

import Barbies
import Data.Binary (Binary)
import Data.Functor.Const (Const (..))
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import GID (GID, gidFromBase32, gidToBase32, newGID)
import Getter (Getter (..))
import Todo.Task (TaskId, parseTaskId, renderTaskId)

newtype CommentId = CommentId {unCommentId :: GID}
  deriving (Show, Eq, Ord)
  deriving newtype (Binary)

renderCommentId :: CommentId -> String
renderCommentId (CommentId gid) = gidToBase32 gid

parseCommentId :: String -> Maybe CommentId
parseCommentId = fmap CommentId . gidFromBase32

newCommentId :: IO CommentId
newCommentId = CommentId <$> newGID

data ReplyId
  = ReplyTask !TaskId
  | ReplyComment !CommentId
  deriving (Show, Eq)

renderReplyId :: ReplyId -> String
renderReplyId (ReplyTask taskId) = "task:" ++ renderTaskId taskId
renderReplyId (ReplyComment commentId) = "comment:" ++ renderCommentId commentId

parseReplyId :: String -> Maybe ReplyId
parseReplyId input =
  let
    (prefix, suffix) = break (== ':') input
  in
    case prefix of
      "task" ->
        ReplyTask <$> parseTaskId (drop 1 suffix)
      "comment" ->
        ReplyComment <$> parseCommentId (drop 1 suffix)
      _ ->
        Nothing

data CommentMetadata
  = CommentMetadata
  { createdAt :: !UTCTime
  , replyTo :: !ReplyId
  }
  deriving (Show, Eq)

newtype Comment f
  = Comment
  { description :: f Text
  }
  deriving (Generic)
  deriving anyclass (FunctorB, TraversableB, ApplicativeB, ConstraintsB, DistributiveB)

deriving instance (forall x. Show x => Show (f x)) => Show (Comment f)
deriving instance (forall x. Eq x => Eq (f x)) => Eq (Comment f)

commentFieldNames :: Comment (Const Text)
commentFieldNames =
  Comment
    { description = Const $ fromString "description"
    }

commentGetters :: b ~ Comment => b (Getter b)
commentGetters =
  Comment
    { description = Getter description
    }
