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
import GID (GID, gidToBase32, newGID)
import Getter (Getter (..))
import Todo.Task (TaskId)

newtype CommentId = CommentId {unCommentId :: GID}
  deriving (Show, Eq, Ord)
  deriving newtype (Binary)

renderCommentId :: CommentId -> String
renderCommentId (CommentId gid) = gidToBase32 gid

newCommentId :: IO CommentId
newCommentId = CommentId <$> newGID

data ReplyId
  = ReplyTask !TaskId
  | ReplyComment !CommentId
  deriving (Show, Eq)

data CommentMetadata
  = CommentMetadata
  { createdAt :: !UTCTime
  , replyTo :: !ReplyId
  }
  deriving (Show, Eq)

data Comment f
  = Comment
  { description :: !(f Text)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB, DistributiveB)

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
