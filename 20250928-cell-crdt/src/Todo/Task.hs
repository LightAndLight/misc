{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Task where

import Barbies
import Data.Binary (Binary)
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import GID (GID, gidFromBase32, gidToBase32, newGID)
import Getter (Getter (..))
import StateId (StateId)

newtype TaskId = TaskId {unTaskId :: GID}
  deriving (Show, Eq, Ord)
  deriving newtype (Binary)

renderTaskId :: TaskId -> String
renderTaskId (TaskId gid) = gidToBase32 gid

parseTaskId :: String -> Maybe TaskId
parseTaskId = fmap TaskId . gidFromBase32

newTaskId :: IO TaskId
newTaskId = TaskId <$> newGID

data TaskMetadata
  = TaskMetadata
  { createdAt :: !UTCTime
  }
  deriving (Show, Eq)

data Task f
  = Task
  { status :: !(f Text)
  , labels :: !(f (Set Text))
  , title :: !(f Text)
  , description :: !(f Text)
  }
  deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB, DistributiveB)

deriving instance (forall x. Show x => Show (f x)) => Show (Task f)
deriving instance (forall x. Eq x => Eq (f x)) => Eq (Task f)

taskFieldNames :: Task (Const Text)
taskFieldNames =
  Task
    { status = Const $ fromString "status"
    , labels = Const $ fromString "labels"
    , title = Const $ fromString "title"
    , description = Const $ fromString "description"
    }

taskGetters :: b ~ Task => b (Getter b)
taskGetters =
  Task
    { status = Getter status
    , labels = Getter labels
    , title = Getter title
    , description = Getter description
    }

data TaskDiffError
  = -- | A state ID mentioned in the target 'Task' was not present in the source 'Task'.
    StateIdNotFound
      -- | Field name
      !Text
      !StateId
  | -- | The target 'Task' appears to be 'Pick'ing a value by state ID from the source 'Task', but
    -- the value in the target 'Task' doesn't match the value in the source 'Task'.
    --
    -- It's unclear whether the intention is to 'Pick' an existing value by state ID, or 'Set' the
    -- field to the new, changed value.
    AmbiguousUpdate
      -- | Field name
      !Text
      !StateId
  | -- | The target 'Task' still contains conflicts.
    UnresolvedConflicts
      -- | Field name
      !Text
      -- | Conflicting states
      !(Set StateId)
  | -- | A field in the target 'Task' is missing a value.
    NoValue
      -- | Field name
      !Text
  deriving (Show, Eq)

data Conflicted a
  = Resolved a
  | Conflicted (Map StateId a)

data Update a
  = None
  | Set a
  | Pick StateId
  deriving (Show, Eq, Generic)

taskDiff ::
  Task (Map StateId) ->
  Task Conflicted ->
  Either TaskDiffError (Task Update)
taskDiff task1 task2 =
  btraverseC @Eq
    ( \(Getter getter `Pair` Const fieldName) ->
        case getter task2 of
          Resolved value2 ->
            case Map.toList (getter task1) of
              [(_, value1)] | value1 == value2 -> Right None
              _ -> Right $ Set value2
          Conflicted values ->
            case Map.toList values of
              [] -> Left $ NoValue fieldName
              [(stateId, value2)] ->
                case Map.lookup stateId (getter task1) of
                  Just value1 ->
                    if value1 == value2
                      then Right $ Pick stateId
                      else Left $ AmbiguousUpdate fieldName stateId
                  Nothing -> Left $ StateIdNotFound fieldName stateId
              _ ->
                Left $ UnresolvedConflicts fieldName (Map.keysSet values)
    )
    (bzip taskGetters taskFieldNames)
