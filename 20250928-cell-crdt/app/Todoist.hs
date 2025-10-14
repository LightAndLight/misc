{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Todoist
  ( decodeProject
  , projectToState
  , ImportStats (..)
  , Project (..)
  , Task (..)
  , Note (..)
  , Row (..)
  , loadRows
  , rowsToProject
  )
where

import Control.Exception (assert)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Csv (FromRecord)
import qualified Data.Csv as Csv
import Data.Either (partitionEithers)
import Data.Foldable (foldl', toList)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (for)
import GHC.Generics (Generic)
import StateId (StateId, initialStateId)
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)
import Text.Read (readMaybe)
import qualified Todo (State (..))
import qualified Todo.Comment as Todo
  ( Comment (..)
  , CommentId
  , CommentMetadata (..)
  , ReplyId (..)
  , newCommentId
  )
import qualified Todo.Task as Todo (Task (..), TaskId, TaskMetadata (..), newTaskId, renderTaskId)

data Row
  = Row
  { type_ :: !Text
  , content :: !Text
  , description :: !Text
  , is_collapsed :: !Text
  , priority :: !Text
  , indent :: !Text
  , author :: !Text
  , responsible :: !Text
  , date :: !Text
  , date_lang :: !Text
  , timezone :: !Text
  , duration :: !Text
  , duration_unit :: !Text
  , deadline :: !Text
  , deadline_lang :: !Text
  }
  deriving (Show, Eq, Generic, FromRecord)

data Project
  = Project
  { meta :: !(Map Text Text)
  , tasks :: ![Task]
  }
  deriving (Show, Eq)

data Task
  = Task
  { content :: !Text
  , labels :: !(Set Text)
  , description :: !Text
  , priority :: !Int
  , author :: !Text
  , responsible :: !Text
  , {- Task due dates are natural language; annoying to parse.

    , date :: !(Maybe UTCTime)
    , date_lang :: !Text
    -}
    timezone :: !Text
  , {- I don't use durations

    , duration :: !Text
    , duration_unit :: !Text
    -}
    {- I don't use deadlines because they're a paid feature

    , deadline :: !(Maybe UTCTime)
    , deadline_lang :: !Text
    -}
    notes :: ![Note]
  , subtasks :: ![Task]
  }
  deriving (Show, Eq)

data Note
  = Note
  { content :: !Text
  , author :: !Text
  {- Don't need these for now.

  , date :: !(Maybe UTCTime)
  , timezone :: !Text
  -}
  }
  deriving (Show, Eq)

rowsToProject :: [Row] -> Either DecodeError Project
rowsToProject rows = do
  tasks <- rowsToTasks otherRows
  let
    !project =
      Project
        { meta =
            foldl'
              ( \acc row ->
                  let
                    parts = Text.splitOn (fromString "=") row.content
                  in
                    assert (length parts == 2) $
                      Map.insert (parts !! 0) (parts !! 1) acc
              )
              Map.empty
              metaRows
        , tasks
        }
  pure project
  where
    (metaRows, otherRows) = span ((fromString "meta" ==) . (.type_)) rows

data DecodeError = DecodeError {row :: !Row, message :: !Text}
  deriving (Show, Eq)

decodeInt :: Row -> Text -> Either DecodeError Int
decodeInt row input =
  maybe (Left DecodeError{row, message = fromString $ show input ++ " is not an Int"}) Right
    . readMaybe
    $ Text.unpack input

{-
decodeOptional ::
  (Row -> Text -> Either DecodeError a) -> Row -> Text -> Either DecodeError (Maybe a)
decodeOptional f index input
  | Text.null input = pure Nothing
  | otherwise = Just <$> f index input

decodeUTCTime :: Row -> Text -> Either DecodeError UTCTime
decodeUTCTime row input =
  maybe (Left DecodeError{row, message = fromString $ show input ++ " is not a UTCTime"}) Right
    . iso8601ParseM
    $ Text.unpack input
-}

rowsToTasks :: [Row] -> Either DecodeError [Task]
rowsToTasks = (fmap . fmap) snd . go
  where
    go [] = pure []
    go (row : rows)
      | Text.null row.type_ = go rows
      | otherwise =
          assert (row.type_ == fromString "task") $ do
            (content, labels) <- do
              let parts = Text.splitOn (fromString " ") row.content
              let (labels, nonLabels) =
                    partitionEithers $
                      fmap (\input -> maybe (Right input) Left $ Text.stripPrefix (fromString "@") input) parts
              pure (Text.intercalate (fromString " ") nonLabels, Set.fromList labels)

            let (noteRows, rest) = span ((fromString "note" ==) . (.type_)) rows
            priority <- decodeInt row row.priority
            indent <- decodeInt row row.indent
            notes <- traverse rowToNote noteRows
            ( \rest' ->
                let
                  (subtasks, rest'') = span ((indent <) . fst) rest'
                  task =
                    Task
                      { content
                      , labels
                      , description = row.description
                      , priority
                      , author = row.author
                      , responsible = row.responsible
                      , timezone = row.timezone
                      , notes
                      , subtasks = fmap snd subtasks
                      }
                in
                  (indent, task) : rest''
              )
              <$> go rest

rowToNote :: Row -> Either DecodeError Note
rowToNote row =
  pure $!
    Note
      { content = row.content
      , author = row.author
      }

loadRows :: FilePath -> IO [Row]
loadRows path = do
  rows <-
    withFile path ReadMode $ \handle -> do
      input <- LazyByteString.hGetContents handle
      case Csv.decode @Row Csv.HasHeader input of
        Left err -> do
          -- TODO: better error message
          print err
          exitFailure
        Right rows ->
          pure $! rows
  pure $ toList rows

decodeProject :: FilePath -> IO Project
decodeProject path = do
  rows <- loadRows path

  either
    -- TODO: better error message
    (error . ("decode error: " ++) . show)
    pure
    (rowsToProject (toList rows))

data ImportStats
  = ImportStats
  { numTasks :: !Int
  , numNotes :: !Int
  }

countTasks :: Task -> Int
countTasks task = foldl' (\acc -> (acc +) . countTasks) 1 task.subtasks

countNotes :: Task -> Int
countNotes task = foldl' (\acc -> (acc +) . countNotes) (length task.notes) task.subtasks

withTrailingNewline :: Text -> Text
withTrailingNewline input
  | not (Text.null input), Text.last input == '\n' = input
  | otherwise = input <> fromString "\n"

projectToState :: Project -> IO (ImportStats, Todo.State)
projectToState project = do
  now <- getCurrentTime

  let
    mkTasks ::
      [Todoist.Task] ->
      IO
        ( Map Todo.TaskId Todoist.Task
        , [(Todo.TaskId, (Todo.Task (Map StateId), Todo.TaskMetadata))]
        )
    mkTasks tasks = do
      tasks' <-
        for tasks $ \task -> do
          (idMap, subtasks) <- mkTasks task.subtasks

          taskId <- Todo.newTaskId
          let
            description :: Text
            description
              | null subtasks = task.description
              | otherwise =
                  task.description
                    <> fromString "\n\nSubtasks:\n\n"
                    <> fromString
                      ( foldMap
                          ( \(subtaskId, _) ->
                              "* <tsk:task:" <> Todo.renderTaskId subtaskId <> ">\n"
                          )
                          subtasks
                      )
          pure
            ( Map.insert taskId task idMap
            , taskId
            , task
            , Todo.Task
                { status = Map.singleton initialStateId $ fromString "todo"
                , labels = Map.singleton initialStateId task.labels
                , title = Map.singleton initialStateId task.content
                , description = Map.singleton initialStateId $ withTrailingNewline description
                }
            , Todo.TaskMetadata{createdAt = now}
            )

      let !idMap' = foldMap (\(x, _, _, _, _) -> x) tasks'
      let tasks'' = fmap (\(_, taskId, _, task, metadata) -> (taskId, (task, metadata))) tasks'

      pure (idMap', tasks'')

    mkComments ::
      [(Todo.TaskId, Todoist.Task)] ->
      IO [(Todo.CommentId, (Todo.Comment (Map StateId), Todo.CommentMetadata))]
    mkComments tasks =
      fmap concat . for tasks $ \(taskId, task) -> do
        for task.notes $ \note -> do
          commentId <- Todo.newCommentId
          let comment = Todo.Comment{description = Map.singleton initialStateId $ withTrailingNewline note.content}
          let metadata = Todo.CommentMetadata{createdAt = now, replyTo = Todo.ReplyTask taskId}
          pure (commentId, (comment, metadata))

  let numTasks = getSum $ foldMap (Sum . countTasks) project.tasks
  let numNotes = getSum $ foldMap (Sum . countNotes) project.tasks
  (tasksWithIds, tasks) <- mkTasks project.tasks
  comments <- mkComments (Map.toList tasksWithIds)

  let
    !state =
      Todo.State
        { current = Set.singleton initialStateId
        , tasks =
            Map.fromList [(taskId, task) | (taskId, (task, _)) <- tasks]
        , taskMetadata =
            Map.fromList [(taskId, metadata) | (taskId, (_, metadata)) <- tasks]
        , comments =
            Map.fromList [(commentId, comment) | (commentId, (comment, _)) <- comments]
        , commentMetadata =
            Map.fromList [(commentId, metadata) | (commentId, (_, metadata)) <- comments]
        , history = Map.empty
        }

    !importStats = ImportStats{numTasks, numNotes}

  pure (importStats, state)
