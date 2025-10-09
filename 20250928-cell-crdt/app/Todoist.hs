{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import System.IO (IOMode (..), withFile)
import Text.Read (readMaybe)
import qualified Todo

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
  , indent :: !Int
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
  }
  deriving (Show, Eq)

data Note
  = Note
  { content :: !Text
  , author :: !Text
  , date :: !(Maybe UTCTime)
  , timezone :: !Text
  }
  deriving (Show, Eq)

rowsToProject :: [Row] -> Either DecodeError Project
rowsToProject (zip [0 ..] -> rows) = do
  tasks <- rowsToTasks otherRows
  pure $!
    Project
      { meta =
          foldl'
            ( \acc row ->
                let
                  parts = Text.splitOn (fromString "=") (snd row).content
                in
                  assert (length parts == 2) $
                    Map.insert (parts !! 0) (parts !! 1) acc
            )
            Map.empty
            metaRows
      , tasks
      }
  where
    (metaRows, otherRows) = span ((fromString "meta" ==) . (.type_) . snd) rows

data DecodeError = DecodeError {index :: !Int, message :: !Text}
  deriving (Show, Eq)

decodeInt :: Int -> Text -> Either DecodeError Int
decodeInt index input =
  maybe (Left DecodeError{index, message = fromString $ show input ++ " is not an Int"}) Right
    . readMaybe
    $ Text.unpack input

decodeOptional ::
  (Int -> Text -> Either DecodeError a) -> Int -> Text -> Either DecodeError (Maybe a)
decodeOptional f index input
  | Text.null input = pure Nothing
  | otherwise = Just <$> f index input

decodeUTCTime :: Int -> Text -> Either DecodeError UTCTime
decodeUTCTime index input =
  maybe (Left DecodeError{index, message = fromString $ show input ++ " is not a UTCTime"}) Right
    . iso8601ParseM
    $ Text.unpack input

rowsToTasks :: [(Int, Row)] -> Either DecodeError [Task]
rowsToTasks = go
  where
    go [] = pure []
    go ((index, row) : rows)
      | Text.null row.type_ = go rows
      | otherwise =
          assert (row.type_ == fromString "task") $ do
            (content, labels) <- do
              let parts = Text.splitOn (fromString " ") row.content
              let (labels, nonLabels) =
                    partitionEithers $
                      fmap (\input -> maybe (Right input) Left $ Text.stripPrefix (fromString "@") input) parts
              pure (Text.intercalate (fromString " ") nonLabels, Set.fromList labels)

            let (noteRows, rest) = span ((fromString "note" ==) . (.type_) . snd) rows
            priority <- decodeInt index row.priority
            indent <- decodeInt index row.indent
            notes <- traverse (uncurry rowToNote) noteRows
            (:)
              Task
                { content
                , labels
                , description = row.description
                , priority
                , indent
                , author = row.author
                , responsible = row.responsible
                , timezone = row.timezone
                , notes
                }
              <$> go rest

rowToNote :: Int -> Row -> Either DecodeError Note
rowToNote index row = do
  date <- decodeOptional decodeUTCTime index row.date
  pure $!
    Note
      { content = row.content
      , author = row.author
      , date
      , timezone = row.timezone
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
  }

projectToState :: Project -> IO (ImportStats, Todo.State)
projectToState project = do
  now <- getCurrentTime
  (tasks, taskMetadata) <- fmap unzip . for project.tasks $ \task -> do
    taskId <- Todo.newTaskId
    pure
      (
        ( taskId
        , Todo.Task
            { status = Map.singleton Todo.initialStateId $ fromString "todo"
            , labels = Map.singleton Todo.initialStateId task.labels
            , title = Map.singleton Todo.initialStateId task.content
            , description = Map.singleton Todo.initialStateId task.description
            }
        )
      , (taskId, Todo.Metadata{createdAt = now})
      )

  let
    !state =
      Todo.State
        { current = Set.singleton Todo.initialStateId
        , tasks = Map.fromList tasks
        , taskMetadata = Map.fromList taskMetadata
        , history = Map.empty
        }

    !importStats = ImportStats{numTasks = length tasks}

  pure (importStats, state)
