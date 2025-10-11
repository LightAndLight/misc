{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Barbies (bfoldMap, bfor_, btraverse, btraverseC, bzip, bzipWith)
import Control.Applicative (Const (..), optional, some, (<**>), (<|>))
import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import qualified Data.Char as Char
import Data.Foldable (fold, foldl', foldlM, for_, traverse_)
import Data.Functor (void)
import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Op (..))
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Functor.Product (Product (..))
import Data.Kind (Type)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (All (..), Alt (..), Any (..))
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy.IO as LazyText
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import MD5 (md5FromBase32)
import qualified Options.Applicative as Options
import Options.Applicative.Help.Pretty ((.$.))
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import qualified System.Environment
import System.Exit (ExitCode (..), exitFailure)
import System.IO (Handle, IOMode (..), hClose, hPutStr, withFile)
import qualified System.Process as Process
import qualified Todo
import qualified Todoist
import Prelude hiding (init)

data Cli
  = Cli {database :: FilePath, command :: Command}

data Command
  = Default
  | Init (Maybe InitFrom)
  | Debug
  | Merge FilePath
  | Task TaskCommand
  | Label LabelCommand

data InitFrom = InitFrom {type_ :: String, path :: FilePath}

data TaskCommand
  = TaskNew
  | TaskList (Maybe (Todo.Task FieldSelectors))
  | TaskView Todo.TaskId
  | TaskEdit Todo.TaskId

taskFieldSelectorsEmpty :: Todo.Task FieldSelectors
taskFieldSelectorsEmpty =
  Todo.Task
    { Todo.status = mempty
    , Todo.labels = mempty
    , Todo.title = mempty
    , Todo.description = mempty
    }

newtype FieldSelectors a = FieldSelectors [FieldSelector a]
  deriving (Semigroup, Monoid)

data FieldSelector :: Type -> Type where
  FieldEq :: Eq a => a -> FieldSelector a
  FieldIn :: Ord a => a -> FieldSelector (Set a)

parseTaskListFilter :: String -> Maybe (Todo.Task FieldSelectors)
parseTaskListFilter input =
  case Attoparsec.parseOnly (taskListFilterParser <* Attoparsec.endOfInput) $ fromString input of
    Left{} -> Nothing
    Right a -> pure a
  where
    textFieldSelectorOpParser :: Attoparsec.Parser (FieldSelector Text)
    textFieldSelectorOpParser = FieldEq <$ Attoparsec.char '=' <*> Attoparsec.takeWhile (/= ',')

    setFieldSelectorOpParser :: Attoparsec.Parser (FieldSelector (Set Text))
    setFieldSelectorOpParser = FieldIn <$ Attoparsec.char '=' <*> Attoparsec.takeWhile (/= ',')

    textFieldSelectorParser :: Text -> Attoparsec.Parser (FieldSelectors Text)
    textFieldSelectorParser field =
      fmap (FieldSelectors . pure) (Attoparsec.string field *> textFieldSelectorOpParser)

    setFieldSelectorParser :: Text -> Attoparsec.Parser (FieldSelectors (Set Text))
    setFieldSelectorParser field =
      fmap (FieldSelectors . pure) (Attoparsec.string field *> setFieldSelectorOpParser)

    taskListFilterParsers :: Todo.Task (Const (Attoparsec.Parser (Todo.Task FieldSelectors)))
    taskListFilterParsers =
      Todo.Task
        { Todo.status =
            Const $
              (\f -> taskFieldSelectorsEmpty{Todo.status = f}) <$> textFieldSelectorParser (fromString "status")
        , Todo.labels =
            Const $
              (\f -> taskFieldSelectorsEmpty{Todo.labels = f}) <$> setFieldSelectorParser (fromString "label")
        , Todo.title =
            Const $
              (\f -> taskFieldSelectorsEmpty{Todo.title = f}) <$> textFieldSelectorParser (fromString "title")
        , Todo.description =
            Const $
              (\f -> taskFieldSelectorsEmpty{Todo.description = f})
                <$> textFieldSelectorParser (fromString "description")
        }

    taskListFilterParser :: Attoparsec.Parser (Todo.Task FieldSelectors)
    taskListFilterParser =
      foldl' (bzipWith (<>)) taskFieldSelectorsEmpty
        <$> Attoparsec.sepBy
          (getAlt (bfoldMap (Alt . getConst) taskListFilterParsers))
          (Attoparsec.char ',')

data LabelCommand
  = LabelList

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      ( Options.long "database"
          <> Options.short 'd'
          <> Options.metavar "PATH"
          <> Options.help "The database on which to operate"
      )
    <*> ( Options.hsubparser
            ( Options.command
                "init"
                ( Options.info
                    initParser
                    (Options.progDesc "Create a database" <> Options.fullDesc <> Options.footerDoc (Just initHeader))
                )
                <> Options.command
                  "debug"
                  (Options.info debugParser (Options.progDesc "Debug a database" <> Options.fullDesc))
                <> Options.command
                  "merge"
                  (Options.info mergeParser (Options.progDesc "Combine two databases" <> Options.fullDesc))
                <> Options.command
                  "task"
                  (Options.info (Task <$> taskParser) (Options.progDesc "Task operations" <> Options.fullDesc))
                <> Options.command
                  "label"
                  (Options.info (Label <$> labelParser) (Options.progDesc "Label operations" <> Options.fullDesc))
            )
            <|> pure Default
        )
  where
    initHeader =
      fromString "Importing:"
        .$. fromString "  Use --from/-f TYPE:PATH to import entries."
        .$. fromString ""
        .$. fromString "  e.g. tsk -d your_database.tsk init -f todoist:/path/to/todoist.csv"
        .$. fromString ""
        .$. fromString "  Supported TYPEs:"
        .$. fromString ""
        .$. fromString
          "  * todoist - Todolist CSV exports (https://www.todoist.com/help/articles/import-or-export-a-project-as-a-csv-file-in-todoist-YC8YvN#h_01HMC0QDKGXBX2TNQ9A559QY0M)"

    initParser =
      Init
        <$> optional
          ( Options.option
              ( Options.maybeReader $ \input -> do
                  let (type_, rest) = break (== ':') input
                  path <-
                    case rest of
                      ':' : path -> pure path
                      _ -> Nothing
                  pure InitFrom{type_, path}
              )
              ( Options.long "from"
                  <> Options.short 'f'
                  <> Options.metavar "TYPE:PATH"
                  <> Options.help "Import entries from a TYPE-formatted database at PATH."
              )
          )

    debugParser =
      pure Debug

    mergeParser =
      Merge <$> Options.strArgument (Options.metavar "PATH" <> Options.help "Database to be merged in")

    taskParser =
      Options.hsubparser
        ( Options.command
            "new"
            (Options.info taskNewParser (Options.progDesc "Create a new task" <> Options.fullDesc))
            <> Options.command
              "list"
              (Options.info taskListParser (Options.progDesc "List tasks" <> Options.fullDesc))
            <> Options.command
              "view"
              (Options.info taskViewParser (Options.progDesc "View a task" <> Options.fullDesc))
            <> Options.command
              "edit"
              (Options.info taskEditParser (Options.progDesc "Edit a task" <> Options.fullDesc))
        )

    taskNewParser =
      pure TaskNew

    taskListParser =
      TaskList
        <$> optional
          ( Options.option
              (Options.maybeReader parseTaskListFilter)
              (Options.long "filter" <> Options.metavar "FILTER" <> Options.help "Task filter expression")
          )

    taskViewParser =
      TaskView
        <$> Options.argument
          (Todo.TaskId <$> Options.maybeReader Todo.gidFromBase32)
          (Options.metavar "ID" <> Options.help "Task to view")

    taskEditParser =
      TaskEdit
        <$> Options.argument
          (Todo.TaskId <$> Options.maybeReader Todo.gidFromBase32)
          (Options.metavar "ID" <> Options.help "Task to edit")

    labelParser =
      Options.hsubparser
        ( Options.command
            "list"
            (Options.info (pure LabelList) (Options.progDesc "List labels" <> Options.fullDesc))
        )

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)

  case command cli of
    Default -> default_ (database cli)
    Init mFrom -> init (database cli) mFrom
    Debug -> debug (database cli)
    Merge other -> merge (database cli) other
    Task TaskNew -> taskNew (database cli)
    Task (TaskList mQuery) -> taskList (database cli) mQuery
    Task (TaskView taskId) -> taskView (database cli) taskId
    Task (TaskEdit taskId) -> taskEdit (database cli) taskId
    Label LabelList -> labelList (database cli)

default_ :: FilePath -> IO ()
default_ path = do
  state <- Todo.stateDeserialise path
  let
    tasks =
      sortOn (Down . Todo.createdAt . snd . snd)
        . Map.toList
        $ Map.intersectionWith (,) (Todo.tasks state) (Todo.taskMetadata state)
  traverse_ renderTask tasks

renderTask :: (Todo.TaskId, (Todo.Task (Map Todo.StateId), Todo.TaskMetadata)) -> IO ()
renderTask (Todo.TaskId taskId, (task, _metadata)) = do
  putStr $ "(" ++ Todo.gidToBase32 taskId ++ ")"
  if Map.size (Todo.title task) > 1 || Map.size (Todo.description task) > 1
    then putStr "* "
    else putStr "  "
  case Map.toList (Todo.title task) of
    [] -> undefined
    (_stateId, title) : _ -> Text.IO.putStrLn title

init :: FilePath -> Maybe InitFrom -> IO ()
init path Nothing = do
  let state = Todo.stateNew
  Todo.stateSerialise path state
  putStrLn $ "Created " ++ path
init dbPath (Just InitFrom{type_, path = importPath}) =
  case type_ of
    "todoist" -> do
      project <- Todoist.decodeProject importPath
      (Todoist.ImportStats numTasks, state) <- Todoist.projectToState project
      Todo.stateSerialise dbPath state
      putStrLn $ "Created " ++ dbPath ++ "\n"
      putStrLn "Imported:"
      putStrLn $ "* " ++ show numTasks ++ " tasks"
    _ -> do
      putStrLn $ "error: unsupported import type: " ++ type_
      exitFailure

debug :: FilePath -> IO ()
debug path = do
  state <- Todo.stateDeserialise path
  print state

merge :: FilePath -> FilePath -> IO ()
merge path other = do
  state <- Todo.stateDeserialise path
  otherState <- Todo.stateDeserialise other
  let state' = Todo.stateMerge state otherState

  let pathNew = path ++ ".new"
  Todo.stateSerialise pathNew state'
  renameFile pathNew path
  putStrLn $ "Merged " ++ other ++ " into " ++ path

  let conflicts = Todo.stateConflicts state'
  unless (Map.null conflicts) $ do
    putStrLn "\nConflicts:\n"
    for_ (Map.toList conflicts) $ \(taskId, task) -> do
      putStrLn $ "(" ++ Todo.renderTaskId taskId ++ ")"
      bfor_ (bzip Todo.taskFieldNames task) $ \(Const fieldName `Pair` value) -> do
        unless (Map.size value <= 1) . putStrLn $ "* " ++ Text.unpack fieldName

taskFileTemplate :: String
taskFileTemplate =
  bfoldMap
    (\(Const fieldName) -> "! " ++ Text.unpack fieldName ++ "\n\n")
    Todo.taskFieldNames

taskNew :: FilePath -> IO ()
taskNew path = do
  editor <- System.Environment.getEnv "EDITOR"
  now <- getCurrentTime

  let taskFile = "drafts/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now ++ ".txt"
  writeFile taskFile taskFileTemplate

  exitCode <- Process.withCreateProcess (Process.proc editor [taskFile]) $ \_mStdin _mStdout _mStderr processHandle -> do
    Process.waitForProcess processHandle
  case exitCode of
    ExitFailure status -> do
      putStrLn $ "Aborted (editor exited with status " ++ show status ++ ")"
      exitFailure
    ExitSuccess -> do
      exists <- doesFileExist taskFile
      if exists
        then do
          task <-
            btraverse
              ( \case
                  Todo.Resolved a ->
                    pure $ Identity a
                  Todo.Conflicted{} -> do
                    -- TODO: recover gracefully
                    putStrLn "error: a new task can't introduce conflicts"
                    exitFailure
              )
              =<< either
                ( \err -> do
                    -- TODO: better error message
                    print err
                    exitFailure
                )
                pure
                . validateIdentifiedTask
              =<< readTask taskFile taskFileParser
          state <- Todo.stateDeserialise path
          state' <- Todo.stateChange (Todo.NewTask task) state

          let pathNew = path ++ ".new"
          Todo.stateSerialise pathNew state'
          renameFile pathNew path
          removeFile taskFile

          -- TODO: print task ID?
          putStrLn $ "Created task " ++ Text.unpack (runIdentity $ Todo.title task)
        else putStrLn "Aborted"

readTask :: FilePath -> Attoparsec.Parser a -> IO a
readTask path parser = do
  input <- LazyText.readFile path
  case Attoparsec.parseOnly (parser <* Attoparsec.endOfInput) input of
    Left{} ->
      error $ "failed to parse task " ++ path
    Right task ->
      pure task

fieldCommentParser :: String -> Attoparsec.Parser (Maybe Todo.StateId)
fieldCommentParser field =
  Attoparsec.char '!'
    *> Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n')
    *> Attoparsec.string (fromString field)
    *> Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n')
    *> optional
      ( Attoparsec.char '('
          *> ( do
                chars <- Attoparsec.takeWhile1 Char.isAlphaNum
                case md5FromBase32 $ Text.unpack chars of
                  Nothing -> fail "invalid state ID"
                  Just md5 -> pure $ Todo.StateId md5
             )
          <* Attoparsec.char ')'
          <* Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n')
      )
    <* Attoparsec.char '\n'

data Identified a
  = Unidentified a
  | Identified Todo.StateId a
  deriving (Show, Eq)

data ValidateIdentifiedError
  = -- | The change proposes multiple distinct values for a field.
    --
    -- Example:
    --
    -- @
    -- ! title
    -- Title one
    --
    -- ! title
    -- Title two
    -- @
    --
    -- A change should not conflict with itself.
    ConflictingResolutions
      -- | Field name
      !Text
  | -- | The change proposes a new value for a field in addition to a conflict set.
    --
    -- Example:
    --
    -- @
    -- ! title (ID)
    -- Title one
    --
    -- ! title
    -- Title two
    -- @
    --
    -- A new value should be proposed, or conflicts should be proposed, but not both.
    UnexpectedResolution
      -- | Field name
      !Text
      -- | Current proposed conflicts
      !(Set Todo.StateId)
  | -- | The change proposes a conflict set in addition to a new value.
    --
    -- Example:
    --
    -- @
    -- ! title
    -- Title two
    --
    -- ! title (ID)
    -- Title one
    -- @
    --
    -- A new value should be proposed, or conflicts should be proposed, but not both.
    UnexpectedConflict
      -- | Field name
      !Text
      -- | Proposed conflict
      !Todo.StateId
  | -- | A field has different values proposed for the same 'StateId'.
    --
    -- Example:
    --
    -- @
    -- ! title (ID)
    -- Title one
    --
    -- ! title (ID)
    -- Title two
    -- @
    --
    -- A change should not conflict with itself.
    InconsistentConflict
      -- | Field name
      !Text
      !Todo.StateId
  | -- | The proposed field change is empty.
    EmptyChange
      -- | Field name
      !Text
  deriving (Show, Eq)

validateIdentifiedTask ::
  Todo.Task (Compose [] Identified) -> Either ValidateIdentifiedError (Todo.Task Todo.Conflicted)
validateIdentifiedTask =
  btraverseC @Eq
    ( \(Const fieldName `Pair` Compose identifieds) -> do
        let
          f Nothing (Unidentified a) =
            pure . Just $ Todo.Resolved a
          f acc@(Just (Todo.Resolved a)) (Unidentified a') = do
            when (a /= a') . Left $ ConflictingResolutions fieldName
            pure acc
          f acc@(Just (Todo.Conflicted as)) (Unidentified a') = do
            unless (a' `elem` as) . Left $ UnexpectedResolution fieldName (Map.keysSet as)
            pure acc
          f Nothing (Identified stateId a) =
            pure . Just $ Todo.Conflicted (Map.singleton stateId a)
          f (Just (Todo.Resolved a)) (Identified stateId a') = do
            when (a /= a') . Left $ UnexpectedConflict fieldName stateId
            pure . Just $ Todo.Conflicted (Map.singleton stateId a')
          f (Just (Todo.Conflicted as)) (Identified stateId a') = do
            for_ (Map.lookup stateId as) $ \a ->
              when (a /= a') $ Left $ InconsistentConflict fieldName stateId
            pure . Just . Todo.Conflicted $ Map.insert stateId a' as

        result <- foldlM f Nothing identifieds
        case result of
          Just conflicted -> pure conflicted
          Nothing -> Left $ EmptyChange fieldName
    )
    . bzip Todo.taskFieldNames

taskFileParser :: Attoparsec.Parser (Todo.Task (Compose [] Identified))
taskFileParser =
  foldl'
    (bzipWith (\(Compose a) (Compose b) -> Compose (a ++ b)))
    emptyTask
    <$> some
      ( statusFieldParser
          <|> labelsFieldParser
          <|> titleFieldParser
          <|> descriptionFieldParser
      )
  where
    emptyTask =
      Todo.Task
        { Todo.status = mempty
        , Todo.labels = mempty
        , Todo.title = mempty
        , Todo.description = mempty
        }

    lineParser = Attoparsec.takeWhile (/= '\n') <* Attoparsec.takeWhile Char.isSpace

    commaSepParser =
      Attoparsec.sepBy
        (Attoparsec.takeWhile (`notElem` ",\n"))
        (Attoparsec.char ',' <* Attoparsec.skipWhile Char.isSpace)
        <* Attoparsec.skipWhile Char.isSpace

    statusFieldParser = do
      mStateId <- fieldCommentParser "status"
      status <- lineParser
      case mStateId of
        Nothing ->
          pure emptyTask{Todo.status = Compose [Unidentified status]}
        Just stateId ->
          pure emptyTask{Todo.status = Compose [Identified stateId status]}

    labelsFieldParser = do
      mStateId <- fieldCommentParser "labels"
      labels <- Set.fromList <$> commaSepParser
      case mStateId of
        Nothing ->
          pure emptyTask{Todo.labels = Compose [Unidentified labels]}
        Just stateId ->
          pure emptyTask{Todo.labels = Compose [Identified stateId labels]}

    titleFieldParser = do
      mStateId <- fieldCommentParser "title"
      title <- lineParser
      case mStateId of
        Nothing ->
          pure emptyTask{Todo.title = Compose [Unidentified title]}
        Just stateId ->
          pure emptyTask{Todo.title = Compose [Identified stateId title]}

    descriptionFieldParser = do
      mStateId <- fieldCommentParser "description"
      description <-
        fmap
          Text.pack
          ( Attoparsec.manyTill
              Attoparsec.anyChar
              (void (lookAhead . Attoparsec.string $ fromString "\n!") <|> Attoparsec.endOfInput)
              <* optional (Attoparsec.char '\n')
          )
      case mStateId of
        Nothing ->
          pure emptyTask{Todo.description = Compose [Unidentified description]}
        Just stateId ->
          pure emptyTask{Todo.description = Compose [Identified stateId description]}

taskRenderValues :: Todo.Task (Op String)
taskRenderValues =
  Todo.Task
    { Todo.status = Op Text.unpack
    , Todo.labels = Op (intercalate ", " . fmap Text.unpack . Set.toAscList)
    , Todo.title = Op Text.unpack
    , Todo.description = Op Text.unpack
    }

writeTask :: Handle -> Todo.Task (Map Todo.StateId) -> IO ()
writeTask handle task =
  hPutStr handle . intercalate "\n\n" $
    bfoldMap
      ( \(Const fieldName `Pair` Op renderValue `Pair` values) ->
          case Map.toList values of
            [(_, value)] ->
              [ "! "
                  ++ Text.unpack fieldName
                  ++ "\n"
                  ++ renderValue value
              ]
            values' ->
              fmap
                ( \(stateId, value) ->
                    "! " ++ Text.unpack fieldName ++ " (" ++ Todo.renderStateId stateId ++ ")\n" ++ renderValue value
                )
                values'
      )
      (bzip (bzip Todo.taskFieldNames taskRenderValues) task)

renderUpdateTask :: Todo.Task (Map Todo.StateId) -> Todo.Task Todo.Update -> String
renderUpdateTask task update =
  foldMap (++ "\n\n") $
    bfoldMap
      ( \(Const fieldName `Pair` Op renderValue `Pair` Todo.Getter getter) ->
          renderField (Text.unpack fieldName) (Text.pack . renderValue) getter
      )
      (bzip (bzip Todo.taskFieldNames taskRenderValues) Todo.taskGetters)
  where
    prependLines prefix =
      intercalate "\n" . fmap ((prefix ++) . Text.unpack) . Text.splitOn (fromString "\n")

    renderField :: String -> (a -> Text) -> (forall f. Todo.Task f -> f a) -> [String]
    renderField fieldName renderValue getter =
      case getter update of
        Todo.None ->
          []
        Todo.Set a ->
          foldMap
            ( \(stateId', a') ->
                [ "- ! "
                    ++ fieldName
                    ++ " ("
                    ++ Todo.renderStateId stateId'
                    ++ ")\n"
                    ++ prependLines "- " (renderValue a')
                ]
            )
            (Map.toList $ getter task)
            ++ [ "+ ! "
                  ++ fieldName
                  ++ "\n"
                  ++ prependLines "+ " (renderValue a)
               ]
        Todo.Pick stateId ->
          foldMap
            ( \(stateId', a') ->
                if stateId == stateId'
                  then
                    [ "  ! "
                        ++ fieldName
                        ++ " ("
                        ++ Todo.renderStateId stateId'
                        ++ ")\n"
                        ++ prependLines "  " (renderValue a')
                    ]
                  else
                    [ "- ! "
                        ++ fieldName
                        ++ " ("
                        ++ Todo.renderStateId stateId'
                        ++ ")\n"
                        ++ prependLines "- " (renderValue a')
                    ]
            )
            (Map.toList $ getter task)

taskList :: FilePath -> Maybe (Todo.Task FieldSelectors) -> IO ()
taskList path mFilter = do
  state <- Todo.stateDeserialise path
  let filteredTasks = Map.filter (maybe (const True) filterTask mFilter) (Todo.tasks state)
  let
    tasks =
      sortOn (Down . Todo.createdAt . snd . snd)
        . Map.toList
        $ Map.intersectionWith (,) filteredTasks (Todo.taskMetadata state)
  traverse_ renderTask tasks
  where
    filterTask :: Todo.Task FieldSelectors -> Todo.Task (Map Todo.StateId) -> Bool
    filterTask taskSelectors task =
      getAny $
        bfoldMap
          ( \(FieldSelectors selectors `Pair` values) ->
              Any $
                not (null selectors) && all (\selector -> any (matches selector) values) selectors
          )
          (bzip taskSelectors task)

    matches :: FieldSelector a -> a -> Bool
    matches (FieldEq a) b = a == b
    matches (FieldIn a) b = a `Set.member` b

taskView :: FilePath -> Todo.TaskId -> IO ()
taskView path taskId = do
  state <- Todo.stateDeserialise path
  case Map.lookup taskId (Todo.tasks state) of
    Nothing -> do
      putStrLn "Task not found"
      exitFailure
    Just task -> do
      pager <- System.Environment.getEnv "PAGER"

      let
        pagerArgs =
          [ {-
            This is the `less` flag to display text starting at the top of the screen instead of
            the bottom. Slight concern about portability.
            -}
            "-c"
          ]
      let process = (Process.proc pager pagerArgs){Process.std_in = Process.CreatePipe}
      exitCode <- Process.withCreateProcess process $ \mStdin _mStdout _mStderr processHandle -> do
        let hStdin = fromMaybe (error "failed to open stdin") mStdin
        writeTask hStdin task
        hClose hStdin

        Process.waitForProcess processHandle
      case exitCode of
        ExitFailure status -> do
          putStrLn $ "Aborted (pager exited with status " ++ show status ++ ")"
          exitFailure
        ExitSuccess ->
          pure ()

taskEdit :: FilePath -> Todo.TaskId -> IO ()
taskEdit path taskId = do
  state <- Todo.stateDeserialise path
  case Map.lookup taskId (Todo.tasks state) of
    Nothing -> do
      putStrLn "Task not found"
      exitFailure
    Just task -> do
      editor <- System.Environment.getEnv "EDITOR"
      createDirectoryIfMissing True "drafts"

      now <- getCurrentTime

      let taskFile =
            "drafts/"
              ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
              ++ "-"
              ++ Todo.gidToBase32 (Todo.unTaskId taskId)
              ++ ".txt"
      let taskFileBase = taskFile ++ ".base"
      withFile taskFileBase WriteMode $ \handle ->
        writeTask handle task
      copyFile taskFileBase taskFile

      ( do
          exitCode <- Process.withCreateProcess (Process.proc editor [taskFile]) $ \_mStdin _mStdout _mStderr processHandle -> do
            Process.waitForProcess processHandle
          case exitCode of
            ExitFailure status -> do
              putStrLn $ "Aborted (editor exited with status " ++ show status ++ ")"
              exitFailure
            ExitSuccess -> do
              base <- readFile taskFileBase
              new <- readFile taskFile
              if base == new
                then putStrLn "No change"
                else do
                  task' <-
                    -- TODO: nicer error message
                    either (\err -> print err *> exitFailure) pure
                      . validateIdentifiedTask
                      =<< readTask taskFile taskFileParser
                  updateTask <-
                    case Todo.taskDiff task task' of
                      Right updateTask ->
                        pure updateTask
                      Left err -> do
                        -- TODO: nicer error message
                        print err
                        exitFailure

                  if getAll $ bfoldMap (All . (\case Todo.None -> True; _ -> False)) updateTask
                    then putStrLn "No change"
                    else do
                      putStrLn "Changes:\n"

                      putStr $ renderUpdateTask task updateTask
                      state' <- Todo.stateChange Todo.UpdateTask{Todo.taskId, Todo.updateTask} state
                      let pathNew = path ++ ".new"
                      Todo.stateSerialise pathNew state'
                      renameFile pathNew path
                      removeFile taskFile
                      putStrLn $ "Updated task " ++ Todo.gidToBase32 (Todo.unTaskId taskId)
        )
        `finally` removeFile taskFileBase

labelList :: FilePath -> IO ()
labelList path = do
  state <- Todo.stateDeserialise path
  let labels = foldMap (fold . Todo.labels) $ Todo.tasks state
  for_ (Set.toAscList labels) $ \label -> do
    Text.IO.putStrLn label
