{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Applicative ((<**>), (<|>), some, optional, Const (..))
import qualified Options.Applicative as Options
import qualified Todo
import qualified System.Environment
import Data.Time.Clock (getCurrentTime)
import qualified System.Process as Process
import System.Directory (doesFileExist, renameFile, removeFile, createDirectoryIfMissing, copyFile)
import System.Exit (ExitCode(..), exitFailure)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text as Text
import Prelude hiding (init)
import Data.Foldable (traverse_, foldl', foldlM, for_)
import qualified Data.Map as Map
import Data.List (sortOn, intercalate)
import qualified Data.Text.IO as Text.IO
import Data.Map (Map)
import Data.Monoid (All(..))
import Barbies (bfoldMap, bzipWith, btraverseC, bzip)
import Data.Ord (Down(..))
import Data.String (fromString)
import qualified Data.Char as Char
import Data.Attoparsec.Combinator (lookAhead)
import Control.Exception (finally)
import Data.Functor.Compose (Compose (..))
import Control.Monad (when, unless)
import Data.Set (Set)
import Data.Functor.Product (Product(..))
import Data.Functor (void)

data Cli
  = Cli{ database :: FilePath, command :: Command }

data Command
  = Default
  | Init
  | Debug
  | Merge FilePath
  | New NewCommand
  | Edit EditCommand

data NewCommand
  = NewTask

data EditCommand
  = EditTask Todo.TaskId

cliParser :: Options.Parser Cli
cliParser =
  Cli <$>
    Options.strOption (Options.long "database" <> Options.short 'd' <> Options.metavar "PATH" <> Options.help "The database on which to operate") <*>
  (Options.hsubparser
    ( Options.command "init" (Options.info initParser (Options.progDesc "Create a database" <> Options.fullDesc)) <>
        Options.command "debug" (Options.info debugParser (Options.progDesc "Debug a database" <> Options.fullDesc)) <>
        Options.command "merge" (Options.info mergeParser (Options.progDesc "Combine two databases" <> Options.fullDesc)) <>
        Options.command "new" (Options.info newParser (Options.progDesc "Add a new resource" <> Options.fullDesc)) <>
        Options.command "edit" (Options.info editParser (Options.progDesc "Edit a resource" <> Options.fullDesc))
    ) <|>
    pure Default
  )
  where
    initParser =
      pure Init

    debugParser =
      pure Debug

    mergeParser =
      Merge <$> Options.strArgument (Options.metavar "PATH" <> Options.help "Database to be merged in")

    newParser =
      New <$>
      Options.hsubparser
        (Options.command "task" (Options.info newTaskParser (Options.progDesc "Create a new task" <> Options.fullDesc)))

    newTaskParser =
      pure NewTask

    editParser =
      Edit <$>
      Options.hsubparser
        (Options.command "task" (Options.info editTaskParser (Options.progDesc "Edit a task" <> Options.fullDesc)))

    editTaskParser =
      EditTask <$>
      Options.argument (Todo.TaskId <$> Options.maybeReader Todo.gidFromBase32) (Options.metavar "ID" <> Options.help "Task to edit")

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)

  case command cli of
    Default -> default_ (database cli)
    Init -> init (database cli)
    Debug -> debug (database cli)
    Merge other -> merge (database cli) other
    New NewTask -> newTask (database cli) 
    Edit (EditTask taskId) -> editTask (database cli) taskId

default_ :: FilePath -> IO ()
default_ path = do
  state <- Todo.stateDeserialise path
  traverse_ renderTask .
    sortOn (Down . Todo.createdAt . snd . snd) .
    Map.toList $
    Map.intersectionWith (,) (Todo.tasks state) (Todo.taskMetadata state)
  where
    renderTask :: (Todo.TaskId, (Todo.Task (Map Todo.StateId), Todo.Metadata)) -> IO ()
    renderTask (Todo.TaskId taskId, (task, _metadata)) = do
      putStr $ "(" ++ Todo.gidToBase32 taskId ++ ")"
      if Map.size (Todo.title task) > 1 || Map.size (Todo.description task) > 1
        then putStr "* "
        else putStr "  "
      case Map.toList (Todo.title task) of
        [] -> undefined
        (_stateId, title) : _ -> Text.IO.putStrLn title

init :: FilePath -> IO ()
init path = do
  let state = Todo.stateNew
  Todo.stateSerialise path state
  putStrLn $ "Created " ++ path

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

newTask :: FilePath -> IO ()
newTask path = do
  editor <- System.Environment.getEnv "EDITOR"
  now <- getCurrentTime
  let taskFile = "drafts/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now ++ ".txt"
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
          task  <- readTask taskFile resolvedTaskParser
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
  Attoparsec.char '!' *>
  Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n') *>
  Attoparsec.string (fromString field) *>
  Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n') *>
  optional (
    Attoparsec.char '(' *>
    (do
      chars <- Attoparsec.takeWhile1 Char.isAlphaNum
      case Todo.md5FromBase32 $ Text.unpack chars of
        Nothing -> fail "invalid state ID"
        Just md5 -> pure $ Todo.StateId md5
    ) <*
    Attoparsec.char ')' <*
    Attoparsec.takeWhile (\c -> Char.isSpace c && c /= '\n')
  ) <*
  Attoparsec.char '\n'

titleParser :: Attoparsec.Parser Text
titleParser = Attoparsec.takeWhile (/= '\n') <* Attoparsec.takeWhile Char.isSpace

data Identified a
  = Unidentified a
  | Identified Todo.StateId a
  deriving (Show, Eq)

data ValidateIdentifiedError
  -- | The change proposes multiple distinct values for a field.
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
  = ConflictingResolutions
      -- | Field name
      !Text

  -- | The change proposes a new value for a field in addition to a conflict set.
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
  | UnexpectedResolution
      -- | Field name
      !Text
      -- | Current proposed conflicts
      !(Set Todo.StateId)

  -- | The change proposes a conflict set in addition to a new value.
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
  | UnexpectedConflict
      -- | Field name
      !Text
      -- | Proposed conflict
      !Todo.StateId

  -- | A field has different values proposed for the same 'StateId'.
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
  | InconsistentConflict
      -- | Field name
      !Text
      !Todo.StateId

  -- | The proposed field change is empty.
  | EmptyChange
      -- | Field name
      !Text
  deriving (Show, Eq)

validateIdentifiedTask :: Todo.Task (Compose [] Identified) -> Either ValidateIdentifiedError (Todo.Task Todo.Conflicted)
validateIdentifiedTask =
  btraverseC @Eq
    (\(Const fieldName `Pair` Compose identifieds) -> do
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
    ) .
    bzip Todo.taskFieldNames

identifiedTaskParser :: Attoparsec.Parser (Todo.Task (Compose [] Identified))
identifiedTaskParser =
  foldl' (bzipWith (\(Compose a) (Compose b) -> Compose (a ++ b))) Todo.Task{ Todo.title = mempty, Todo.description = mempty } <$>
  some
    ((do
        mStateId <- fieldCommentParser "title"
        title <- titleParser
        case mStateId of
          Nothing ->
            pure Todo.Task{ Todo.title = Compose [Unidentified title], Todo.description = mempty }
          Just stateId ->
            pure Todo.Task{ Todo.title = Compose [Identified stateId title], Todo.description = mempty }
      ) <|>
      (do
        mStateId <- fieldCommentParser "description"
        description <-
          fmap
            Text.pack
            (Attoparsec.manyTill
              Attoparsec.anyChar
              (void (lookAhead . Attoparsec.string $ fromString "\n!") <|> Attoparsec.endOfInput) <*
              optional (Attoparsec.char '\n')
            )
        case mStateId of
          Nothing ->
            pure Todo.Task{ Todo.title = mempty, Todo.description = Compose [Unidentified description] }
          Just stateId ->
            pure Todo.Task{ Todo.title = mempty, Todo.description = Compose [Identified stateId description] }
      )
    )

resolvedTaskParser :: Attoparsec.Parser (Todo.Task Identity)
resolvedTaskParser = do
  title <- titleParser
  description <- Attoparsec.takeWhile (const True)
  pure Todo.Task{ Todo.title = pure title, Todo.description = pure description }

writeTask :: FilePath -> Todo.Task (Map Todo.StateId) -> IO ()
writeTask path task =
  writeFile path . intercalate "\n\n" $
  (case Map.toList (Todo.title task) of
    [(_, title)] ->
      [ (if hasConflicts then "! title\n" else "") ++
        Text.unpack title
      ]
    titles ->
      fmap
        (\(stateId, title) -> "! title (" ++ Todo.renderStateId stateId ++ ")\n" ++ Text.unpack title)
        titles
  ) ++
  (case Map.toList (Todo.description task) of
    [(_, description)] ->
      [ (if hasConflicts then "! description\n" else "") ++
        Text.unpack description
      ]
    descriptions ->
      fmap
        (\(stateId, description) -> "! description (" ++ Todo.renderStateId stateId ++ ")\n" ++ Text.unpack description)
        descriptions
  )
  where
    hasConflicts =
      Map.size (Todo.title task) > 1 ||
      Map.size (Todo.description task) > 1

renderUpdateTask :: Todo.Task (Map Todo.StateId) -> Todo.Task Todo.Update -> String
renderUpdateTask task update =
  foldMap (++ "\n\n") $
  renderTextField "title" Todo.title ++
  renderTextField "description" Todo.description
  where
    prependLines prefix =
      intercalate "\n" . fmap ((prefix ++) . Text.unpack) . Text.splitOn (fromString "\n")

    renderTextField :: String -> (forall f. Todo.Task f -> f Text) -> [String]
    renderTextField fieldName getter =
      case getter update of
        Todo.None ->
          []
        Todo.Set a ->
          foldMap
            (\(stateId', a') ->
              [ "- ! " ++ fieldName ++ " (" ++ Todo.renderStateId stateId' ++ ")\n" ++
                prependLines "- " a'
              ]
            )
            (Map.toList $ getter task) ++
          [ "+ ! " ++ fieldName ++ "\n" ++
            prependLines "+ " a
          ]
        Todo.Pick stateId ->
          foldMap
            (\(stateId', a') ->
              if stateId == stateId'
              then
                [ "  ! " ++ fieldName ++ " (" ++ Todo.renderStateId stateId' ++ ")\n" ++
                  prependLines "  " a'
                ]
              else
                [ "- ! " ++ fieldName ++ " (" ++ Todo.renderStateId stateId' ++ ")\n" ++
                  prependLines "- " a'
                ]
            )
            (Map.toList $ getter task)

editTask :: FilePath -> Todo.TaskId -> IO ()
editTask path taskId = do
  state <- Todo.stateDeserialise path
  case Map.lookup taskId (Todo.tasks state) of
    Nothing -> do
      putStrLn "Task not found"
      exitFailure
    Just task -> do
      editor <- System.Environment.getEnv "EDITOR"
      createDirectoryIfMissing True "drafts"

      now <- getCurrentTime

      let taskFile = "drafts/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now ++ "-" ++ Todo.gidToBase32 (Todo.unTaskId taskId) ++ ".txt"
      let taskFileBase = taskFile ++ ".base"
      writeTask taskFileBase task
      copyFile taskFileBase taskFile

      (do
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
                result <- readTask taskFile (Left <$> identifiedTaskParser <|> Right <$> resolvedTaskParser)
                updateTask <-
                  case result of
                    Left identifiedTask -> do
                      conflictedTask <-
                        -- TODO: nicer error message
                        either (\err -> print err *> exitFailure) pure $
                        validateIdentifiedTask identifiedTask
                      case Todo.taskDiffConflicted task conflictedTask of
                        Right updateTask -> pure updateTask
                        Left err -> do
                          -- TODO: nicer error message
                          print err
                          exitFailure
                    Right task' -> do
                      pure $ Todo.taskDiff task task'

                if getAll $ bfoldMap (All . (\case; Todo.None -> True; _ -> False)) updateTask
                  then putStrLn "No change"
                  else do
                    putStrLn "Changes:\n"

                    putStr $ renderUpdateTask task updateTask
                    state' <- Todo.stateChange Todo.UpdateTask{ Todo.taskId, Todo.updateTask } state
                    let pathNew = path ++ ".new"
                    Todo.stateSerialise pathNew state'
                    renameFile pathNew path
                    removeFile taskFile
                    putStrLn $ "Updated task " ++ Todo.gidToBase32 (Todo.unTaskId taskId)
        ) `finally`
          removeFile taskFileBase
