{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Options
import qualified Todo
import qualified System.Environment
import Data.Time.Clock (getCurrentTime)
import qualified System.Process as Process
import System.Directory (doesFileExist, renameFile, removeFile, createDirectoryIfMissing)
import System.Exit (ExitCode(..), exitFailure)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Functor.Identity (Identity, runIdentity)
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text as Text
import Prelude hiding (init)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.List (sortOn)
import qualified Data.Text.IO as Text.IO
import Data.Map (Map)
import Data.Monoid (All(..))
import Barbies (bfoldMap)
import Data.Ord (Down(..))

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
        [(_stateId, title)] -> Text.IO.putStrLn title
        _titles -> error "TODO: title conflict"

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
          task <- readTask taskFile
          state <- Todo.stateDeserialise path
          state' <- Todo.stateChange (Todo.NewTask task) state
          let pathNew = path ++ ".new"
          Todo.stateSerialise pathNew state'
          renameFile pathNew path
          removeFile taskFile

          -- TODO: print task ID?
          putStrLn $ "Created task " ++ Text.unpack (runIdentity $ Todo.title task)
        else putStrLn "Aborted"

readTask :: FilePath -> IO (Todo.Task Identity)
readTask path = do
  input <- LazyText.readFile path
  case Attoparsec.parseOnly taskParser input of
    Left{} ->
      error $ "failed to parse task " ++ path
    Right (title, description) ->
      pure Todo.Task{ Todo.title = pure title, Todo.description = pure description }
  where
    taskParser :: Attoparsec.Parser (Text, Text)
    taskParser =
      (,) <$>
      Attoparsec.takeWhile (/= '\n') <*>
      (Attoparsec.takeWhile1 (== '\n') *> Attoparsec.takeWhile (const True) <|> pure Text.empty)

writeTask :: FilePath -> Todo.Task (Map Todo.StateId) -> IO ()
writeTask path task =
  writeFile path $
  (case Map.toList (Todo.title task) of
    [(_, title)] -> Text.unpack title ++ "\n\n"
    _ -> error "TODO: print title conflict"
  ) ++
  (case Map.toList (Todo.description task) of
    [(_, description)] -> Text.unpack description
    _ -> error "TODO: print description conflict"
  )

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

      let taskFile = "drafts/" ++ Todo.gidToBase32 (Todo.unTaskId taskId) ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now ++ ".txt"
      writeTask taskFile task

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
              task' <- readTask taskFile
              let updateTask = Todo.taskDiff task task'

              if getAll $ bfoldMap (All . (\case; Todo.None -> True; _ -> False)) updateTask
                then putStrLn "No change"
                else do
                  state' <- Todo.stateChange Todo.UpdateTask{ Todo.taskId, Todo.updateTask } state
                  let pathNew = path ++ ".new"
                  Todo.stateSerialise pathNew state'
                  renameFile pathNew path
                  putStrLn $ "Updated task " ++ Todo.gidToBase32 (Todo.unTaskId taskId)

              removeFile taskFile
            else putStrLn "Aborted"
