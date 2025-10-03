module Main where

import Control.Applicative ((<**>), (<|>))
import qualified Options.Applicative as Options
import qualified Todo
import qualified System.Environment
import Data.Time.Clock (getCurrentTime)
import qualified System.Process as Process
import System.Directory (doesFileExist, renameFile, removeFile)
import System.Exit (ExitCode(..), exitFailure)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Functor.Identity (Identity)
import qualified Data.Attoparsec.Text.Lazy as Attoparsec
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Text as Text
import Todo (Task(..))

data Cli
  = Cli{ database :: FilePath, command :: Command }

data Command
  = Create
  | Debug
  | New NewCommand

data NewCommand
  = NewTask

cliParser :: Options.Parser Cli
cliParser =
  Cli <$>
    Options.strOption (Options.long "database" <> Options.short 'd' <> Options.metavar "PATH" <> Options.help "The database on which to operate") <*>
  Options.hsubparser
    ( Options.command "create" (Options.info createParser (Options.progDesc "Create a database" <> Options.fullDesc)) <>
        Options.command "debug" (Options.info debugParser (Options.progDesc "Debug a database" <> Options.fullDesc)) <>
        Options.command "new" (Options.info newParser (Options.progDesc "Add a new resource" <> Options.fullDesc))
    )
  where
    createParser =
      pure Create

    debugParser =
      pure Debug

    newParser =
      New <$>
      Options.hsubparser
        (Options.command "task" (Options.info newTaskParser (Options.progDesc "Create a new task" <> Options.fullDesc)))

    newTaskParser =
      pure NewTask

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)

  case command cli of
    Create -> create (database cli)
    Debug -> debug (database cli)
    New NewTask -> newTask (database cli) 

create :: FilePath -> IO ()
create path = do
  let state = Todo.stateNew
  Todo.stateSerialise path state
  putStrLn $ "Created " ++ path

debug :: FilePath -> IO ()
debug path = do
  state <- Todo.stateDeserialise path
  print state

newTask :: FilePath -> IO ()
newTask path = do
  editor <- System.Environment.getEnv "EDITOR"
  now <- getCurrentTime
  let taskFile = "drafts/" ++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now ++ ".txt"
  exitCode <- Process.withCreateProcess (Process.proc editor [taskFile]) $ \mStdin mStdout mStderr processHandle -> do
    Process.waitForProcess processHandle
  case exitCode of
    ExitFailure status -> do
      putStrLn $ "Aborted (editor exited with status " ++ show status ++ ")"
      exitFailure
    ExitSuccess -> do
      exists <- doesFileExist taskFile
      if exists
        then do
          task <- parseTask taskFile
          state <- Todo.stateDeserialise path
          state' <- Todo.stateChange (Todo.NewTask task) state
          let pathNew = path ++ ".new"
          Todo.stateSerialise pathNew state'
          renameFile pathNew path
          removeFile taskFile
          putStrLn _createdTask
        else putStrLn "Aborted"

parseTask :: FilePath -> IO (Todo.Task Identity)
parseTask path = do
  input <- LazyText.readFile path
  case Attoparsec.parseOnly taskParser input of
    Left{} ->
      error $ "failed to parse task " ++ path
    Right (title, description) ->
      pure Todo.Task{ title = pure title, description = pure description }
  where
    taskParser :: Attoparsec.Parser (Text, Text)
    taskParser =
      (,) <$>
      Attoparsec.takeWhile (/= '\n') <*>
      (Attoparsec.takeWhile1 (== '\n') *> Attoparsec.takeWhile (const True) <|> pure Text.empty)
