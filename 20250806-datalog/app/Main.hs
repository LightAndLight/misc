{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Codec.CBOR.JSON (decodeValue, encodeValue)
import Codec.CBOR.Read (deserialiseFromBytes)
import qualified Codec.CBOR.Write as CBOR
import Control.Applicative ((<**>), (<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_, race_, replicateConcurrently)
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TQueue (newTQueueIO, readTQueue, writeTQueue)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (guard)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as LazyText
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Database
  ( Row (..)
  , diskDatabaseEmpty
  , diskDatabaseInsertRows
  , loadDiskDatabase
  , storeDiskDatabase
  )
import Eval (eval_seminaive, formatChange)
import qualified Options.Applicative as Options
import qualified Parse
import Streaming.Chars.Text (StreamText (..))
import Syntax (Constant (..))
import System.Directory (listDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeExtension, (</>))
import System.Process (readProcessWithExitCode)
import qualified Text.Diagnostic as Diagnostic
import Text.Diagnostic.Sage (parseError)
import Text.Parser.Sage (getParsersSage)
import Text.Sage (parse)

data Cli
  = Scrape FilePath
  | Db FilePath FilePath
  | Query FilePath Text

cliParser :: Options.Parser Cli
cliParser =
  Options.subparser
    ( Options.command
        "scrape"
        ( Options.info
            (scrapeParser <**> Options.helper)
            (Options.progDesc "Save all Nix store derivations into a single CBOR file" <> Options.fullDesc)
        )
        <> Options.command
          "db"
          ( Options.info
              (dbParser <**> Options.helper)
              ( Options.progDesc "Convert the Nix store derivations file into a proper database" <> Options.fullDesc
              )
          )
        <> Options.command
          "query"
          ( Options.info
              (queryParser <**> Options.helper)
              (Options.progDesc "Query a database" <> Options.fullDesc)
          )
    )
  where
    scrapeParser =
      Scrape
        <$> Options.strOption
          ( Options.short 'o'
              <> Options.long "output"
              <> Options.value "nix-store.cbor"
              <> Options.showDefault
              <> Options.metavar "FILE"
              <> Options.help "Destination file"
          )

    dbParser =
      Db
        <$> Options.strArgument
          (Options.metavar "INPUT" <> Options.help "Source file")
        <*> Options.strOption
          ( Options.short 'o'
              <> Options.long "output"
              <> Options.value "database.bin"
              <> Options.showDefault
              <> Options.metavar "FILE"
              <> Options.help "Destination file"
          )

    queryParser =
      Query
        <$> Options.strOption
          ( Options.long "db"
              <> Options.value "database.bin"
              <> Options.showDefault
              <> Options.metavar "FILE"
              <> Options.help "Database file"
          )
        <*> Options.strArgument
          ( Options.metavar "QUERY"
              <> Options.help "Query to run"
          )

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  case cli of
    Scrape output -> scrape output
    Db input output -> db input output
    Query database input -> query database input

scrape :: FilePath -> IO ()
scrape output = do
  items <- listDirectory "/nix/store"
  let drvs = filter ((".drv" ==) . takeExtension) items
  let count = length drvs

  queue <- newTQueueIO
  doneVar <- newTVarIO False
  processedCountVar <- newTVarIO 0
  resultsVar <- newTVarIO KeyMap.empty
  start <- getCurrentTime
  ( do
      traverse_ (atomically . writeTQueue queue) drvs
      atomically $ writeTVar doneVar True
    )
    `concurrently_` replicateConcurrently
      100
      ( do
          let
            loop = do
              mDrv <- atomically $ do
                fmap Just (readTQueue queue) <|> do
                  done <- readTVar doneVar
                  if done then pure Nothing else retry
              case mDrv of
                Nothing -> pure ()
                Just path -> do
                  let drv = "/nix/store" </> path
                  (exitCode, stdout, stderr) <- readProcessWithExitCode "nix" ["derivation", "show", drv] ""
                  case exitCode of
                    ExitFailure code -> do
                      putStrLn $ "error: `nix derivation show " ++ drv ++ "` failed (exit code " ++ show code ++ "):"
                      putStrLn stderr
                    ExitSuccess -> do
                      let result = Json.eitherDecodeStrictText @Json.Value $ fromString stdout
                      case result of
                        Left err -> do
                          putStrLn $
                            "error: `nix derivation show " ++ drv ++ "` returned invalid JSON (error message: " ++ err ++ "):"
                          putStrLn stdout
                          exitFailure
                        Right value -> do
                          atomically $ do
                            modifyTVar' resultsVar $ KeyMap.insert (fromString drv) value
                            modifyTVar' processedCountVar (+ 1)
                          loop
          loop
      )
    `concurrently_` ( do
                        let
                          loop prevProcessedCount = do
                            processedCount <- readTVarIO processedCountVar
                            if count == processedCount
                              then putStrLn "done"
                              else
                                if prevProcessedCount /= processedCount
                                  then do
                                    putStrLn $ show processedCount ++ "/" ++ show count
                                    loop processedCount
                                  else do
                                    race_
                                      (threadDelay 100_000)
                                      ( atomically $ do
                                          processedCount' <- readTVar processedCountVar
                                          guard $ processedCount' == count
                                      )
                                    loop prevProcessedCount

                        loop (-1)
                    )
  end <- getCurrentTime
  putStrLn $ "took " ++ show (end `diffUTCTime` start)

  putStrLn "Serialising data..."
  start' <- getCurrentTime
  results <- readTVarIO resultsVar
  Data.ByteString.Lazy.writeFile output
    . CBOR.toLazyByteString
    . encodeValue
    $ Json.Object results
  putStrLn $ "Wrote " ++ output
  end' <- getCurrentTime
  putStrLn $ "Took " ++ show (end' `diffUTCTime` start')

db ::
  -- | Input
  FilePath ->
  -- | Output
  FilePath ->
  IO ()
db input output = do
  bytes <- Data.ByteString.Lazy.readFile input

  putStrLn $ "Reading " ++ input ++ "..."
  drvs <- case deserialiseFromBytes (decodeValue False) bytes of
    Left err -> do
      putStrLn $ "error: failed to decode " ++ input ++ ":"
      print err
      exitFailure
    Right (_, value) ->
      case value of
        Json.Object drvs ->
          pure drvs
        _ -> do
          putStrLn "error: expected object, got:"
          print value
          exitFailure

  putStrLn "Constructing database..."
  let
    !database =
      diskDatabaseInsertRows
        (fromString "derivation")
        (objectToRows drvs)
        diskDatabaseEmpty

  putStrLn $ "Writing " ++ output ++ "..."
  storeDiskDatabase output database
  putStrLn "done"

objectToRows :: Json.Object -> [Row]
objectToRows obj =
  [ Row [CString (Key.toText k), valueToConstant v']
  | (k, Json.Object v) <- KeyMap.toList obj
  , -- `nix derivation show` outputs `{"/nix/store/...": {...}}` per derivation.
  v' <- maybeToList $ KeyMap.lookup k v
  ]

valueToConstant :: Json.Value -> Constant
valueToConstant (Json.Object o) =
  CMap . Map.fromList . fmap (\(k, v) -> (CString $ Key.toText k, valueToConstant v)) $
    KeyMap.toList o
valueToConstant (Json.Array a) =
  CList $ fmap valueToConstant a
valueToConstant (Json.String s) =
  CString s
valueToConstant (Json.Bool b) =
  CBool b
valueToConstant (Json.Number n) = error $ "TODO: number " ++ show n
valueToConstant Json.Null = error "TODO: null"

query :: FilePath -> Text -> IO ()
query databasePath input = do
  program <-
    case parse (getParsersSage Parse.program) (StreamText input) of
      Left err -> do
        putStrLn . LazyText.unpack $
          Diagnostic.render Diagnostic.defaultConfig (fromString "(interactive)") input $
            parseError err
        exitFailure
      Right a -> pure a
  putStrLn $ "Loading " ++ databasePath ++ "..."
  database <- loadDiskDatabase databasePath
  putStrLn "Done"
  let (_changes, output) = eval_seminaive database program
  putStrLn . LazyText.unpack $ formatChange output
