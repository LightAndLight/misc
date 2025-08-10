{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Control.Concurrent.Async (replicateConcurrently, concurrently_, race_)
import Data.Foldable (traverse_, foldl')
import Control.Concurrent.STM.TQueue (newTQueueIO, writeTQueue, readTQueue)
import Control.Concurrent.STM.TVar (newTVarIO, writeTVar, readTVar, readTVarIO, modifyTVar')
import Control.Concurrent.STM (atomically, retry)
import Control.Applicative ((<|>), (<**>))
import System.Process (readProcessWithExitCode)
import Control.Concurrent (threadDelay)
import Control.Monad (guard)
import qualified Data.Aeson as Json
import Data.String (fromString)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure, ExitCode (..))
import qualified Options.Applicative as Options
import qualified Codec.CBOR.Write as CBOR
import Codec.CBOR.JSON (encodeValue, decodeValue)
import Codec.CBOR.Read (deserialiseFromBytes)
import Lib (Row(..), Constant(..), databaseEmpty, databaseInsertRow)
import qualified Data.Aeson.Key as Key
import qualified Data.Map as Map
import Codec.Serialise (writeFileSerialise)
import Data.Maybe (maybeToList)

data Cli
  = Scrape
  | Db FilePath

cliParser :: Options.Parser Cli
cliParser =
  Options.subparser
    (Options.command
      "scrape"
      (Options.info
        (scrapeParser <**> Options.helper)
        (Options.progDesc "Save all Nix store derivations into a single CBOR file" <> Options.fullDesc)
      ) <>
     Options.command
      "db"
      (Options.info
        (dbParser <**> Options.helper)
        (Options.progDesc "Convert the Nix store derivations file into a proper database" <> Options.fullDesc)
      )
    )
  where
    scrapeParser = pure Scrape
    dbParser = Db <$> Options.strArgument (Options.metavar "INPUT" <> Options.help "The Nix store derivations file to convert")

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  case cli of
    Scrape -> scrape
    Db input -> db input

scrape :: IO ()
scrape = do
  items <- listDirectory "/nix/store"
  let drvs = filter ((".drv" ==) . takeExtension) items
  let count = length drvs

  queue <- newTQueueIO
  doneVar <- newTVarIO False
  processedCountVar <- newTVarIO 0
  resultsVar <- newTVarIO KeyMap.empty
  start <- getCurrentTime
  (do
      traverse_ (atomically . writeTQueue queue) drvs
      atomically $ writeTVar doneVar True
    ) `concurrently_`
    replicateConcurrently 100 (do
      let
        loop =  do
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
                      putStrLn $ "error: `nix derivation show " ++ drv ++ "` returned invalid JSON (error message: " ++ err ++ "):"
                      putStrLn stdout
                      exitFailure
                    Right value -> do
                      atomically $ do
                        modifyTVar' resultsVar $ KeyMap.insert (fromString drv) value
                        modifyTVar' processedCountVar (+1)
                      loop
      loop
    ) `concurrently_`
    (do
      let
        loop prevProcessedCount = do
          processedCount <- readTVarIO processedCountVar
          if count == processedCount
            then putStrLn "done"
            else if prevProcessedCount /= processedCount
            then do
              putStrLn $ show processedCount ++ "/" ++ show count
              loop processedCount
            else do
              race_
                (threadDelay 100_000)
                (atomically $ do
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
  Data.ByteString.Lazy.writeFile "nix-store.cbor"
    . CBOR.toLazyByteString
    . encodeValue
    $ Json.Object results
  putStrLn "done"
  end' <- getCurrentTime
  putStrLn $ "took " ++ show (end' `diffUTCTime` start')

db :: FilePath -> IO ()
db input = do
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
  let
    database =
      foldl'
        (\acc row -> snd $ databaseInsertRow (fromString "derivation") row acc)
        databaseEmpty
        (objectToRows drvs)

  putStrLn "Writing database.cbor..."
  writeFileSerialise "database.cbor" database
  putStrLn "done"

objectToRows :: Json.Object -> [Row]
objectToRows obj =
  [ Row [CString (Key.toText k), valueToConstant v']
  | (k, Json.Object v) <- KeyMap.toList obj
  -- `nix derivation show` outputs `{"/nix/store/...": {...}}` per derivation.
  , v' <- maybeToList $ KeyMap.lookup k v
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
