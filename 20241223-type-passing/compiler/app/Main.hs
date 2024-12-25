{-# LANGUAGE LambdaCase #-}

module Main where

import Compiler.Check (checkModule, runCheckT)
import Compiler.Codegen (genBuiltins, genBuiltinsHeader, genModule)
import Compiler.Codegen.Builder (runModuleT)
import Compiler.Fresh (runFreshT)
import qualified Compiler.Parse as Parse
import Control.Applicative ((<**>))
import Data.Char (isSpace)
import Data.Foldable (for_, toList, traverse_)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.IO as Lazy.Text
import qualified Options.Applicative as Options
import qualified Paths_type_passing_compiler
import Streaming.Chars.Text (StreamText (..))
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing, getCurrentDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeBaseName, takeExtension, (<.>), (</>))
import qualified System.IO
import System.Process (readProcessWithExitCode)
import qualified Text.Diagnostic as Diagnostic
import Text.Sage (Label (..), ParseError (..), parse)

data Cli = Cli
  { inputFile :: FilePath
  , outputFile :: FilePath
  , buildDir :: FilePath
  }

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strArgument
      ( Options.metavar "FILE"
          <> Options.help "Compile FILE"
      )
    <*> Options.strOption
      ( Options.short 'o'
          <> Options.long "output"
          <> Options.metavar "FILE"
          <> Options.help "Save the compiled program to FILE"
          <> Options.value "out"
          <> Options.showDefault
      )
    <*> Options.strOption
      ( Options.long "build-dir"
          <> Options.metavar "DIR"
          <> Options.help "Store intermediate files in DIR"
          <> Options.value ".build"
          <> Options.showDefault
      )

eprintln :: String -> IO ()
eprintln = System.IO.hPutStrLn System.IO.stderr

sepBy :: Monoid m => m -> [m] -> m
sepBy _ [] = mempty
sepBy _ [x] = x
sepBy s (x : xs@(_ : _)) = x <> s <> sepBy s xs

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  cwd <- getCurrentDirectory

  inputFile <- canonicalizePath $ cwd </> cli.inputFile
  eprintln $ "Compiling " ++ inputFile ++ "..."

  outputFile <- canonicalizePath $ cwd </> cli.outputFile

  buildDir <- canonicalizePath $ cwd </> cli.buildDir
  createDirectoryIfMissing True buildDir

  input <- Text.readFile inputFile
  module_ <-
    case parse Parse.module_ $ StreamText input of
      Left (Unexpected offset labels) -> do
        Lazy.Text.hPutStrLn System.IO.stderr
          . Diagnostic.render Diagnostic.defaultConfig (Text.pack cli.inputFile) input
          . Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret
          $ Diagnostic.Message
            ( Builder.fromString "expected one of: "
                <> sepBy
                  (Builder.fromString ", ")
                  ( fmap
                      ( \case
                          Eof -> Builder.fromString "end of file"
                          Char c -> Builder.fromString (show c)
                          String s -> Builder.fromString s
                          Text t -> Builder.fromText t
                      )
                      (toList labels)
                  )
            )
        exitFailure
      Right a -> pure a

  checkedModule <-
    case runIdentity . runCheckT $ checkModule module_ of
      Left err -> error $ show err
      Right a -> pure a

  runtimeCodePaths <- do
    let
      runtimeFiles =
        [ "runtime.h"
        , "runtime/array.h"
        , "runtime/array.c"
        , "runtime/closure.h"
        , "runtime/rc.h"
        , "runtime/rc.c"
        , "runtime/type.h"
        , "runtime/type.c"
        ]

    createDirectoryIfMissing False $ buildDir </> "runtime"

    for_ runtimeFiles $ \runtimeFile -> do
      dataFileName <- Paths_type_passing_compiler.getDataFileName $ "data" </> runtimeFile
      copyFile dataFileName (buildDir </> runtimeFile)

    pure $
      mapMaybe
        ( \runtimeFile ->
            if takeExtension runtimeFile == ".c"
              then Just $ buildDir </> runtimeFile
              else Nothing
        )
        runtimeFiles

  let builtinsHeaderPath = buildDir </> "builtins" <.> "h"
  do
    let (builtinsHeader, ()) = runIdentity . runFreshT . runModuleT $ genBuiltinsHeader
    Lazy.Text.writeFile builtinsHeaderPath builtinsHeader

  let builtinsCodePath = buildDir </> "builtins" <.> "c"
  do
    let (builtinsCode, ()) = runIdentity . runFreshT . runModuleT $ genBuiltins
    Lazy.Text.writeFile builtinsCodePath builtinsCode

  let moduleCodePath = buildDir </> takeBaseName inputFile <.> "c"
  do
    let (moduleCode, ()) = runIdentity . runFreshT . runModuleT $ genModule checkedModule
    Lazy.Text.writeFile moduleCodePath moduleCode

  let command = "clang"
  let args = runtimeCodePaths ++ [builtinsCodePath, moduleCodePath, "-o", buildDir </> "out"]
  (exitCode, stdout, stderr) <- readProcessWithExitCode command args ""

  case exitCode of
    ExitFailure code -> do
      eprintln $
        "\nerror: "
          ++ "command `"
          ++ command
          ++ foldMap
            (\arg -> " " ++ if any isSpace arg then show arg else arg)
            args
          ++ "` failed\n"

      eprintln $ "exit code: " ++ show code
      eprintln "stdout: "
      traverse_ (eprintln . ("  " ++)) (lines stdout)

      eprintln "stderr:"
      traverse_ (eprintln . ("  " ++)) (lines stderr)
    ExitSuccess -> do
      eprintln "Done"
      copyFile (buildDir </> "out") outputFile
      eprintln $ "Created " ++ outputFile
