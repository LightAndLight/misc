module Main where

import Control.Comonad (extract)
import Data.Foldable (foldlM)
import qualified Data.HashMap.Strict as HashMap
import Data.List (find)
import Data.Traversable (for)
import Lib
import Options.Applicative
import System.Exit (exitFailure)

data Config
  = Config FilePath [(String, FilePath)] Bool (Maybe String)

configParser :: Parser Config
configParser =
  Config
    <$> strArgument (metavar "FILE" <> help "The file to compile")
    <*> many
      ( option
          ( eitherReader $ \input ->
              let (prefix, suffix) = break (== ':') input
               in case suffix of
                    [] -> Left "missing module path"
                    _ : cs -> pure (prefix, cs)
          )
          (long "module" <> help "Register a module. Format: MODULE_NAME:MODULE_PATH")
      )
    <*> switch (long "check" <> help "Typecheck without compiling or running")
    <*> optional (strOption (long "eval" <> help "Evaluate a definition" <> metavar "NAME"))

main :: IO ()
main = do
  Config path modulePaths checkOnly mEval <- execParser (info (configParser <**> helper) fullDesc)
  if checkOnly
    then runCheck modulePaths path
    else case mEval of
      Nothing ->
        runCompile path
      Just name ->
        runEval modulePaths path name

parse :: FilePath -> IO Module
parse path = do
  contents <- readFile path

  case parseModule contents of
    Left (ParseError err) -> do
      putStrLn err
      exitFailure
    Right module_ ->
      pure module_

check :: (String -> Maybe Module) -> Module -> IO ()
check modules module_ =
  case checkModule modules (\name -> HashMap.lookup name builtinsCheck <|> syntaxesCheck name) module_ of
    Left err -> do
      print err
      exitFailure
    Right () ->
      pure ()

loadModules :: [(String, FilePath)] -> IO (HashMap.HashMap String Module)
loadModules =
  foldlM
    ( \acc (moduleName, modulePath) -> do
        module_ <- parse modulePath
        check (\name -> HashMap.lookup name acc) module_
        pure $ HashMap.insert moduleName module_ acc
    )
    mempty

runCheck :: [(String, FilePath)] -> FilePath -> IO ()
runCheck modulePaths path = do
  modules <- loadModules modulePaths
  module_ <- parse path
  check (\name -> HashMap.lookup name modules) module_
  putStrLn "success"

runCompile :: FilePath -> IO ()
runCompile path =
  putStrLn $ "compile " <> path

runEval :: [(String, FilePath)] -> FilePath -> String -> IO ()
runEval modulePaths path name = do
  modules <- loadModules modulePaths
  module_ <- parse path
  check (\name -> HashMap.lookup name modules) module_
  case lookupDef name module_ of
    Nothing -> do
      print module_
      putStrLn $ name <> " not found in " <> path
      exitFailure
    Just (_, params, _, program) ->
      if null params
        then do
          let
            localContext :: String -> Maybe (EvalContext -> [Either Type Program] -> Values -> Values)
            localContext n = do
              (_, params', _, program') <- lookupDef n module_
              pure $ \context args ->
                evalProgram
                  ( EvalContext $ \n' ->
                      case lookup n' $ zipWith (\(arg, _) param -> (arg, param)) params' args of
                        Just (Right p) ->
                          Just $ \context [] -> evalProgram context p
                        _ ->
                          getEvalContext context n'
                  )
                  (extract program')

          putStrLn . printValues $ evalProgram (EvalContext $ \n -> localContext n <|> builtinsEval n <|> syntaxesEval n) (extract program) Nil
        else do
          putStrLn $ name <> " must take 0 arguments to be used with --eval"