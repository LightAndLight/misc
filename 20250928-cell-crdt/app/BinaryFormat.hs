{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import BinaryCodec (Def, Ty (..), docDocumented, documentedDef, runDocumented)
import Control.Applicative ((<**>))
import Data.Foldable (for_)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.IO as LazyText
import qualified Options.Applicative as Options
import qualified Pretty
import System.IO (hPutStrLn, stderr)
import qualified Todo.V0

newtype Cli
  = Cli {version :: Text}

cliParser :: Options.Parser Cli
cliParser =
  Cli
    <$> Options.strOption
      ( Options.long "version" <> Options.metavar "NUMBER" <> Options.help "Binary format version to print"
      )

data SomeCodec = forall a. SomeCodec (Def (Codec a))

versions :: [(Text, SomeCodec)]
versions = [(fromString "0", SomeCodec Todo.V0.state)]

main :: IO ()
main = do
  cli <- Options.execParser (Options.info (cliParser <**> Options.helper) Options.fullDesc)
  case lookup cli.version versions of
    Nothing -> do
      hPutStrLn stderr $ "error: unknown version " ++ show cli.version
      hPutStrLn stderr "available versions:"
      for_ versions $ \(version, _) ->
        Text.hPutStrLn stderr version
    Just (SomeCodec codec) -> do
      let (defs, _ty) = runDocumented $ documentedDef codec
      for_ defs $ LazyText.putStrLn . Pretty.render . docDocumented
