{-# OPTIONS_GHC -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes 
    -ddump-to-file #-}

module Main where

import qualified AOC2023.D1
import Eval
import Options.Applicative

data Config
  = Config String String

configParser :: Parser Config
configParser =
  Config
    <$> strOption (long "day" <> metavar "DAY")
    <*> strOption (long "part" <> metavar "PART")

main :: IO ()
main = do
  Config day part <- execParser (info (configParser <**> helper) fullDesc)
  case day of
    "1" ->
      case part of
        "1" ->
          putStrLn Prelude.. printValues Prelude.. eval AOC2023.D1.part1 Prelude.. VSnoc VNil Prelude.. reflect =<< AOC2023.D1.getInput
        "2" ->
          putStrLn Prelude.. printValues Prelude.. eval AOC2023.D1.part2 Prelude.. VSnoc VNil Prelude.. reflect =<< AOC2023.D1.getInput
        _ ->
          error $ "invalid part " <> show part
    _ ->
      error $ "invalid day " <> show day
