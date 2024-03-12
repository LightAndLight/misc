{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
    -ddump-to-file #-}

module Main where

import Control.Monad (guard)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Foldable (foldl')
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Prelude hiding (break)

main :: IO ()
main = do
  input <- ByteString.Lazy.readFile "measurements.txt"

  let summary =
        foldl'
          (\acc -> maybe acc (\(k, v) -> Map.insertWith (<>) k v acc) . summariseLine)
          mempty
          (ByteString.Lazy.Char8.split '\n' input)

  ByteString.Lazy.Char8.putStrLn . Builder.toLazyByteString $ printSummary summary

printSummary :: Map Lazy.ByteString Summary -> Builder
printSummary =
  ("{" <>)
    . (<> "}")
    . (`sepBy` ", ")
    . fmap
      ( \(name, summary) ->
          Builder.lazyByteString name
            <> "="
            <> Builder.string8 (show $ summaryMin summary)
            <> "/"
            <> Builder.string8 (show $ summaryTotal summary / summaryCount summary)
            <> "/"
            <> Builder.string8 (show $ summaryMax summary)
      )
    . Map.toAscList
 where
  sepBy :: (Monoid m) => [m] -> m -> m
  sepBy [] _ = mempty
  sepBy [x] _ = x
  sepBy (x : xs) sep = x <> sep <> sepBy xs sep

newtype Deci = Deci Int32
  deriving (Eq, Ord)

instance Fractional Deci where
  fromRational r = Deci (floor (r * 10))
  (/) (Deci a) (Deci b) = Deci (div (a * 10) b)

instance Num Deci where
  fromInteger n = Deci $ fromInteger (n * 10)
  (+) (Deci a) (Deci b) = Deci (a + b)
  (*) (Deci a) (Deci b) = Deci (a * b)
  abs (Deci a) = Deci (abs a)
  signum (Deci a) = Deci (10 * signum a)
  negate (Deci a) = Deci (negate a)

instance Show Deci where
  show (Deci n) = let (q, r) = quotRem n 10 in show q <> "." <> show (abs r)

data Summary = Summary
  { summaryTotal :: !Deci
  , summaryCount :: !Deci
  , summaryMin :: !Deci
  , summaryMax :: !Deci
  }
  deriving (Show)

instance Semigroup Summary where
  Summary a b c d <> Summary a' b' c' d' = Summary (a + a') (b + b') (c `min` c') (d `max` d')

{-# INLINE readDecimal #-}
readDecimal :: Lazy.ByteString -> Maybe Deci
readDecimal input = do
  (big, suffix) <- ByteString.Lazy.Char8.readInt32 input
  (c, suffix') <- ByteString.Lazy.Char8.uncons suffix
  guard $ c == '.'
  (small, suffix'') <- ByteString.Lazy.Char8.readInt32 suffix'
  guard $ ByteString.Lazy.null suffix''
  pure $ Deci (10 * big + small)

{-# INLINE summariseLine #-}
summariseLine :: Lazy.ByteString -> Maybe (Lazy.ByteString, Summary)
summariseLine input
  | ByteString.Lazy.null input = Nothing
  | otherwise = do
      let (prefix, suffix) = ByteString.Lazy.Char8.break (== ';') input
          suffix' = ByteString.Lazy.drop 1 suffix
      value <- readDecimal suffix'
      Just (prefix, Summary value 1 value value)