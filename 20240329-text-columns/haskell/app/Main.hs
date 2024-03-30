{-# language BangPatterns #-}
module Main where

import Data.Char (isPrint)
import Data.Foldable (fold, foldl', traverse_)
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShow)
import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad (guard)
import Data.List (scanl')
import qualified Data.Text.Lazy.Builder as Builder
import Data.Semigroup (stimes)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  let (prefix, suffix) = splitAt n xs in
  if null prefix
  then []
  else if null suffix
  then [prefix]
  else prefix : chunksOf n suffix

scanMaxWidths :: (a -> Int64) -> Int -> [[a]] -> [[Int64]]
scanMaxWidths f numCols =
  scanl' (\acc row -> zipWith max acc (fmap f row <> repeat 0)) (replicate numCols 0)

toRow :: (Lazy.Text -> Int64) -> [Int64] -> [Lazy.Text] -> Lazy.Text
toRow len widths =
  foldMap
    (\(width, content) ->
      content <>
      Text.Lazy.replicate (2 + width - len content) (Text.Lazy.singleton ' ')
    ) . 
    zip widths

printableLen :: Lazy.Text -> Int64
printableLen = Text.Lazy.length . Text.Lazy.filter isPrint

termPrintableLen :: Lazy.Text -> Int64
termPrintableLen = go 0
  where
    go !acc input =
      case Text.Lazy.uncons input of
        Nothing -> acc
        Just (c, cs) ->
          case c of
            '\x1B'
              | Just ('[', rest) <- Text.Lazy.uncons cs ->
                  go acc . Text.Lazy.tail $ Text.Lazy.dropWhile (/= 'm') rest
            _
              | isPrint c -> go (1 + acc) cs
              | otherwise -> go acc cs

allWithLast :: (a -> Bool) -> [a] -> Maybe a
allWithLast f = go
  where
    go [] = Nothing
    go [x] = if f x then Just x else Nothing
    go (x:xs) = if f x then go xs else Nothing

main :: IO ()
main = do
  lines <- Text.Lazy.lines <$> Text.Lazy.IO.getContents
  let numItems = length lines
  let maxWidth = 120
  traverse_ Text.Lazy.IO.putStrLn . head $
    mapMaybe
      (\numCols -> do
        let chunks = chunksOf numCols lines
        let widthsScan = scanMaxWidths termPrintableLen numCols chunks
        let totalPadding = (fromIntegral numCols - 1) * 2
        guard $ all (\widths -> sum widths + totalPadding <= maxWidth) widthsScan
        pure $ fmap (toRow termPrintableLen $ last widthsScan) chunks
      )
      [numItems, numItems - 1 .. 2] <>
    [lines]
