{-# language BangPatterns #-}
module Main where

import Data.Char (isPrint)
import Data.Foldable (fold)
import qualified Data.Text.Lazy.IO as Text.Lazy.IO
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Int (Int64)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  let (prefix, suffix) = splitAt n xs in
  if null prefix
  then []
  else if null suffix
  then [prefix]
  else prefix : chunksOf n suffix

zipWithPadded :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipWithPadded f z [] [] = []
zipWithPadded f z [] (x:xs) = f z x : zipWithPadded f z [] xs
zipWithPadded f z (x:xs) [] = f x z : zipWithPadded f z xs []
zipWithPadded f z (x:xs) (y:ys) = f x y : zipWithPadded f z xs ys

maxWidths :: (a -> Int64) -> [[a]] -> [Int64]
maxWidths f = foldr (zipWithPadded max 0 . fmap f) []

columns :: (Lazy.Text -> Int64) -> Int -> [Lazy.Text] -> [Lazy.Text]
columns len numCols input =
  fmap
    (fold . 
      zipWith 
        (\width content ->
          content <>
          Text.Lazy.replicate (2 + width - len content) (Text.Lazy.singleton ' ')
        )
        widths
    )
    chunked
  where
    chunked = chunksOf numCols input
    widths = maxWidths len chunked

columnize :: (Lazy.Text -> Int64) -> Int64 -> [Lazy.Text] -> [Lazy.Text]
columnize len maxWidth input =
  head $
    filter 
      (all ((maxWidth >=) . Text.Lazy.length))
      (fmap
        (\numCols -> columns len numCols input)
        [numItems, numItems - 1 .. 2]
      )
    <>
    [input]
  where
    numItems = length input

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

main :: IO ()
main = do
  input <- Text.Lazy.IO.getContents
  Text.Lazy.IO.putStr .
    Text.Lazy.unlines .
    columnize termPrintableLen 120 $
    Text.Lazy.lines input
