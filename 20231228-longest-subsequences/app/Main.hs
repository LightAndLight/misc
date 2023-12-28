{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes 
    -ddump-to-file #-}

module Main where

import Data.Bifunctor (first)
import Data.Function
import Data.List
import GHC.Exts (Int (..), Int#, (+#))

-- | Precondition: input is sorted.
{-# INLINEABLE consecutiveRanges #-}
consecutiveRanges :: (Ord a, Enum a) => [a] -> [[a]]
consecutiveRanges =
  unfoldr
    ( \s ->
        if null s
          then Nothing
          else Just $ maximumBy (compare `on` length) . filter (consecutive . fst) $ zip (inits s) (tails s)
    )
 where
  consecutive [] = True
  consecutive xs@(_ : ys) = all (\(a, b) -> succ a >= b) (zip xs ys)

{-# NOINLINE consecutiveRanges' #-}
consecutiveRanges' :: (Ord a, Enum a) => [a] -> [[a]]
consecutiveRanges' =
  unfoldr
    ( \s ->
        case go s of
          ([], []) ->
            Nothing
          (prefix, suffix) ->
            Just (prefix, suffix)
    )
 where
  go :: (Ord a, Enum a) => [a] -> ([a], [a])
  go [] = ([], [])
  go [x] = ([x], [])
  go (x : xs@(y : _)) =
    if succ x >= y
      then first (x :) (go xs)
      else ([x], xs)

{-# NOINLINE consecutiveRanges'' #-}
consecutiveRanges'' :: (Ord a, Enum a) => [a] -> [[a]]
consecutiveRanges'' = go
 where
  go :: (Ord a, Enum a) => [a] -> [[a]]
  go [] = []
  go (y : ys) =
    let !(# prefix, suffix #) = consumePrefix y ys
     in prefix : go suffix

  consumePrefix :: (Ord a, Enum a) => a -> [a] -> (# [a], [a] #)
  consumePrefix x (y : ys)
    | succ x >= y =
        let !(# prefix, suffix #) = consumePrefix y ys
         in (# x : prefix, suffix #)
  consumePrefix x xs = (# [x], xs #)

toSlices :: [[a]] -> [(Int, Int)]
toSlices xs =
  -- zip (scanl' (\prev x -> prev + length x) 0 xs) (fmap length xs)
  tail $ scanl' (\(prev, prevLength) x -> (prev + prevLength, length x)) (0, 0) xs

{-# NOINLINE consecutiveSlices #-}
consecutiveSlices :: (Enum a, Ord a) => [a] -> [(Int, Int)]
consecutiveSlices = toSlices . consecutiveRanges

{-# NOINLINE consecutiveSlices' #-}
consecutiveSlices' :: (Ord a, Enum a) => [a] -> [(Int, Int)]
consecutiveSlices' =
  unfoldr
    ( \(ix, s) ->
        case go 0 s of
          (0, []) ->
            Nothing
          (len, suffix) ->
            Just ((ix, len), (ix + len, suffix))
    )
    . (,) 0
 where
  go :: (Ord a, Enum a) => Int -> [a] -> (Int, [a])
  go len [] = (len, [])
  go len [_] = (len + 1, [])
  go len (x : xs@(y : _)) =
    if succ x >= y
      then go (len + 1) xs
      else (len + 1, xs)

{-# NOINLINE consecutiveSlices'' #-}
consecutiveSlices'' :: (Ord a, Enum a) => [a] -> [(Int, Int)]
consecutiveSlices'' =
  unfoldr
    ( \(ix, s) ->
        case go Nothing 0 s of
          (0, []) ->
            Nothing
          (len, suffix) ->
            Just ((ix, len), (ix + len, suffix))
    )
    . (,) 0
 where
  go :: (Ord a, Enum a) => Maybe a -> Int -> [a] -> (Int, [a])
  go _ len [] = (len, [])
  go prev len xs@(y : ys) =
    case prev of
      Just x ->
        if succ x >= y
          then go (Just y) (len + 1) ys
          else (len, xs)
      Nothing ->
        go (Just y) (len + 1) ys

{-# NOINLINE consecutiveSlices''' #-}
consecutiveSlices''' :: (Ord a, Enum a) => [a] -> [(Int, Int)]
consecutiveSlices''' =
  unfoldr
    ( \(ix, s) ->
        case s of
          [] ->
            Nothing
          y : ys ->
            let (len, suffix) = go 1 y ys
             in Just ((ix, len), (ix + len, suffix))
    )
    . (,) 0
 where
  go :: (Ord a, Enum a) => Int -> a -> [a] -> (Int, [a])
  go len _ [] = (len, [])
  go len x xs@(y : ys) =
    if succ x >= y
      then go (len + 1) y ys
      else (len, xs)

{-# NOINLINE consecutiveSlices'''' #-}
consecutiveSlices'''' :: (Ord a, Enum a) => [a] -> [(Int, Int)]
consecutiveSlices'''' = go 0#
 where
  go :: (Ord a, Enum a) => Int# -> [a] -> [(Int, Int)]
  go ix [] = []
  go ix (y : ys) =
    let !(# len, suffix #) = consumePrefix 1# y ys
     in (I# ix, I# len) : go (ix +# len) suffix

  consumePrefix :: (Ord a, Enum a) => Int# -> a -> [a] -> (# Int#, [a] #)
  consumePrefix len x (y : ys) | succ x >= y = consumePrefix (len +# 1#) y ys
  consumePrefix len _ xs = (# len, xs #)

check :: ([a] -> [(Int, Int)]) -> [a] -> [[a]]
check f xs = let ys = f xs in fmap (\(ix, len) -> take len $ drop ix xs) ys

main :: IO ()
main = do
  print $ consecutiveRanges @Int []
  print $ consecutiveRanges [1, 2, 3]
  print $ consecutiveRanges [1, 3]
  print $ consecutiveRanges [1, 2, 3, 4, 5, 6]
  print $ consecutiveRanges [1, 2, 3, 5, 6]
  print $ consecutiveRanges [1, 3, 5, 6]

  putStrLn "-----"
  print $ consecutiveRanges' @Int []
  print $ consecutiveRanges' [1, 2, 3]
  print $ consecutiveRanges' [1, 3]
  print $ consecutiveRanges' [1, 2, 3, 4, 5, 6]
  print $ consecutiveRanges' [1, 2, 3, 5, 6]
  print $ consecutiveRanges' [1, 3, 5, 6]

  putStrLn "-----"
  print $ consecutiveRanges'' @Int []
  print $ consecutiveRanges'' [1, 2, 3]
  print $ consecutiveRanges'' [1, 3]
  print $ consecutiveRanges'' [1, 2, 3, 4, 5, 6]
  print $ consecutiveRanges'' [1, 2, 3, 5, 6]
  print $ consecutiveRanges'' [1, 3, 5, 6]

  putStrLn "-----"
  print $ check (consecutiveSlices @Int) []
  print $ check consecutiveSlices [1, 2, 3]
  print $ check consecutiveSlices [1, 3]
  print $ check consecutiveSlices [1, 2, 3, 4, 5, 6]
  print $ check consecutiveSlices [1, 2, 3, 5, 6]
  print $ check consecutiveSlices [1, 3, 5, 6]

  putStrLn "-----"
  print $ check (consecutiveSlices' @Int) []
  print $ check consecutiveSlices' [1, 2, 3]
  print $ check consecutiveSlices' [1, 3]
  print $ check consecutiveSlices' [1, 2, 3, 4, 5, 6]
  print $ check consecutiveSlices' [1, 2, 3, 5, 6]
  print $ check consecutiveSlices' [1, 3, 5, 6]

  putStrLn "-----"
  print $ check (consecutiveSlices'' @Int) []
  print $ check consecutiveSlices'' [1, 2, 3]
  print $ check consecutiveSlices'' [1, 3]
  print $ check consecutiveSlices'' [1, 2, 3, 4, 5, 6]
  print $ check consecutiveSlices'' [1, 2, 3, 5, 6]
  print $ check consecutiveSlices'' [1, 3, 5, 6]

  putStrLn "-----"
  print $ check (consecutiveSlices''' @Int) []
  print $ check consecutiveSlices''' [1, 2, 3]
  print $ check consecutiveSlices''' [1, 3]
  print $ check consecutiveSlices''' [1, 2, 3, 4, 5, 6]
  print $ check consecutiveSlices''' [1, 2, 3, 5, 6]
  print $ check consecutiveSlices''' [1, 3, 5, 6]

  putStrLn "-----"
  print $ check (consecutiveSlices'''' @Int) []
  print $ check consecutiveSlices'''' [1, 2, 3]
  print $ check consecutiveSlices'''' [1, 3]
  print $ check consecutiveSlices'''' [1, 2, 3, 4, 5, 6]
  print $ check consecutiveSlices'''' [1, 2, 3, 5, 6]
  print $ check consecutiveSlices'''' [1, 3, 5, 6]