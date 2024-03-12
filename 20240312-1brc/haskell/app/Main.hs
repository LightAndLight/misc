{-# LANGUAGE BangPatterns #-}
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

import Control.Concurrent (forkIO, getNumCapabilities)
import Control.Concurrent.STM
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Fixed (Deci, E1, Fixed (..))
import Data.Foldable (foldl', foldlM, for_)
import Data.Int (Int32)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum (..))
import Data.Semigroup (Max (..), Min (..))
import Data.Traversable (for)

main :: IO ()
main = do
  input <- ByteString.Lazy.readFile "measurements.txt"

  -- let count = foldl' (\acc _line -> acc + 1) (0 :: Int32) $ ByteString.Lazy.Char8.split '\n' input
  -- print count

  let summary =
        foldl'
          ( \acc line ->
              maybe acc (\(k, v) -> Map.insertWith (<>) k v acc) (summariseLine line)
          )
          mempty
          (ByteString.Lazy.Char8.split '\n' input)
  ByteString.Lazy.Char8.putStrLn . Builder.toLazyByteString $ printSummary summary

{-
n <- getNumCapabilities
if n <= 1
  then do
    ByteString.Lazy.Char8.putStrLn . Builder.toLazyByteString . printSummary $ summariseLines input
  else do
    queue <- newTQueueIO
    done <- newTVarIO False
    results <- for [0 .. n - 1] $ \ix -> do
      result <- newEmptyTMVarIO
      _ <- forkIO $ do
        acc <- newTVarIO mempty
        let
          loop = do
            mLine <-
              atomically $
                fmap Just (readTQueue queue)
                  `orElse` fmap
                    (const Nothing)
                    (readTVar done >>= check)
            case mLine of
              Nothing ->
                atomically $ putTMVar result =<< readTVar acc
              Just line -> do
                let mSummary = summariseLine line
                for_ mSummary $ \(k, v) -> atomically $ modifyTVar' acc (Map.insertWith (<>) k v)
                loop
        loop
      pure result

    for_ (ByteString.Lazy.Char8.split '\n' input) $ \line -> do
      atomically $ writeTQueue queue line
    atomically $ writeTVar done True

    summary <-
      atomically $
        foldlM
          ( \acc result -> do
              r <- takeTMVar result
              pure $ Map.unionWith (<>) acc r
          )
          mempty
          results

    ByteString.Lazy.Char8.putStrLn . Builder.toLazyByteString $ printSummary summary
-}

printSummary :: Map Lazy.ByteString Summary -> Builder
printSummary =
  ("{" <>)
    . (<> "}")
    . (`sepBy` ", ")
    . fmap
      ( \(name, summary) ->
          Builder.lazyByteString name
            <> "="
            <> Builder.string8 (show (MkFixed (fromIntegral $ summaryMin summary) :: Deci))
            <> "/"
            <> Builder.string8 (show $ (MkFixed (fromIntegral $ summaryTotal summary) :: Deci) / fromIntegral (summaryCount summary))
            <> "/"
            <> Builder.string8 (show (MkFixed (fromIntegral $ summaryMax summary) :: Deci))
      )
    . Map.toAscList
 where
  sepBy :: (Monoid m) => [m] -> m -> m
  sepBy [] _ = mempty
  sepBy [x] _ = x
  sepBy (x : xs) sep = x <> sep <> sepBy xs sep

data Summary = Summary
  { summaryTotal :: {-# UNPACK #-} !Int32
  , summaryCount :: {-# UNPACK #-} !Int32
  , summaryMin :: {-# UNPACK #-} !Int32
  , summaryMax :: {-# UNPACK #-} !Int32
  }
  deriving (Show)

instance Semigroup Summary where
  Summary a b c d <> Summary a' b' c' d' = Summary (a + a') (b + b') (c `min` c') (d `max` d')

{-# INLINE summariseLine #-}
summariseLine :: Lazy.ByteString -> Maybe (Lazy.ByteString, Summary)
summariseLine input
  | ByteString.Lazy.null input = Nothing
  | otherwise =
      let (prefix, suffix) = ByteString.Lazy.Char8.break (== ';') input
          suffix' = ByteString.Lazy.drop 1 suffix
          value :: Int32
          value =
            let MkFixed n = read @Deci $ ByteString.Lazy.Char8.unpack suffix'
             in fromIntegral n
       in Just (prefix, Summary value 1 value value)