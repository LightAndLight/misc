{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Char (toUpper)
import Data.Time.Clock (getCurrentTime)
import Lib

echo :: Event String -> FRP (Event String)
echo = pure

uppercase :: Event String -> FRP (Event String)
uppercase = pure . fmap (fmap toUpper)

accumulate :: Event String -> FRP (Event String)
accumulate eLine = do
  rec let eAccum = (\(line, acc) -> acc <> line) <$> attach eLine bAccum
      bAccum <- stepperM "" eAccum
  pure $ fmap ("history: " <>) eAccum

timeywimey1 :: Event String -> FRP (Event String)
timeywimey1 eLine = do
  let bTime = fromIO getCurrentTime
  pure $ show . snd <$> attach eLine ((,) <$> bTime <*> bTime)

timeywimey2 :: Event String -> FRP (Event String)
timeywimey2 eLine = do
  let bTime = fromIO getCurrentTime
  pure $ alignWith show (attach eLine bTime) (attach eLine bTime)

main :: IO ()
main = frpMain $ stdio timeywimey2
