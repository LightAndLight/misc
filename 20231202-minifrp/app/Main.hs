{-# LANGUAGE RecursiveDo #-}

module Main where

import Data.Char (toUpper)
import Data.Time.Clock (getCurrentTime)
import Lib
import Prelude hiding (filter)

echo :: Event String -> FRP (Event String)
echo = pure

uppercase :: Event String -> FRP (Event String)
uppercase = pure . fmap (fmap toUpper)

accumulate :: Event String -> FRP (Event String)
accumulate eLine = do
  rec let eAccum = (\(line, acc) -> acc <> line) <$> attach eLine bAccum
      bAccum <- stepperB "" eAccum
  pure $ fmap ("history: " <>) eAccum

timeywimey1 :: Event String -> FRP (Event String)
timeywimey1 eLine = do
  let bTime = fromIO getCurrentTime
  pure $ show . snd <$> attach eLine ((,) <$> bTime <*> bTime)

timeywimey2 :: Event String -> FRP (Event String)
timeywimey2 eLine = do
  let bTime = fromIO getCurrentTime
  pure $ alignWith show (attach eLine bTime) (attach eLine bTime)

switchey :: Event String -> FRP (Event String)
switchey eLine = do
  let eSaidCaps = filter (== "caps") eLine
  pure $ switcherE eLine (fmap (fmap toUpper) eLine <$ eSaidCaps)

toggle :: Event String -> FRP (Event String)
toggle eLine = do
  let eSaidCaps = filter (== "caps") eLine
  rec bIsCaps <- stepperB False $ (\(_, isCaps) -> not isCaps) <$> attach eSaidCaps bIsCaps
  pure $ (\(line, isCaps) -> if isCaps then fmap toUpper line else line) <$> attach eLine bIsCaps

main :: IO ()
main = frpMain $ stdio toggle
