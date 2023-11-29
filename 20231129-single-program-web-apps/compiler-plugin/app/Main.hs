{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Compiler.Plugin.Interface (Quoted, quote)
import qualified Compiler.Plugin.Interface as X

three :: Int
three = 1 + 2

test1 :: Quoted (a -> a)
test1 = quote (\x -> x)

data Event a where
  Dummy :: Event a
  FmapEvent :: Quoted (a -> b) -> Event a -> Event b

deriving instance Show (Event a)

instance Functor Event where
  fmap f a = FmapEvent (quote f) a

test2 :: Quoted (a -> a)
test2 = quote (\x -> x)

test3 :: Event Bool
test3 = fmap not Dummy

test4 :: Quoted (Int -> Int)
test4 = X.quote (\x -> x + 1)

main :: IO ()
main = do
  print three
  print test1
  print test2
  print test3
  print test4
