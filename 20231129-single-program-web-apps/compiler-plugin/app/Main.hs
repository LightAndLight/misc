{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Compiler.Plugin.Interface (Quoted, quote)
import qualified Compiler.Plugin.Interface as X

-- import Other
data Event a where
  Dummy :: Event a
  FmapEvent :: Quoted (a -> b) -> Event a -> Event b
  PureEvent :: Quoted a -> Event a
  ApplyEvent :: Event (a -> b) -> Event a -> Event b

deriving instance Show (Event a)

instance Functor Event where
  {-# INLINE fmap #-}
  fmap f a = FmapEvent (quote f) a

instance Applicative Event where
  {-# INLINE pure #-}
  pure a = PureEvent (quote a)

  {-# INLINE (<*>) #-}
  (<*>) a b = ApplyEvent a b

three :: Int
three = 1 + 2

test1 :: Quoted (a -> a)
test1 = quote (\x -> x)

test2 :: Quoted (a -> a)
test2 = quote (\x -> x)

test2' :: Event Bool
test2' = fmap (\x -> x) Dummy

test2'' :: Event Bool
test2'' = (\f x -> f x) <$> Dummy <*> Dummy

test3 :: Event Bool
test3 = fmap not Dummy

test4 :: Quoted (Int -> Int)
test4 = X.quote (\x -> x + 1)

main :: IO ()
main = do
  print three
  putStrLn "test1"
  print test1
  putStrLn "test2"
  print test2
  putStrLn "test2'"
  print test2'
  putStrLn "test2''"
  print test2''
  putStrLn "test3"
  print test3
  putStrLn "test4"
  print test4
