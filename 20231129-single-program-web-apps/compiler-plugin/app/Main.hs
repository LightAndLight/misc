{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Compiler.Plugin.Interface (Quoted, quote)
import qualified Compiler.Plugin.Interface as X
import Data.Foldable (for_)

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

quoteId :: Quoted (a -> a)
quoteId = quote (\x -> x)

qualifiedQuoteInc :: Quoted (Int -> Int)
qualifiedQuoteInc = X.quote (\x -> x + 1)

fmapId :: Event Int
fmapId = fmap (\x -> x) Dummy

x = 1

fmapInc :: Event Int
fmapInc = fmap (\x -> x + 1) Dummy

applyAp :: Event Bool
applyAp = (\f x -> f x) <$> Dummy <*> Dummy

fmapNot :: Event Bool
fmapNot = fmap not Dummy

examples :: [(String, String)]
examples =
  [ ("quoteId", show quoteId)
  , ("qualifiedQuoteInc", show qualifiedQuoteInc)
  , ("fmapId", show fmapId)
  , ("fmapInc", show fmapInc)
  , ("applyAp", show applyAp)
  , ("fmapNot", show fmapNot)
  ]

main :: IO ()
main =
  for_ examples $ \(name, value) -> putStrLn $ name <> ": " <> value
