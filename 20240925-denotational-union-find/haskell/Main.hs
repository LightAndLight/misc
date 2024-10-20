module Main where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

newtype Equivalence = Equivalence (IntMap Int)
  deriving Show

empty :: Equivalence
empty = Equivalence IntMap.empty

equate :: Int -> Int -> Equivalence -> Equivalence
equate a b (Equivalence e)
  | a == b =
      case IntMap.lookup a e of
        Nothing -> Equivalence $ IntMap.insert a a e
        Just{} -> Equivalence e
  | otherwise =
      case (find a $ Equivalence e, find b $ Equivalence e) of
        (Just aRep, Just bRep) ->
          if aRep == bRep
          then Equivalence e
          else Equivalence $ IntMap.insert bRep aRep e
        (Just aRep, Nothing) -> Equivalence $ IntMap.insert b aRep e
        (Nothing, Just bRep) -> Equivalence $ IntMap.insert a bRep e
        (Nothing, Nothing) -> Equivalence $ IntMap.insert b a $ IntMap.insert a a e
  where
    find :: Int -> Equivalence -> Maybe Int
    find a (Equivalence e) = do
      aRep <- IntMap.lookup a e
      if a == aRep
        then pure aRep
        else find aRep (Equivalence e)

equal :: Int -> Int -> Equivalence -> Bool
equal a b (Equivalence e) =
  case (IntMap.lookup a e, IntMap.lookup b e) of
    (Just aRep, Just bRep) -> aRep == bRep
    _ -> False

data Equivalence' a = Equivalence' (Map a Int) Equivalence

newtype Element a = Element Int

empty' :: Equivalence' a
empty' = Equivalence' Map.empty empty

insert' :: Ord a => a -> Equivalence' a -> (Equivalence' a, Element a)
insert' a x@(Equivalence' atoi e@(Equivalence e')) =
  case Map.lookup a atoi of
    Nothing ->
      let n = IntMap.size e' in
      let atoi' = Map.insert a n atoi in
      (Equivalence' atoi' $ equate n n e, Element n)
    Just n ->
      (x, Element n)

equate' :: Element a -> Element a -> Equivalence' a -> Equivalence' a
equate' (Element m) (Element n) (Equivalence' atoi e) = Equivalence' atoi $ equate m n e

equal' :: Element a -> Element a -> Equivalence' a -> Bool
equal' (Element m) (Element n) (Equivalence' atoi e) = equal m n e

trace :: Show a => a -> IO a
trace a = a <$ print a

main :: IO ()
main = do
  do
    x <- trace empty
    x <- trace $ equate 1 2 x
    _ <- trace (equal 1 1 x, equal 1 2 x, equal 2 1 x, equal 2 2 x)
    x <- trace $ equate 2 3 x
    _ <- trace (equal 1 1 x, equal 1 2 x, equal 2 1 x, equal 1 3 x, equal 3 1 x, equal 2 2 x, equal 2 3 x, equal 3 2 x, equal 3 3 x)
    x <- trace $ equate 4 5 x
    _ <- trace (equal 4 4 x, equal 4 5 x, equal 5 4 x, equal 5 5 x)
    _ <- trace (equal 1 4 x, equal 4 1 x, equal 2 4 x, equal 4 2 x)
    pure ()    
  do
    x <- trace empty
    x <- trace $ equate 1 2 x
    x <- trace $ equate 3 4 x
    x <- trace $ equate 2 3 x
    pure ()
  
