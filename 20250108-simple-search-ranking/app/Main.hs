{-# language BangPatterns #-}
{-# language DeriveFunctor #-}
{-# language TupleSections #-}
module Main where

import Data.List (inits, sort, sortBy, sortOn, tails, maximumBy, uncons)
import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers)
import Data.Ord (Down(..))
import Control.Monad (guard)
import Data.Function (on)
import Data.Bool (bool)
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Bifunctor (first)

data Result = Result{offset :: Int, len :: Int}
  deriving (Show, Eq)

newtype Ranked = Ranked Result
  deriving (Show, Eq)

instance Ord Ranked where
  compare (Ranked (Result o l)) (Ranked (Result o' l')) =
    case compare l l' of
      GT -> GT
      EQ ->
        case compare o o' of
          GT -> LT
          EQ -> EQ
          LT -> GT
      LT -> LT

searchPrefixing ::
  -- | Query
  String ->

  -- | Target
  String ->

  [Result]
searchPrefixing query = go 0 [(query, Result 0 0)] []
  where
    stepQueries ::
      -- | Partial
      Char ->

      -- | Current queries
      [(String, Result)] ->
      
      -- | Successes
      ( [Result]
      
      -- | Remaining queries
      , [(String, Result)]
      )
    stepQueries c =
      partitionEithers .
      mapMaybe
        (\(remaining, result) ->
          case remaining of
            [] ->
              undefined

            c' : cs'
              | c == c' ->
                  let result' = result{ len = len result + 1 } in
                  case cs' of
                    [] 
                      | len result' > 2 -> Just $ Left result'
                      | otherwise -> Nothing
                    _:_ -> Just $ Right (cs', result')
              | len result > 2 -> Just $ Left result
              | otherwise -> Nothing
        )
    
    go ::
      -- | Position in the target
      Int ->

      -- | Current queries
      [        
        -- | Remaining
        ( String
        
        -- | Result if matched
        , Result
        )
      ] ->

      -- | Current results
      [Result] ->

      -- | Remaining target
      String ->
      
      [Result]
    go !ix queries results [] = results
    go !ix queries results (c : cs) =
      let (results', queries') = stepQueries c queries in
      go (ix + 1) ((query, Result (ix + 1) 0) : queries') (results' ++ results) cs

trivial :: String -> Bool
trivial "" = True
trivial [_] = True
trivial [_, _] = True
trivial _ = False
  
substrings :: String -> [(Int, String)]
substrings word =
  let (prefixes, infixes) = go 0 word in
  fmap (0,) prefixes ++ infixes
  where
    go _ix [] = ([], [])
    go ix (x : xs) =
      let ~(prefixes, infixes) = go (ix + 1) xs in
      ([x] : fmap (x :) prefixes, fmap (ix + 1,) prefixes ++ infixes)

newtype Trie a = Trie { unTrie :: Map Char (Trie a, [a]) }
  deriving (Show, Functor)

instance Semigroup (Trie a) where
  Trie a <> Trie b = Trie (Map.unionWith (<>) a b)

trieToList :: Trie a -> [(String, a)]
trieToList (Trie m) =
  Map.foldrWithKey (\c (t, vs) rest -> fmap (pure c,) vs ++ (fmap . first) (c:) (trieToList t) ++ rest) [] m

substringsTrie :: String -> Trie Int
substringsTrie = go
  where
    go [] = Trie mempty
    go (x : xs) = 
      let t = go xs in
      Trie $ Map.insertWith (<>) x (t, [0]) (unTrie $ fmap (+ 1) t)

searchInfixing :: String -> String -> [Result]
searchInfixing query target = maxOf $ do
  (_qix, q) <- filter (not . trivial . snd) $ substrings query
  (tix, t) <- filter (not . trivial . snd) $ substrings target
  guard $ q == t
  pure $ Result tix (length q)
  where
    maxOf [] = []
    maxOf xs = [maximumBy (compare `on` Ranked) xs]  

main :: IO ()
main = do
  let query = "inline"
  let
    targets =
      sort
      [ "When to inline in Haskell?"
      , "POSIX command line syntax"
      , "Meaningful choice in games"
      , "Machine learning resources"
      , "Blizzard's technical lineage"
      , replicate 1000 'M'
      , "Designing an inlining heuristic"
      , "Deriving a CLI parser interface"
      , "Deriving a CLI parser interface"
      ]

  print . sortOn (Down . Ranked . fst) $ do
    target <- targets
    result <- searchPrefixing query target
    pure (result, target)
  
  print . sortOn (Down . Ranked . fst) $ do
    target <- targets
    result <- searchInfixing query target
    pure (result, target)
