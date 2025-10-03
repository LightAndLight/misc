{-# language ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
module Lib where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable, hash)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Data.List (nub)
import Data.Foldable (foldl')

newtype StateId = StateId Int
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable)

mkInitialStateId :: Hashable a => a -> StateId
mkInitialStateId = StateId . hash . Set

mkStateId :: Hashable a => Set StateId -> Change a -> StateId
mkStateId parents change = StateId $ hash (parents, change)

data Change a
  = Set a
  | Pick StateId
  deriving (Show, Eq, Generic, Hashable)

apply :: Change a -> Map StateId a -> a
apply (Set a) _ = a
apply (Pick stateId) state =
  fromMaybe
    (error $ show stateId ++ " not in state: " ++ show (Map.keys state))
    (Map.lookup stateId state)

data Entry a
  = Start !a
  | Change !(Set StateId) !(Change a)
  deriving (Show, Eq)

data Versioned a
  = Versioned
  { current :: !(Map StateId a)
  , history :: !(Map StateId (Entry a))
  } deriving (Show, Eq)

versionedNew :: Hashable a => a -> Versioned a
versionedNew a =
  Versioned
    { current = Map.singleton stateId a
    , history = Map.singleton stateId (Start a)
    }
  where
    stateId = mkInitialStateId a

versionedChange :: Hashable a => Change a -> Versioned a -> Versioned a
versionedChange change versioned =
  let
    source = Map.keysSet $ current versioned
    target = mkStateId (Map.keysSet (current versioned)) change
  in
  Versioned
    { current = Map.singleton target (apply change $ current versioned)
    , history =
        Map.insert
          target
          (Change (Map.keysSet $ current versioned) change)
          (history versioned)
    }

fastForward :: Map (Set StateId) (Map StateId (Change a)) -> Map StateId a -> Map StateId a
fastForward edges vertex =
  {- The use of `powerSet` to check all subsets of the current state means this
  function has worst-case time complexity `O(2^v log v) * O(log e)`, where
  `v` is the size of the vertex and `e` is the total number of edges.
  -}
  let
    vertex' =
      foldMap
        (\source ->
          maybe
            mempty
            (fmap (`apply` vertex))
            (Map.lookup source edges)
        )
        (Set.toList $ Set.powerSet $ Map.keysSet vertex)
  in
    if Map.null vertex'
    then vertex
    else fastForward edges vertex'

versionedMerge :: Eq a => Versioned a -> Versioned a -> Versioned a
versionedMerge v1 v2 =
  let
    newHistory = history v1 <> history v2
    currentStateIds = Map.keysSet (current v1) <> Map.keysSet (current v2)
    newCurrent =
      case lowestCommonAncestor newHistory currentStateIds of
        Nothing ->
          {- My intuition says that having a lowest common ances
          -}
          current v1 `Map.union` current v2
        Just lca ->
          let edges = edgesToLca newHistory lca currentStateIds in
          fastForward edges (current v1) `Map.union`
          fastForward edges (current v2)
  in
    Versioned
      { current = newCurrent
      , history = newHistory
      }

edgesToLca ::
  forall a.
  Map StateId (Entry a) ->
  StateId ->
  Set StateId ->
  Map (Set StateId) (Map StateId (Change a))
edgesToLca history lca = go Map.empty
  where
    go ::
      Map (Set StateId) (Map StateId (Change a)) ->
      Set StateId ->
      Map (Set StateId) (Map StateId (Change a))
    go acc stateIds
      | Set.null stateIds = acc
      | otherwise =
          let
            (acc', stateIds') =
              foldMap
                (\stateId ->
                  case Map.lookup stateId history of
                    Nothing ->
                      mempty
                    Just Start{} ->
                      mempty
                    Just Change{} | stateId == lca ->
                      mempty
                    Just (Change parents change) ->
                      (Map.singleton parents (Map.singleton stateId change), parents)
                )
                stateIds
          in
          go (Map.unionWith (<>) acc acc') stateIds'

data History a
  = Nil
  | Sequential (History a) (Change a, StateId)
  | Concurrent (History a) [History a]
  deriving (Show, Eq)

pathsToRoot :: Versioned a -> StateId -> [Seq StateId]
pathsToRoot versioned stateId =
  case Map.lookup stateId (history versioned) of
    Nothing ->
      []
    Just (Start _) ->
      [Seq.singleton stateId]
    Just (Change previous _) ->
      [ stateId <| path
      | stateId' <- Set.toList previous
      , path <- pathsToRoot versioned stateId'
      ]

longestCommonPath :: Eq a => [Seq a] -> Seq a
longestCommonPath = go Seq.empty
  where
    go :: Eq a => Seq a -> [Seq a] -> Seq a
    go result paths =
      let paths' = fmap Seq.viewr paths in
      case traverse (\case; Seq.EmptyR -> Nothing; ps Seq.:> p -> Just (ps, p)) paths' of
        Nothing ->
          result
        Just paths'' ->
          let (pss, ps) = unzip paths'' in
          case nub ps of
            [p] -> go (p <| result) pss
            _ -> result

enroute :: Map StateId (Entry a) -> StateId -> Set StateId
enroute history stateId =
  case Map.lookup stateId history of
    Nothing -> Set.empty
    Just Start{} -> Set.singleton stateId
    Just (Change parents _) ->
      case Set.toList parents of
        [] -> error "change has no parents"
        x : xs ->
          Set.insert stateId $
          foldl' (\acc parentStateId -> Set.intersection acc (enroute history parentStateId)) (enroute history x) xs

lowestCommonAncestor :: Map StateId (Entry a) -> Set StateId -> Maybe StateId
lowestCommonAncestor history = loop . Set.toList
  where
    loop stateIds =
      case stateIds of
        [] -> Nothing
        x : xs ->
          case xs of
            [] -> Just x
            y : ys -> do
              y' <- go (enroute history x) y
              loop $ y' : ys

    go enrouteSet b
      | b `Set.member` enrouteSet = Just b
      | otherwise =
          case Map.lookup b history of
            Nothing -> Nothing
            Just Start{} -> if Set.member b enrouteSet then Just b else Nothing
            Just (Change parents _) ->
              case Set.toList parents of
                [b'] -> go enrouteSet b'
                _ -> do
                  b' <- lowestCommonAncestor history parents
                  go enrouteSet b'
    
versionedHistory :: forall a. Versioned a -> History a
versionedHistory versioned = go Nothing (Map.keysSet (current versioned))
  where
    go :: Maybe StateId -> Set StateId -> History a
    go mTop stateIds
      | Set.null stateIds = Nil
      | Just top <- mTop, top `Set.member` stateIds = Nil
      | Set.size stateIds == 1, let stateId = Set.findMax stateIds =
          case Map.lookup stateId (history versioned) of
            Nothing ->
              Nil
            Just (Start a) ->
              Nil `Sequential` (Set a, stateId)
            Just (Change previous change) ->
              Sequential (go mTop previous) (change, stateId)
      | otherwise =
          let
            mAncestor = lowestCommonAncestor (history versioned) stateIds
          in
          Concurrent
            (go mTop $ maybe Set.empty Set.singleton mAncestor)
            (go mAncestor . Set.singleton <$> Set.toList stateIds)

main :: IO ()
main = putStrLn "Hello, Haskell!"
