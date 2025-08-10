{-# LANGUAGE LambdaCase #-}
module LibOld where

import Data.Text (Text)
import Data.Vector (Vector)
import Data.Map (Map)
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Control.Monad (guard)
import Data.Maybe (maybeToList)
import Control.Applicative ((<|>))
import Data.Set (Set)
import qualified Data.Set as Set

data Expr
  -- | Wildcard
  --
  -- @_@
  = Wild

  -- | Name
  --
  -- @x@
  | Name !Text

  -- | String
  --
  -- @"string"@
  | String !Text

  -- | Conjunction
  --
  -- @expr, expr@
  | Conj Expr Expr

  -- | Disjunction
  --
  -- @expr | expr@
  | Disj Expr Expr

  -- | Unification
  --
  -- @expr = expr@
  | Eq Expr Expr

  -- | Inequality
  --
  -- @expr != expr@
  | Neq Expr Expr

  -- | Relation
  --
  -- @rel(a, b)@
  | Relation !Text (Vector Expr)
  deriving (Show, Eq)

data Value
  = VString !Text
  | VTuple (Vector Value)
  deriving (Show, Eq, Ord)

newtype Table
  = Table (Vector (Vector Value))
  deriving (Show, Eq)

data Constraint
  = CEq Value
  | CNeq (Set Value) (Set Text)
  deriving (Show, Eq)

match ::
  Map Text Table ->
  Map Text Constraint ->
  Vector Expr ->
  Vector Value ->
  Maybe (Map Text Constraint)
match tables bindings args row
  | Vector.length args == Vector.length row =
      Vector.foldl'
        (\acc x ->
          case x of
            (Wild, _) ->
              acc
            (Name name, value) | not $ Map.member name tables -> do
              case Map.lookup name =<< acc of
                Just (CEq value') -> do
                  guard $ value == value'
                  acc
                Just (CNeq values _names) -> do
                  guard . not $ Set.member value values
                  Map.insert name (CEq value) <$> acc
                Nothing ->
                  Map.insert name (CEq value) <$> acc
            (String str, VString str') | str == str' ->
              acc
            _ ->
              Nothing
        )
        (Just bindings)
        (Vector.zip args row)
  | otherwise = error "arity mismatch"

antimatch ::
  Map Text Table ->
  Map Text Constraint ->
  Expr ->
  Expr ->
  Maybe (Map Text Constraint)
antimatch tables bindings a b =
  case (a, b) of
    (Wild, _) ->
      Just bindings
    (Name name, Name name')
      | not $ Map.member name tables
      , not $ Map.member name' tables
      ->
        case (Map.lookup name bindings, Map.lookup name' bindings) of
          (Nothing, Nothing) ->
            Just .
            Map.insert name (CNeq mempty (Set.singleton name')) .
            Map.insert name' (CNeq mempty (Set.singleton name)) $
            bindings
          (Nothing, Just{}) ->
            Just $ Map.insert name (CNeq mempty (Set.singleton name')) bindings
          (Just{}, Nothing) ->
            Just $ Map.insert name' (CNeq mempty (Set.singleton name)) bindings
          (Just (CEq value), Just (CEq value')) -> do
            guard $ value /= value'
            pure bindings
          (Just (CNeq values names), Just (CNeq values' names')) ->
            Just .
            Map.insert name  (CNeq values (Set.insert name names)) .
            Map.insert name' (CNeq values' (Set.insert name' names')) $
            bindings
          (_, Just (CNeq values' names')) ->
            Just .
            Map.insert name' (CNeq values' (Set.insert name' names')) $
            bindings
          (Just (CNeq values names), _) ->
            Just .
            Map.insert name (CNeq values (Set.insert name names)) $
            bindings
    (Name name, _)
      | not $ Map.member name tables
      , Just value <- toValue b -> do
      case Map.lookup name bindings of
        Just (CEq value') -> do
          guard $ value /= value'
          pure bindings
        Just (CNeq values names) ->
          Just $ Map.insert name (CNeq (Set.insert value values) names) bindings
        Nothing ->
          Just $ Map.insert name (CNeq (Set.singleton value) mempty) bindings
    (_, Name name)
      | not $ Map.member name tables
      , Just value <- toValue a -> do
      case Map.lookup name bindings of
        Just (CEq value') -> do
          guard $ value /= value'
          pure bindings
        Just (CNeq values names) ->
          Just $ Map.insert name (CNeq (Set.insert value values) names) bindings
        Nothing ->
          Just $ Map.insert name (CNeq (Set.singleton value) mempty) bindings
    (String str, String str') | str /= str' ->
      Just bindings
    _ ->
      Nothing

toValue :: Expr -> Maybe Value
toValue (String str) = Just $ VString str
toValue _ = Nothing

eval ::
  Map Text Table ->
  Expr ->
  [Map Text Value]
eval tables =
  fmap (Map.mapMaybe $ \case CEq a -> Just a; CNeq _ _ -> Nothing) . eval' tables Map.empty

eval' ::
  Map Text Table ->
  Map Text Constraint ->
  Expr ->
  [Map Text Constraint]
eval' tables bindings (Relation name args) =
  case Map.lookup name tables of
    Just (Table rows) ->
      [ matched
      | row <- Vector.toList rows
      , Just matched <- pure $ match tables bindings args row
      ]
    Nothing ->
      error $ Text.unpack name ++ " not in scope"
eval' tables bindings (Conj a b) = do
  bindings' <- eval' tables bindings a
  eval' tables bindings' b
eval' tables bindings (Disj a b) =
  eval' tables bindings a <|> eval' tables bindings b
eval' tables bindings (Eq a b) =
  case (toValue a, toValue b) of
    (Nothing, Nothing) ->
      []
    (Nothing, Just value) ->
      maybeToList $ match tables bindings (Vector.singleton a) (Vector.singleton value)
    (Just value, Nothing) ->
      maybeToList $ match tables bindings (Vector.singleton b) (Vector.singleton value)
    (Just value, Just value') -> do
      guard $ value == value'
      pure bindings
eval' tables bindings (Neq a b) =
  maybeToList $ antimatch tables bindings a b
eval' _tables _bindings _expr = error "TODO"
