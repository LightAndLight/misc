{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Eval where

import Control.Monad (guard)
import Control.Monad.Writer (MonadWriter (tell), runWriter)
import Data.Foldable (fold, foldl', foldlM)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList, mapMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database
  ( Database (..)
  , IsDatabase
  , Row (..)
  , databaseFact
  , databaseLookupRelation
  , databaseReplaceRelation
  , databaseUpdate
  , deleteRelation
  , formatRow
  , lookupRelation
  , orderedSetDifference
  , orderedSetNull
  , orderedSetToList, getIndex, orderedSetSingleton, Table (..)
  )
import Syntax
  ( BExpr (..)
  , Binding (..)
  , Constant (..)
  , Definition (..)
  , Expr (..)
  , Program (..)
  , Relation (..), toConstant
  )
import Data.List (sortBy)

newtype Change = Change {unChange :: Database}
  deriving (Show, Eq, Semigroup, Monoid)

subtractChange :: Change -> Change -> Change
subtractChange (Change (Database db)) (Change (Database db')) =
  Change . Database $
    foldl'
      ( \acc (k', Table v' _indexes) ->
          Map.update
            ( \(Table v _indexes) -> do
                let v'' = orderedSetDifference v v'
                guard . not $ orderedSetNull v''
                pure $ Table v'' Map.empty
            )
            k'
            acc
      )
      db
      (Map.toList db')

formatChanges :: [Change] -> Lazy.Text
formatChanges = Lazy.intercalate (fromString "---\n") . fmap formatChange

formatChange :: Change -> Lazy.Text
formatChange (Change (Database db)) =
  Map.foldMapWithKey
    ( \name (Table rows _indexes) ->
        Lazy.fromStrict name
          <> fromString " = {"
          <> if orderedSetNull rows
            then fromString "}\n"
            else
              Lazy.intercalate (fromString ",") ((fromString "\n  " <>) . formatRow <$> orderedSetToList rows)
                <> fromString "\n}\n"
    )
    db

{-# ANN eval_naive "HLINT: ignore Use camelCase" #-}
eval_naive :: Database -> Program -> ([Change], Change)
eval_naive db (Program defs) = swap . runWriter $ go mempty
  where
    go :: MonadWriter [Change] m => Change -> m Change
    go !change = do
      let newChange = foldMap (consequence db change) defs
      let (changed, Change -> change') = databaseUpdate (unChange change) (unChange newChange)
      if changed
        then do
          tell $ pure change'
          go change'
        else pure change'

consequence :: IsDatabase db => db -> Change -> Definition -> Change
consequence db change (Rule name args body bindings) =
  let
    matches =
      maybe
        (error "rule body is empty")
        (matchBody db change bindings Map.empty)
        (NonEmpty.nonEmpty $ Vector.toList body)
  in
    foldMap
      ( \match ->
          Change $
            databaseFact
              name
              (fmap (\arg -> fromMaybe (error $ "no value for " ++ Text.unpack arg) $ match Map.!? arg) args)
      )
      matches
consequence _db _change (Fact name args) = Change $ databaseFact name args

genBinding :: Map Text Constant -> BExpr -> [Row]
genBinding subst (BKeys expr) =
  case expr of
    Wild -> error "argument to keys not bound"
    Var v ->
      case Map.lookup v subst of
        Nothing -> error "argument to keys not bound"
        Just c ->
          case c of
            CMap m -> Row . pure <$> Map.keys m
            _ -> error "argument to keys not a map"
    Map _exact m -> Row . pure <$> Map.keys m
    _ -> error "argument to keys not a map"
genBinding subst (BItems expr) =
  case expr of
    Wild -> error "argument to items not bound"
    Var v ->
      case Map.lookup v subst of
        Nothing -> error "argument to items not bound"
        Just c ->
          case c of
            CMap m -> (\(a, b) -> Row $ Vector.fromList [a, b]) <$> Map.toList m
            _ -> error "argument to items not a map"
    Map _exact _m -> error "TODO: items from map expr"
    _ -> error "argument to items not a map"

matchBody ::
  IsDatabase db =>
  db ->
  -- | Accumulated changes
  Change ->
  Vector Binding ->
  Map Text Constant ->
  NonEmpty Relation ->
  [Map Text Constant]
matchBody db change@(Change db') bindings subst (Relation name args :| rels) =
  case Vector.find (\(Binding bname _) -> name == bname) bindings of
    Just (Binding _bname bexpr) ->
      [ match
      | row <- genBinding subst bexpr
      , subst' <- maybeToList $ matchRow subst args row
      , match <-
          case NonEmpty.nonEmpty rels of
            Nothing -> [subst']
            Just rels' -> matchBody db change bindings subst' rels'
      ]
    Nothing ->
      [ match
      | subst' <- matchRows db db' subst name args
      , match <-
          case NonEmpty.nonEmpty rels of
            Nothing -> [subst']
            Just rels' -> matchBody db change bindings subst' rels'
      ]

matchRow :: Map Text Constant -> Vector Expr -> Row -> Maybe (Map Text Constant)
matchRow subst expected (Row actual)
  | Vector.length expected == Vector.length actual =
      Vector.foldl'
        ( \acc (e, c) -> do
            subst' <- acc
            unify subst' e c
        )
        (Just subst)
        (Vector.zip expected actual)
  | otherwise = error $ "arity mismatch between " ++ show expected ++ " and " ++ show actual

matchRows ::
  IsDatabase db =>
  db ->
  -- | Accumulated changes
  Database ->
  Map Text Constant ->
  -- | Relation name
  Text ->
  Vector Expr ->
  [Map Text Constant]
matchRows db db' subst name expected =
  [ subst'
  | row <-
      orderedSetToList $
        maybe
          (fold $ lookupRelation name db)
          (\(c, index) ->
            maybe mempty orderedSetSingleton $ Map.lookup c index
          )
          mIndex <>
        fold (lookupRelation name db')
  , subst' <- maybeToList $ matchRow subst expected row
  ]
  where
    constants =
      [ (ix, c)
      | (ix, e) <- zip [0::Int ..] (Vector.toList expected)
      , c <- maybeToList $ toConstant subst e
      ]

    mIndex =
      listToMaybe $ mapMaybe (\(ix, c) -> (,) c <$> getIndex name ix db) constants

unify :: Map Text Constant -> Expr -> Constant -> Maybe (Map Text Constant)
unify subst e c' =
  case (e, c') of
    (Wild, _) ->
      pure subst
    (Var v, _) -> do
      case Map.lookup v subst of
        Nothing -> pure $ Map.insert v c' subst
        Just c -> do
          guard $ c == c'
          pure subst
    (Constant c, _) ->
      subst <$ guard (c == c')
    (Map exact m, CMap m') ->
      if exact
        then do
          -- Unify every key value pair in the constant map
          foldlM
            ( \subst' (key, c'') -> do
                e' <- Map.lookup key m
                unify subst' e' c''
            )
            subst
            (Map.toList m')
        else do
          -- Unify only the key value pairs in the variable map
          foldlM
            ( \subst' (key, e') -> do
                c'' <- Map.lookup key m'
                unify subst' e' c''
            )
            subst
            (Map.toList m)
    (Map _exact _m, _) ->
      error "can't unify map with non-map"
    (List l, CList l')
      | Vector.length l == Vector.length l' ->
          foldlM
            (\subst' (x, x') -> unify subst' x x')
            subst
            (Vector.zip l l')
    (List _l, _) ->
      error "can't unify list with non-list"

{- The goal of seminaive evaluation is to compute only *new* tuples at each iteration.
When a step produces no new tuples, the fixpoint has been reached.

```prolog
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

A rule produces new tuples when *any* rule in its body produced new tuples in the previous iteration.

An EDB predicate only "produces" new tuples in the "zeroth iteration".
Therefore, it will never produce new tuples (in iterations 1 and up).

```
Delta_{edge}{0}(X, Y) :- edge(X, Y).
Delta_{edge}{i+1}(X, Y) :- false.
```

Therefore rule that solely depends on EDB predicates will only produce new tuples in the first iteration:

```
Delta_{path}{i+1}(X, Y) :- Delta_{edge}{i}(X, Y).
```

```
Delta_{path}{1}(X, Y) :- edge(X, Y).
Delta_{path}{i+2}(X, Y) :- false.
```

When a rule has multiple body rules, we need to account for changes in each of them.

```
Delta_{path}{i+1}(X, Y) :- Delta{edge}{i}(X, Y), path(Y, Z).
Delta_{path}{i+1}(X, Y) :- edge(X, Y), Delta_{path}{i}(Y, Z).
```
-}
{-# ANN eval_seminaive "HLINT: ignore Use camelCase" #-}
eval_seminaive :: forall db. IsDatabase db => db -> Program -> ([Change], Change)
eval_seminaive db (Program (fmap (sipSorted db) -> defs)) = swap . runWriter $ go True mempty Nothing
  where
    go ::
      MonadWriter [Change] m =>
      -- \| Is this the first iteration?
      Bool ->
      Change ->
      Maybe Change ->
      m Change
    go first !acc !delta = do
      let delta' = foldMap (consequence_seminaive first db acc delta) defs
      if delta' /= mempty
        then do
          tell $ pure delta'
          go False (acc <> delta') (Just delta')
        else
          pure acc

-- | Sort the rules in each definition according to a "sideways information passing strategy".
sipSorted :: IsDatabase db => db -> Definition -> Definition
sipSorted _db def@Fact{} = def
sipSorted db (Rule name params body bindings) =
  Rule name params body' bindings
  where
    body' = Vector.fromList . sortBy sipOrdering $ Vector.toList body

    sipOrdering :: Relation -> Relation -> Ordering
    sipOrdering (Relation name1 args1) (Relation name2 args2) =
      let
        vars1 =
          Map.fromList
            [ (v, ix)
            | (ix, e) <- zip [0::Int ..] (Vector.toList args1)
            , Var v <- pure e
            ]

        vars2 =
          Map.fromList
            [ (v, ix)
            | (ix, e) <- zip [0::Int ..] (Vector.toList args2)
            , Var v <- pure e
            ]

        varsBoth :: [(Text, (Int, Int))]
        varsBoth = Map.toList $ Map.intersectionWith (,) vars1 vars2

        count1indexes =
          length
            [ ()
            | (_v, (ix1, _ix2)) <- varsBoth
            , _ <- maybeToList $ getIndex name1 ix1 db
            ]
        count2indexes =
          length
            [ ()
            | (_v, (_ix1, ix2)) <- varsBoth
            , _ <- maybeToList $ getIndex name2 ix2 db
            ]
      in
        count1indexes `compare` count2indexes

{-# ANN consequence_seminaive "HLINT: ignore Use camelCase" #-}
consequence_seminaive ::
  IsDatabase db =>
  -- | Is this the first iteration?
  Bool ->
  db ->
  -- | Accumulated changes
  Change ->
  -- | Changes from previous iteration
  Maybe Change ->
  Definition ->
  Change
consequence_seminaive True _db _acc _delta (Fact name args) =
  Change $ databaseFact name args
consequence_seminaive False _db _acc _delta (Fact _name _args) =
  mempty
consequence_seminaive _ db _acc Nothing rule =
  consequence db (Change mempty) rule
consequence_seminaive _ db acc (Just delta) rule@(Rule _name _args body _bindings) =
  foldMap
    ( \(Relation focusName _) ->
        consequence
          (deleteRelation focusName db)
          ( Change $
              databaseReplaceRelation
                focusName
                (fold Set.empty $ databaseLookupRelation focusName (unChange delta))
                (unChange acc)
          )
          rule
    )
    (Vector.toList body)
    {- Rules like `R(x) :- R(x)` will always produce a "new" tuple if the previous iteration
    did. So our definition of new must exclude tuples that were produced in the previous
    iteration.
    -}
    `subtractChange` delta
