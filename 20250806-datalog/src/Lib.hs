{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib where

import Data.Vector (Vector)
import Data.Text (Text)
import Numeric.Natural (Natural)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Writer (runWriter, MonadWriter (tell))
import Data.Monoid (Any(..))
import Data.Foldable (foldlM, foldl', traverse_, for_)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Maybe (maybeToList, fromMaybe)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad (guard)
import Data.Tuple (swap)
import qualified Data.Text.Lazy as Lazy
import Data.String (fromString)
import GHC.Generics (Generic)
import Codec.Serialise (Serialise, readFileDeserialise)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import System.IO.Posix.MMap (unsafeMMapFile)
import Data.Binary.Get (Decoder(..), runGetIncremental, pushChunk, runGet)
import qualified Data.Binary as Binary
import Data.Binary (Binary)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Control.Applicative (many)
import Data.Binary.Put (runPut)

newtype Program = Program (Vector Definition)
  deriving newtype (Show, Eq, Semigroup, Monoid)

data Definition
  -- |
  -- @
  -- {name}({args}) :- {body} where {bindings}.
  -- @
  = Rule
      -- | Name
      !Text
      -- | Arguments
      !(Vector Text)
      -- | Body
      !(Vector Relation)
      -- | Bindings
      !(Vector Binding)
  | Fact
      -- | Name
      !Text
      -- | Arguments
      !(Vector Constant)
  deriving (Show, Eq)

data Relation
  = Relation !Text !(Vector Expr)
  deriving (Show, Eq)

data Expr
  = Wild
  | Var !Text
  | Constant !Constant
  | Map
      -- | Keys must match exactly
      Bool
      (Map Constant Expr)
  | List (Vector Expr)
  deriving (Show, Eq)

data Binding
  -- | @{name} is {bexpr}
  = Binding
      -- | Name
      !Text
      -- | Value
      !BExpr
  deriving (Show, Eq)

data BExpr
  = BKeys Expr
  | BItems Expr
  deriving (Show, Eq)

data Constant
  = CBool !Bool
  | CString !Text
  | CNatural !Natural
  | CMap !(Map Constant Constant)
  | CList !(Vector Constant)
  deriving (Show, Eq, Ord, Generic, Serialise, Binary)

newtype Database = Database (Map Text (Set Row))
  deriving newtype (Show, Eq, Serialise)

newtype Row = Row (Vector Constant)
  deriving newtype (Show, Eq, Ord, Serialise, Binary)

-- | When both 'Database's contain the same relation, the relation is merged using set union.
instance Semigroup Database where
  Database db1 <> Database db2 =
    Database (Map.unionWith (<>) db1 db2)

instance Monoid Database where
  mempty = Database Map.empty

loadCborDatabase :: FilePath -> IO Database
loadCborDatabase = readFileDeserialise

databaseEmpty :: Database
databaseEmpty = Database mempty

databaseFact ::
  -- | Relation name
  Text ->
  -- | Arguments
  Vector Constant ->

  Database
databaseFact name args = Database $ Map.singleton name (Set.singleton $ Row args)

databaseInsertRow ::
  -- | Relation name
  Text ->
  -- | New row
  Row ->
  Database ->
  -- | Whether the database changed, and the (possibly) updated database
  (Bool, Database)
databaseInsertRow relation row (Database db) =
  case Map.lookup relation db of
    Just rows | Set.member row rows -> (False, Database db)
    _ ->
      let
        !db' = Database $ Map.insertWith (<>) relation (Set.singleton row) db
      in
        (True, db')

databaseUpdate ::
  Database ->
  Change ->
  -- | Whether the database changed, and the (possibly) updated database
  (Bool, Database)
databaseUpdate db (Change (Database change)) =
  let
    (db', Any changed) =
      runWriter $
      foldlM
        (\acc (name, rows) ->
          foldlM
            (\acc' row -> do
              let (changed', acc'') = databaseInsertRow name row acc'
              tell $ Any changed'
              pure acc''
            )
            acc
            (Set.toList rows)
        )
        db
        (Map.toList change)
  in
    (changed, db')

databaseLookupRelation ::
  -- | Relation name
  Text ->
  Database ->
  Maybe (Set Row)
databaseLookupRelation name (Database db) = Map.lookup name db

databaseDeleteRelation ::
  -- | Relation name
  Text ->
  Database ->
  Database
databaseDeleteRelation name (Database db) = Database (Map.delete name db)

databaseRestrictRelation ::
  -- | Relation names
  Set Text ->
  Database ->
  Database
databaseRestrictRelation names (Database db) = Database (Map.restrictKeys db names)

databaseReplaceRelation ::
  -- | Relation name
  Text ->
  Set Row ->
  Database ->
  Database
databaseReplaceRelation name rows (Database db) = Database (Map.insert name rows db)

newtype Change = Change{ unChange :: Database }
  deriving newtype (Show, Eq, Semigroup, Monoid)

subtractChange :: Change -> Change -> Change
subtractChange (Change (Database db)) (Change (Database db')) =
  Change . Database $
  foldl'
    (\acc (k', v') ->
      Map.update
        (\v -> do
          let v'' = Set.difference v v'
          guard . not $ Set.null v''
          pure v''
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
    (\name rows ->
      Lazy.fromStrict name <> fromString " = {" <>
      if Set.null rows
      then fromString "}\n"
      else
        Lazy.intercalate (fromString ",") ((fromString "\n  " <>) . formatRow <$> Set.toList rows) <>
        fromString "\n}\n"
    )
    db

formatRow :: Row -> Lazy.Text
formatRow (Row items) =
  fromString "(" <>
  Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList items) <>
  fromString ")"

formatConstant :: Constant -> Lazy.Text
formatConstant (CString s) =
  fromString (show s)
formatConstant (CNatural n) =
  fromString (show n)
formatConstant (CBool b) =
  if b then fromString "true" else fromString "false"
formatConstant (CList xs) =
  fromString "[" <>
  Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList xs) <>
  fromString "]"
formatConstant (CMap xs) =
  fromString "[" <>
  Lazy.intercalate (fromString ", ") ((\(k, v) -> formatConstant k <> fromString " = " <> formatConstant v) <$> Map.toList xs) <>
  fromString "]"

newtype DiskDatabase = DiskDatabase (Map Text ByteString)

instance Binary a => Binary (Vector a) where
  get = do
    len <- Binary.get @Int
    Vector.replicateM len Binary.get

  put xs = do
    Binary.put (Vector.length xs)
    traverse_ Binary.put xs

storeDiskDatabase :: FilePath -> DiskDatabase -> IO ()
storeDiskDatabase path (DiskDatabase relations) = do
  let relations' = Map.toList relations
  let offsets = scanl (\acc (_name, bytes) -> acc + ByteString.length bytes) 0 relations'
  let index = Vector.fromList $ zipWith (\(name, _bytes) offset -> (name, offset)) relations' offsets
  Binary.encodeFile path index
  for_ relations' $ \(_name, bytes) -> do
    ByteString.appendFile path bytes

loadDiskDatabase :: FilePath -> IO DiskDatabase
loadDiskDatabase path = do
  bytes <- unsafeMMapFile path
  case runGetIncremental (Binary.get @(Vector (Text, Int))) `pushChunk` bytes of
    Done rest _consumed index -> do
      let relations = fmap (`ByteString.drop` rest) . Map.fromList $ Vector.toList index
      pure $ DiskDatabase relations
    Partial _ ->
      error "partial binary decode"
    Fail _rest _consumed err ->
      error $ "binary decode failure: " ++ err

diskDatabaseEmpty :: DiskDatabase
diskDatabaseEmpty = DiskDatabase Map.empty

diskDatabaseInsertRow ::
  -- | Relation name
  Text ->
  -- | New row
  Row ->
  DiskDatabase ->
  DiskDatabase
diskDatabaseInsertRow name row (DiskDatabase db) =
  DiskDatabase $
  Map.insertWith
    (\new old -> old <> new)
    name
    (LazyByteString.toStrict $ Binary.encode row)
    db

diskDatabaseInsertRows ::
  Foldable f =>
  -- | Relation name
  Text ->
  -- | New rows
  f Row ->
  DiskDatabase ->
  DiskDatabase
diskDatabaseInsertRows name rows (DiskDatabase db) =
  DiskDatabase $
  Map.insertWith
    (\new old -> old <> new)
    name
    (LazyByteString.toStrict . runPut $ traverse_ Binary.put rows)
    db

diskDatabaseDeleteRelation :: Text -> DiskDatabase -> DiskDatabase
diskDatabaseDeleteRelation name (DiskDatabase db) = DiskDatabase (Map.delete name db)

diskDatabaseLookupRelation :: Text -> DiskDatabase -> Maybe (Set Row)
diskDatabaseLookupRelation name (DiskDatabase db) =
  Set.fromList . runGet (many (Binary.get @Row)) . LazyByteString.fromStrict <$>
  Map.lookup name db

{-# ANN eval_naive "HLINT: ignore Use camelCase" #-}
eval_naive :: Database -> Program -> ([Change], Change)
eval_naive db (Program defs) = swap . runWriter $ go mempty
  where
    go :: MonadWriter [Change] m => Change -> m Change
    go !change = do
      let newChange = foldMap (consequence db change) defs
      let (changed, Change -> change') = databaseUpdate (unChange change) newChange
      if changed
        then do
          tell $ pure change'
          go change'
        else pure change'

class IsDatabase db where
  deleteRelation :: Text -> db -> db
  lookupRelation :: Text -> db -> Maybe (Set Row)

instance IsDatabase Database where
  deleteRelation = databaseDeleteRelation
  lookupRelation = databaseLookupRelation

instance IsDatabase DiskDatabase where
  deleteRelation = diskDatabaseDeleteRelation
  lookupRelation = diskDatabaseLookupRelation

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
      (\match ->
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
  Change ->
  Vector Binding ->
  Map Text Constant ->
  NonEmpty Relation ->
  [Map Text Constant]
matchBody db change@(Change db') bindings subst (Relation name args :| rels) =
  [ match
  | row <-
      case Vector.find (\(Binding bname _) -> name == bname) bindings of
        Nothing ->
          Set.toList $
          fromMaybe Set.empty (lookupRelation name db) <>
          fromMaybe Set.empty (lookupRelation name db')
        Just (Binding _bname bexpr) ->
          genBinding subst bexpr
  , subst' <- maybeToList $ matchRow subst args row
  , match <-
      case NonEmpty.nonEmpty rels of
        Nothing -> [subst']
        Just rels' -> matchBody db change bindings subst' rels' 
  ]

matchRow :: Map Text Constant -> Vector Expr -> Row -> Maybe (Map Text Constant)
matchRow subst expected (Row actual)
  | Vector.length expected == Vector.length actual =
      Vector.foldl'
        (\acc (e, c) -> do
          subst' <- acc
          unify subst' e c
        )
        (Just subst)
        (Vector.zip expected actual)
  | otherwise = error $ "arity mismatch between " ++ show expected ++ " and " ++ show actual

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
          (\subst' (key, c'') -> do
            e' <- Map.lookup key m
            unify subst' e' c''
          )
          subst
          (Map.toList m')
      else do
        -- Unify only the key value pairs in the variable map
        foldlM
          (\subst' (key, e') -> do
            c'' <- Map.lookup key m'
            unify subst' e' c''
          )
          subst
          (Map.toList m)
    (Map _exact _m, _) ->
      error "can't unify map with non-map"
    (List l, CList l') | Vector.length l == Vector.length l' ->
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
eval_seminaive db (Program defs) = swap . runWriter $ go True mempty Nothing
  where
    go ::
      MonadWriter [Change] m =>
      -- | Is this the first iteration?
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
    (\(Relation focusName _) ->
      consequence
        (deleteRelation focusName db)
        (Change $
          databaseReplaceRelation
            focusName
            (fromMaybe Set.empty $ databaseLookupRelation focusName (unChange delta))
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
