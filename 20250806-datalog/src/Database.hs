{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import Codec.Serialise (Serialise (..), readFileDeserialise, writeFileSerialise)
import Control.Monad.Writer (runWriter, tell)
import Data.Binary (Binary)
import Data.Foldable (foldlM, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Any (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Syntax (Constant, formatConstant)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)

class IsDatabase db where
  deleteRelation :: Text -> db -> db
  lookupRelation :: Text -> db -> Maybe (Set Row)
  getIndex ::
    -- | Relation name
    Text ->
    -- | Indexed column
    Int ->
    db ->
    Maybe (Map Constant Row)

newtype Row = Row (Vector Constant)
  deriving (Show, Eq, Ord, Serialise, Binary, ToJSON)

formatRow :: Row -> Lazy.Text
formatRow (Row items) =
  fromString "("
    <> Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList items)
    <> fromString ")"

newtype Database = Database (Map Text Table)
  deriving (Show, Eq, Serialise)

data Table
  = Table
      -- | Data
      !(Set Row)
      -- | Indexes
      !(Map Int (Map Constant Row))
  deriving (Show, Eq, Generic)

instance Serialise Table

instance Semigroup Table where
  Table rows indexes <> Table rows' indexes' =
    Table (rows <> rows') (Map.unionWith (<>) indexes indexes')

-- | When both 'Database's contain the same relation, the relation is merged using set union.
instance Semigroup Database where
  Database db1 <> Database db2 =
    Database (Map.unionWith (<>) db1 db2)

instance Monoid Database where
  mempty = Database Map.empty

instance IsDatabase Database where
  deleteRelation = databaseDeleteRelation
  lookupRelation = databaseLookupRelation
  getIndex = databaseGetIndex

loadDatabase :: FilePath -> IO Database
loadDatabase = readFileDeserialise

storeDatabase :: FilePath -> Database -> IO ()
storeDatabase = writeFileSerialise

databaseEmpty :: Database
databaseEmpty = Database mempty

databaseFact ::
  -- | Relation name
  Text ->
  -- | Arguments
  Vector Constant ->
  Database
databaseFact name args =
  Database . Map.singleton name $
  Table (Set.singleton $ Row args) Map.empty

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
    Just (Table rows _indexes) | Set.member row rows -> (False, Database db)
    _ ->
      let
        !db' = Database $ Map.insertWith (<>) relation (Table (Set.singleton row) Map.empty) db
      in
        (True, db')

databaseInsertRows ::
  Foldable f =>
  -- | Relation name
  Text ->
  -- | New rows
  f Row ->
  Database ->
  -- | Whether the database changed, and the (possibly) updated database
  Database
databaseInsertRows name rows db =
  foldl'
    (\acc row -> snd $ databaseInsertRow name row acc)
    db
    rows

databaseCreateUniqueIndex ::
  -- | Relation name
  Text ->
  -- | Column to index
  Int ->
  Database ->
  Database
databaseCreateUniqueIndex name col (Database db)
  | Just (Table rows indexes) <- Map.lookup name db =
      let
        !index = Map.fromList [(row Vector.! col, Row row) | Row row <- Set.toList rows]
        !indexes' = Map.insert col index indexes
      in
        Database $ Map.insert name (Table rows indexes') db
  | otherwise = Database db

databaseUpdate ::
  -- | Original databae
  Database ->
  -- | Change
  Database ->
  -- | Whether the database changed, and the (possibly) updated database
  (Bool, Database)
databaseUpdate db (Database change) =
  let
    (db', Any changed) =
      runWriter $
        foldlM
          ( \acc (name, Table rows _indexes) ->
              foldlM
                ( \acc' row -> do
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
databaseLookupRelation name (Database db) = do
  Table rows _indexes <- Map.lookup name db
  pure rows

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
databaseReplaceRelation name rows (Database db) =
  Database (Map.insert name (Table rows Map.empty) db)

databaseGetIndex ::
  -- | Relation name
  Text ->
  -- | Indexed column
  Int ->
  Database ->
  Maybe (Map Constant Row)
databaseGetIndex name col (Database db) = do
  Table _ indexes <- Map.lookup name db
  Map.lookup col indexes

{-
newtype DiskDatabase = DiskDatabase (Map Text ByteString)

instance IsDatabase DiskDatabase where
  deleteRelation = diskDatabaseDeleteRelation
  lookupRelation = diskDatabaseLookupRelation
  getIndex = diskDatabaseGetIndex

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

diskDatabaseLookupRelation :: Text -> DiskDatabase -> Maybe (OrderedSet Row)
diskDatabaseLookupRelation name (DiskDatabase db) =
  orderedSetFromList . runGet (many (Binary.get @Row)) . LazyByteString.fromStrict
    <$> Map.lookup name db
-}
