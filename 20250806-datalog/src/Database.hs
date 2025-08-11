{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
module Database where

import Data.Text (Text)
import Data.Vector (Vector)
import Syntax (Constant, formatConstant)
import Codec.Serialise (Serialise, readFileDeserialise)
import Data.Binary (Binary)
import qualified Data.Text.Lazy as Lazy
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import qualified Data.Vector as Vector
import Data.Monoid (Any(..))
import Control.Monad.Writer (runWriter, tell)
import Data.Foldable (foldlM, for_, traverse_)
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import Data.Binary.Get (Decoder(..), runGetIncremental, pushChunk, runGet)
import qualified Data.ByteString as ByteString
import qualified Data.Binary as Binary
import System.IO.Posix.MMap (unsafeMMapFile)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Binary.Put (runPut)
import Control.Applicative (many)

class IsDatabase db where
  deleteRelation :: Text -> db -> db
  lookupRelation :: Text -> db -> Maybe (Set Row)

newtype Row = Row (Vector Constant)
  deriving (Show, Eq, Ord, Serialise, Binary)

formatRow :: Row -> Lazy.Text
formatRow (Row items) =
  fromString "(" <>
  Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList items) <>
  fromString ")"

newtype Database = Database (Map Text (Set Row))
  deriving (Show, Eq, Serialise)

-- | When both 'Database's contain the same relation, the relation is merged using set union.
instance Semigroup Database where
  Database db1 <> Database db2 =
    Database (Map.unionWith (<>) db1 db2)

instance Monoid Database where
  mempty = Database Map.empty

instance IsDatabase Database where
  deleteRelation = databaseDeleteRelation
  lookupRelation = databaseLookupRelation

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

newtype DiskDatabase = DiskDatabase (Map Text ByteString)

instance IsDatabase DiskDatabase where
  deleteRelation = diskDatabaseDeleteRelation
  lookupRelation = diskDatabaseLookupRelation

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
