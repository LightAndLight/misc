{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Database where

import Codec.Serialise (Serialise (..), readFileDeserialise)
import Control.Applicative (many)
import Control.Monad.Writer (runWriter, tell)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Binary.Get (Decoder (..), pushChunk, runGet, runGetIncremental)
import Data.Binary.Put (runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (foldlM, for_, traverse_)
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
import System.IO.Posix.MMap (unsafeMMapFile)

data OrderedSet a = OrderedSet (Set a) [a]
  deriving (Show, Eq)

instance (Ord a, Serialise a) => Serialise (OrderedSet a) where
  encode (OrderedSet _seen xs) = encode xs
  decode = foldr @[] orderedSetCons orderedSetEmpty <$> decode

instance Ord a => Semigroup (OrderedSet a) where
  a <> b = foldr orderedSetCons b (orderedSetToList a)

instance Ord a => Monoid (OrderedSet a) where
  mempty = orderedSetEmpty

orderedSetToList :: OrderedSet a -> [a]
orderedSetToList (OrderedSet _ xs) = xs

orderedSetFromList :: Ord a => [a] -> OrderedSet a
orderedSetFromList = foldr orderedSetCons orderedSetEmpty

orderedSetEmpty :: OrderedSet a
orderedSetEmpty = OrderedSet Set.empty []

orderedSetSingleton :: a -> OrderedSet a
orderedSetSingleton x = OrderedSet (Set.singleton x) [x]

orderedSetCons :: Ord a => a -> OrderedSet a -> OrderedSet a
orderedSetCons x set@(OrderedSet seen xs) =
  if x `Set.member` seen
    then set
    else OrderedSet (Set.insert x seen) (x : xs)

orderedSetMember :: Ord a => a -> OrderedSet a -> Bool
orderedSetMember x (OrderedSet seen _xs) = Set.member x seen

orderedSetNull :: OrderedSet a -> Bool
orderedSetNull (OrderedSet _ xs) = null xs

orderedSetDifference :: Ord a => OrderedSet a -> OrderedSet a -> OrderedSet a
orderedSetDifference (OrderedSet seenA xsA) (OrderedSet seenB _xsB) =
  OrderedSet
    (seenA `Set.difference` seenB)
    (filter (\x -> not $ Set.member x seenB) xsA)

class IsDatabase db where
  deleteRelation :: Text -> db -> db
  lookupRelation :: Text -> db -> Maybe (OrderedSet Row)

newtype Row = Row (Vector Constant)
  deriving (Show, Eq, Ord, Serialise, Binary)

formatRow :: Row -> Lazy.Text
formatRow (Row items) =
  fromString "("
    <> Lazy.intercalate (fromString ", ") (formatConstant <$> Vector.toList items)
    <> fromString ")"

newtype Database = Database (Map Text (OrderedSet Row))
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
databaseFact name args = Database $ Map.singleton name (orderedSetSingleton $ Row args)

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
    Just rows | orderedSetMember row rows -> (False, Database db)
    _ ->
      let
        !db' = Database $ Map.insertWith (<>) relation (orderedSetSingleton row) db
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
          ( \acc (name, rows) ->
              foldlM
                ( \acc' row -> do
                    let (changed', acc'') = databaseInsertRow name row acc'
                    tell $ Any changed'
                    pure acc''
                )
                acc
                (orderedSetToList rows)
          )
          db
          (Map.toList change)
  in
    (changed, db')

databaseLookupRelation ::
  -- | Relation name
  Text ->
  Database ->
  Maybe (OrderedSet Row)
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
  OrderedSet Row ->
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

diskDatabaseLookupRelation :: Text -> DiskDatabase -> Maybe (OrderedSet Row)
diskDatabaseLookupRelation name (DiskDatabase db) =
  orderedSetFromList . runGet (many (Binary.get @Row)) . LazyByteString.fromStrict
    <$> Map.lookup name db
