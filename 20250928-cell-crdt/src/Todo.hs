{-# language DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Todo where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable, hash)
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import qualified Data.Set as Set
import Data.Functor.Identity (Identity (..))
import Barbies
import Data.Maybe (fromMaybe)
import Control.Monad (guard, replicateM)
import Data.Kind (Type)
import Data.Foldable (foldl', traverse_)
import Data.Time.Clock (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Binary (Binary, Word64)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Fixed (Pico, Milli)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LazyByteString
import Barbies.Constraints (Dict(..))
import qualified Data.Attoparsec.ByteString.Lazy as Attoparsec
import Control.Exception (Exception, throwIO)
import Data.String (fromString)
import Data.Char (ord)
import Data.Binary.Get (runGet)
import System.IO (IOMode(..), withFile)
import qualified Data.ByteString as ByteString

newtype StateId = StateId Int
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable, Binary)

mkStateId :: Set StateId -> Change -> StateId
mkStateId parents change = StateId $ hash (parents, change)

data Task f
  = Task
  { title :: f Text
  , description :: f Text
  } deriving (Generic, FunctorB, TraversableB, ApplicativeB, ConstraintsB, DistributiveB)

deriving instance (forall x. Show x => Show (f x)) => Show (Task f)
deriving instance (forall x. Eq x => Eq (f x)) => Eq (Task f)
deriving instance (forall x. Eq x => Eq (f x), forall x. Hashable x => Hashable (f x)) => Hashable (Task f)

newtype TaskId = TaskId UUID
  deriving (Show, Eq, Ord)
  deriving newtype (Hashable, Binary)

data Metadata
  = Metadata
  { createdAt :: UTCTime
  } deriving (Show, Eq)

data Update a
  = None
  | Set a
  | Pick StateId
  deriving (Show, Eq, Generic, Hashable)

data Change
  = NewTask{ task :: !(Task Identity) }
  | UpdateTask{ taskId :: TaskId, updateTask :: !(Task Update) }
  deriving (Show, Eq, Generic, Hashable)

data Commit = Commit{ parents :: Set StateId, change :: Change }
  deriving Show

data State
  = State
  { current :: !(Set StateId)
  , tasks :: !(Map TaskId (Task (Map StateId)))
  , taskMetadata :: !(Map TaskId Metadata)
  , history :: !(Map StateId Commit)
  } deriving Show

stateNew :: State
stateNew =
  State
    { current = Set.empty
    , tasks = Map.empty
    , taskMetadata = Map.empty
    , history = Map.empty
    }

stateChange :: Change -> State -> IO State
stateChange change@NewTask{ task } state = do
  let stateId = mkStateId (current state) change
  now <- getCurrentTime

  taskId <- TaskId <$> UUID.nextRandom
  pure
    state
      { current = Set.singleton stateId
      , tasks = Map.insert taskId (bmap (Map.singleton stateId . runIdentity) task) (tasks state)
      , taskMetadata = Map.insert taskId Metadata{createdAt = now} (taskMetadata state)
      , history = Map.insert stateId (Commit (current state) change) (history state)
      }
stateChange change@UpdateTask{ taskId, updateTask } state = do
  let stateId = mkStateId (current state) change

  case Map.lookup taskId (tasks state) of
    Nothing -> error $ show taskId ++ " not found"
    Just task -> do
      pure
        state
          { current = Set.singleton stateId
          , tasks =
              Map.insert
                taskId
                (bzipWith
                  (\current update ->
                    case update of
                      None -> current
                      Set a -> Map.singleton stateId a
                      Pick stateId' ->
                        Map.singleton stateId $
                        fromMaybe
                          (error $ show stateId' ++ " missing from " ++ show (Map.keys current))
                          (Map.lookup stateId' current)
                  )
                  task
                  updateTask
                )
                (tasks state)
          , history = Map.insert stateId (Commit (current state) change) (history state)
          }

newtype Getter (b :: (Type -> Type) -> Type) a = Getter (forall f. b f -> f a)

taskGetters :: b ~ Task => b (Getter b)
taskGetters = Task{title = Getter title, description = Getter description}

fastForwardTask :: Map (Set StateId) (Map StateId Change) -> TaskId -> Task (Map StateId) -> Task (Map StateId)
fastForwardTask edges taskId =
  bzipWith
    (\(Getter getter) ->
      fastForward
        (\case
          NewTask{task = task'} ->
            Just (const $ runIdentity (getter task'))
          UpdateTask{taskId = taskId', updateTask} -> do
            guard $ taskId == taskId'
            case getter updateTask of
              None ->
                Nothing
              Set a ->
                Just (const a)
              Pick stateId ->
                Just (\current -> fromMaybe (error $ show stateId ++ " missing from" ++ show (Map.keys current)) $ Map.lookup stateId current)
        )
        edges
    )
    taskGetters
    
fastForward ::
  (Change -> Maybe (Map StateId a -> a)) ->
  Map (Set StateId) (Map StateId Change) ->
  Map StateId a ->
  Map StateId a
fastForward fromChange edges input =
  {- The use of `powerSet` to check all subsets of the current state means this
  function has worst-case time complexity `O(2^v log v) * O(log e)`, where
  `v` is the size of the vertex (i.e. number of conflicts for the datum) and `e`
  is the total number of edges.
  -}
  let
    output =
      foldMap
        (\source ->
          case Map.lookup source edges of
            Nothing -> Map.empty
            Just changes -> ($ input) <$> Map.mapMaybe fromChange changes
        )
        (Set.toList $ Set.powerSet $ Map.keysSet input)
  in
    if Map.null output
    then input
    else fastForward fromChange edges output

fastForward' ::
  Map (Set StateId) (Map StateId Change) ->
  Set StateId ->
  Set StateId
fastForward' edges input =
  {- The use of `powerSet` to check all subsets of the current state means this
  function has worst-case time complexity `O(2^v log v) * O(log e)`, where
  `v` is the size of the vertex (i.e. number of conflicts for the datum) and `e`
  is the total number of edges.
  -}
  let
    output =
      foldMap
        (\source -> maybe Set.empty Map.keysSet $ Map.lookup source edges)
        (Set.toList $ Set.powerSet input)
  in
    if Set.null output
    then input
    else fastForward' edges output

stateMerge :: State -> State -> State
stateMerge s1 s2 =
  let
    newHistory = history s1 <> history s2
    currentStateIds = current s1 <> current s2

    mLca = lowestCommonAncestor newHistory currentStateIds

    (newCurrent, newTasks) =
      case mLca of
        Nothing ->
          ( current s1 <> current s2
          , Map.unionWith (bzipWith (<>)) (tasks s1) (tasks s2)
          )
        Just lca ->
          let edges = edgesToLca newHistory lca currentStateIds in
          ( fastForward' edges (current s1) <> fastForward' edges (current s2)
          , Map.unionWith
              (bzipWith (<>))
              (Map.mapWithKey (fastForwardTask edges) (tasks s1))
              (Map.mapWithKey (fastForwardTask edges) (tasks s2))
          )
  in
  State
    { current = newCurrent
    , tasks = newTasks
    , taskMetadata = taskMetadata s1 <> taskMetadata s2 
    , history = newHistory
    }

edgesToLca ::
  Map StateId Commit ->
  StateId ->
  Set StateId ->
  Map (Set StateId) (Map StateId Change)
edgesToLca history lca = go Map.empty
  where
    go ::
      Map (Set StateId) (Map StateId Change) ->
      Set StateId ->
      Map (Set StateId) (Map StateId Change)
    go acc stateIds
      | Set.null stateIds = acc
      | otherwise =
          let
            (acc', stateIds') =
              foldMap
                (\stateId ->
                  case Map.lookup stateId history of
                    Just (Commit parents change) | stateId /= lca ->
                      (Map.singleton parents (Map.singleton stateId change), parents)
                    _ ->
                      mempty
                )
                stateIds
          in
          go (Map.unionWith (<>) acc acc') stateIds'

enroute :: Map StateId Commit -> StateId -> Set StateId
enroute history stateId =
  case Map.lookup stateId history of
    Nothing -> Set.empty
    Just (Commit parents _) ->
      case Set.toList parents of
        [] -> error "change has no parents"
        x : xs ->
          Set.insert stateId $
          foldl' (\acc parentStateId -> Set.intersection acc (enroute history parentStateId)) (enroute history x) xs

lowestCommonAncestor :: Map StateId Commit -> Set StateId -> Maybe StateId
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
            Just (Commit parents _) ->
              case Set.toList parents of
                [b'] -> go enrouteSet b'
                _ -> do
                  b' <- lowestCommonAncestor history parents
                  go enrouteSet b'

data History
  = Nil
  | Sequential History (Change, StateId)
  | Concurrent History [History]
  deriving (Show, Eq)

stateHistory :: State -> History
stateHistory versioned = go Nothing (current versioned)
  where
    go :: Maybe StateId -> Set StateId -> History
    go mTop stateIds
      | Set.null stateIds = Nil
      | Just top <- mTop, top `Set.member` stateIds = Nil
      | Set.size stateIds == 1, let stateId = Set.findMax stateIds =
          case Map.lookup stateId (history versioned) of
            Nothing ->
              Nil
            Just (Commit previous change) ->
              Sequential (go mTop previous) (change, stateId)
      | otherwise =
          let
            mAncestor = lowestCommonAncestor (history versioned) stateIds
          in
          Concurrent
            (go mTop $ maybe Set.empty Set.singleton mAncestor)
            (go mAncestor . Set.singleton <$> Set.toList stateIds)

stateBinaryPut :: State -> Binary.Put
stateBinaryPut (State current tasks taskMetadata history) = do
  traverse_ Binary.put . ByteString.unpack $ ByteString.Char8.pack "tsk;version=0;\n"
  putCurrent current
  putTasks tasks
  putTaskMetadata taskMetadata
  putHistory history

stateBinaryGet :: Binary.Get State
stateBinaryGet = do
  current <- getCurrent
  tasks <- getTasks
  taskMetadata <- getTaskMetadata
  history <- getHistory
  pure State{current, tasks, taskMetadata, history}

putSet :: Binary a => Set a -> Binary.Put
putSet = Binary.put . Set.toAscList

getSet :: (Binary a, Eq a) => Binary.Get (Set a)
getSet = Set.fromAscList <$> Binary.get

putCurrent :: Set StateId -> Binary.Put
putCurrent = putSet

getCurrent :: Binary.Get (Set StateId)
getCurrent = getSet

putList :: (a -> Binary.Put) -> [a] -> Binary.Put
putList put xs = do
  Binary.put @Word64 . fromIntegral $ length xs
  traverse_ put xs

getList :: Binary.Get a -> Binary.Get [a]
getList get = do
  n <- Binary.get @Word64
  replicateM (fromIntegral n) get

putMap :: Binary k => (v -> Binary.Put) -> Map k v -> Binary.Put
putMap put = putList (\(k, v) -> Binary.put k <> put v) . Map.toAscList

getMap :: (Binary k, Eq k) => Binary.Get v -> Binary.Get (Map k v)
getMap get = Map.fromAscList <$> getList ((,) <$> Binary.get <*> get)

putTasks :: Map TaskId (Task (Map StateId)) -> Binary.Put
putTasks = putMap (bfoldMapC @Binary (putMap Binary.put))

getTasks :: Binary.Get (Map TaskId (Task (Map StateId)))
getTasks = getMap (btraverse (\Dict -> getMap Binary.get) (bdicts @Binary))

putTaskMetadata :: Map TaskId Metadata -> Binary.Put
putTaskMetadata = putMap putMetadata

getTaskMetadata :: Binary.Get (Map TaskId Metadata)
getTaskMetadata = getMap getMetadata

putMetadata :: Metadata -> Binary.Put
putMetadata (Metadata createdAt) = do
  putUTCTime createdAt

getMetadata :: Binary.Get Metadata
getMetadata = do
  createdAt <- getUTCTime
  pure Metadata{ createdAt }

putUTCTime :: UTCTime -> Binary.Put
putUTCTime utcTime = do
  Binary.put @Milli (realToFrac @Pico @Milli . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utcTime)

getUTCTime :: Binary.Get UTCTime
getUTCTime = do
  millis <- Binary.get @Milli
  pure . posixSecondsToUTCTime . secondsToNominalDiffTime $ realToFrac @Milli @Pico millis

putHistory :: Map StateId Commit -> Binary.Put
putHistory = putMap putCommit

getHistory :: Binary.Get (Map StateId Commit)
getHistory = getMap getCommit

putCommit :: Commit -> Binary.Put
putCommit (Commit parents change) = do
  Binary.put $ Set.toAscList parents
  putChange change

getCommit :: Binary.Get Commit
getCommit = do
  parents <- getSet
  change <- getChange
  pure Commit{ parents, change }

putChange :: Change -> Binary.Put
putChange change = do
  putChangeTag change
  putChangeValue change

putChangeTag :: Change -> Binary.Put
putChangeTag NewTask{} = Binary.put @Word64 0
putChangeTag UpdateTask{} = Binary.put @Word64 1

putChangeValue :: Change -> Binary.Put
putChangeValue (NewTask task) =
  bfoldMapC @Binary (\(Identity a) -> Binary.put a) task
putChangeValue (UpdateTask taskId task) = do
  Binary.put taskId
  bfoldMapC @Binary putUpdate task

getChange :: Binary.Get Change
getChange = do
  tag <- Binary.get @Word64
  case tag of
    0 -> do
      task <- btraverse (\Dict -> Identity <$> Binary.get) (bdicts @Binary)
      pure NewTask{ task }
    1 -> do
      taskId <- Binary.get
      updateTask <- btraverse (\Dict -> getUpdate) (bdicts @Binary)
      pure UpdateTask{ taskId, updateTask }
    _ -> fail $ "invalid Change tag: " ++ show tag

putUpdate :: Binary a => Update a -> Binary.Put
putUpdate None =
  Binary.put @Word64 0
putUpdate (Set a) = do
  Binary.put @Word64 1
  Binary.put a
putUpdate (Pick stateId) = do
  Binary.put @Word64 2
  Binary.put stateId

getUpdate :: Binary a => Binary.Get (Update a)
getUpdate = do
  tag <- Binary.get @Word64
  case tag of
    0 -> pure None
    1 -> Set <$> Binary.get
    2 -> Pick <$> Binary.get
    _ -> fail $ "invalid Update tag: " ++ show tag

stateSerialise :: FilePath -> State -> IO ()
stateSerialise path state = LazyByteString.writeFile path (runPut $ stateBinaryPut state)

data InvalidHeaderException = InvalidHeaderException FilePath
  deriving (Show, Exception)

data UnexpectedVersion = UnexpectedVersion FilePath ByteString
  deriving (Show, Exception)

stateDeserialise :: FilePath -> IO State
stateDeserialise path =
  withFile path ReadMode $ \handle -> do
    input <- LazyByteString.hGetContents handle
    case Attoparsec.parse headerParser input of
      Attoparsec.Fail{} -> throwIO $ InvalidHeaderException path
      Attoparsec.Done rest (version, _comment) ->
        case ByteString.Char8.unpack version of
          "0" -> pure $ runGet stateBinaryGet rest
          _ -> throwIO $ UnexpectedVersion path version
  where
    headerParser :: Attoparsec.Parser (ByteString, ByteString)
    headerParser =
      (,) <$
      Attoparsec.string (fromString "tsk;") <*>
      (Attoparsec.string (fromString "version=") *> Attoparsec.takeWhile (/= fromIntegral (ord ';')) <* Attoparsec.string (fromString ";")) <*>
      Attoparsec.takeWhile (/= fromIntegral (ord '\n')) <* Attoparsec.string (fromString "\n")
